/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.formal

import java.io.{File, PrintWriter}
import org.apache.commons.io.FileUtils
import spinal.core.internals.{PhaseContext, PhaseNetlist, DataAssignmentStatement, Operator}
import spinal.core.sim.SimWorkspace
import spinal.core.{BlackBox, Component, GlobalData, SpinalConfig, SpinalReport, SpinalWarning, ClockDomain, ASYNC, SYNC}
import spinal.core.{Reg, Bool, True, False}
import spinal.sim._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, LinkedHashMap}
import scala.io.Source
import scala.util.Random
import sys.process._

sealed trait FormalBackend
case object SymbiYosysFormalBackend extends FormalBackend
case object GhdlFormalBackend extends FormalBackend

trait FormalEngin {}
trait SpinalFormalBackend {
  def doVerify(name: String = "formal"): Unit
}

/** Formal verify Workspace
  */
object FormalWorkspace {
  def allocateUniqueId(): Int = SimWorkspace.allocateUniqueId()
  def allocateWorkspace(path: String, name: String): String = SimWorkspace.allocateWorkspace(path, name)
}

class FormalPhase(backend: FormalBackend) extends PhaseNetlist {
  override def impl(pc: PhaseContext): Unit = {

    // force synchronous reset for DUTs marked with FormalDut()
    pc.walkComponents { c =>
      if (c.isFormalTester) {
        val cds = mutable.LinkedHashMap[ClockDomain, ClockDomain]()
        c.dslBody.walkStatements { s =>
          s.remapClockDomain { cd =>
            if (cd.config.resetKind == ASYNC) {
              cds.getOrElseUpdate(cd, cd.withSyncReset())
            } else {
              cd
            }
          }
        }
      }
    }

    pc.walkComponents {
      case b: BlackBox if b.isBlackBox && b.isSpinalSimWb => b.clearBlackBox()
      case _                                              =>
    }

    if (backend == GhdlFormalBackend) {
      // convert formal random assignments to "unconstrained variables" via attributes
      pc.walkComponents { c =>
        c.dslBody.walkStatements {
          case s: DataAssignmentStatement if s.source.isInstanceOf[Operator.Formal.RandomExp] =>
            val kind = s.source.asInstanceOf[Operator.Formal.RandomExp].kind match {
              case Operator.Formal.RANDOM_ANY_SEQ   => "anyseq"
              case Operator.Formal.RANDOM_ANY_CONST => "anyconst"
              case Operator.Formal.RANDOM_ALL_SEQ   => "allseq"
              case Operator.Formal.RANDOM_ALL_CONST => "allconst"
            }
            s.dlcParent.addAttribute(kind)
          case _ =>
        }
      }

      // rework assignments of initstate() to manually crafted signals in each clock domain
      val initAssignments = LinkedHashMap[(Component, ClockDomain), ArrayBuffer[DataAssignmentStatement]]()
      pc.walkComponents { c =>
        c.dslBody.walkStatements {
          case i: DataAssignmentStatement if i.source.isInstanceOf[Operator.Formal.InitState] =>
            val assignments = initAssignments.getOrElseUpdate((c, i.dlcParent.clockDomain), new ArrayBuffer[DataAssignmentStatement]())
            assignments += i
          case _ =>
        }
      }
      for ((key, assignments) <- initAssignments) {
        val component = key._1
        val clockDomain = key._2
        component.rework {
          clockDomain.withBootReset() {
            val initstate = Reg(Bool()) init(True)
            initstate := False
            initstate.setName(clockDomain.clock.getName() + "_initstate")
            for (assignment <- assignments) {
              assignment.dlcParent := initstate
              assignment.removeStatement()
            }
          }
        }
      }
    }
  }
}

/** SpinalSim configuration
  */
case class SpinalFormalConfig(
    var _workspacePath: String = System.getenv().getOrDefault("SPINALSIM_WORKSPACE", "./simWorkspace"),
    var _workspaceName: String = null,
    var _spinalConfig: SpinalConfig = SpinalConfig().includeFormal,
    var _additionalRtlPath: ArrayBuffer[String] = ArrayBuffer[String](),
    var _additionalIncludeDir: ArrayBuffer[String] = ArrayBuffer[String](),
    var _modesWithDepths: LinkedHashMap[String, Int] = LinkedHashMap[String, Int](),
    var _backend: FormalBackend = SymbiYosysFormalBackend,
    var _keepDebugInfo: Boolean = false,
    var _skipWireReduce: Boolean = false,
    var _hasAsync: Boolean = false,
    var _timeout: Option[Int] = None,
    var _engines: ArrayBuffer[FormalEngin] = ArrayBuffer()
) {
  def withSymbiYosys: this.type = {
    _backend = SymbiYosysFormalBackend
    this
  }

  def withGhdl: this.type = {
    SpinalWarning("Using GHDL as formal backend is experimental!")
    _backend = GhdlFormalBackend
    this
  }

  def withBackend(backend: FormalBackend): this.type = {
    if (backend == GhdlFormalBackend) SpinalWarning("Using GHDL as formal backend is experimental!")
    _backend = backend
    this
  }

  def withBMC(depth: Int = 100): this.type = {
    _modesWithDepths("bmc") = depth
    this
  }

  def withProve(depth: Int = 100): this.type = {
    _modesWithDepths("prove") = depth
    this
  }

  def withCover(depth: Int = 100): this.type = {
    _modesWithDepths("cover") = depth
    this
  }

  def withTimeout(timeout: Int): this.type = {
    _timeout = Some(timeout)
    this
  }

  def withEngies(engins: Seq[FormalEngin]): this.type = {
    _engines.clear()
    _engines ++= engins
    this
  }

  def addEngin(engin: FormalEngin): this.type = {
    _engines ++= Seq(engin)
    this
  }

  def withAsync: this.type = {
    _hasAsync = true
    this
  }

  def withSyncOnly: this.type = {
    _hasAsync = false
    this
  }

  def workspacePath(path: String): this.type = {
    _workspacePath = path
    this
  }

  def workspaceName(name: String): this.type = {
    _workspaceName = name
    this
  }

  def withConfig(config: SpinalConfig): this.type = {
    _spinalConfig = config
    this
  }

  def withSyncResetDefault: this.type = {
    val syncConfig = _spinalConfig.defaultConfigForClockDomains.copy(resetKind = SYNC)
    _spinalConfig = _spinalConfig.copy(defaultConfigForClockDomains = syncConfig)
    this
  }

  def withDebug: this.type = {
    _keepDebugInfo = true
    this
  }

  def withOutWireReduce: this.type = {
    _skipWireReduce = true
    this
  }

  def addRtl(that: String): this.type = {
    _additionalRtlPath += that
    this
  }

  def addIncludeDir(that: String): this.type = {
    _additionalIncludeDir += that
    this
  }

  def doVerify[T <: Component](report: SpinalReport[T])(implicit className: String): Unit = {
    if(!className.isEmpty) workspaceName(className)
    compile(report).doVerify()
  }
  def doVerify[T <: Component](report: SpinalReport[T], name: String)(implicit className: String): Unit = {
    if(!className.isEmpty) workspaceName(className)
    compile(report).doVerify(name)
  }

  def doVerify[T <: Component](rtl: => T)(implicit className: String): Unit = {
    if(!className.isEmpty) workspaceName(className)
    compile(rtl).doVerify()
  }
  def doVerify[T <: Component](rtl: => T, name: String)(implicit className: String): Unit = {
    if(!className.isEmpty) workspaceName(className)
    compile(rtl).doVerify(name)
  }

  def compile[T <: Component](rtl: => T): SpinalFormalBackend = {
    this.copy().compileCloned(rtl)
  }

  def compileCloned[T <: Component](rtl: => T): SpinalFormalBackend = {
    if (_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty("user.home") + _workspacePath.drop(1)

    val uniqueId = FormalWorkspace.allocateUniqueId()
    new File(s"${_workspacePath}/tmp").mkdirs()
    new File(s"${_workspacePath}/tmp/job_$uniqueId").mkdirs()

    val config = _spinalConfig
      .copy(targetDirectory = s"${_workspacePath}/tmp/job_$uniqueId")
      .addTransformationPhase(new FormalPhase(_backend))

    val report = _backend match {
      case SymbiYosysFormalBackend =>
        // config.generateVerilog(rtl)
        config.generateSystemVerilog(rtl)
      case GhdlFormalBackend =>
        config.generateVhdl(rtl)
    }
    report.blackboxesSourcesPaths ++= _additionalRtlPath
    report.blackboxesIncludeDir ++= _additionalIncludeDir
    compile[T](report)
  }

  def compile[T <: Component](report: SpinalReport[T]): SpinalFormalBackend = {
    if (_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty("user.home") + _workspacePath.drop(1)

    if (_workspaceName == null)
      _workspaceName = s"${report.toplevelName}"

    _workspaceName = FormalWorkspace.allocateWorkspace(_workspacePath, _workspaceName)

    println(
      f"[Progress] Formal verification workspace in ${new File(s"${_workspacePath}/${_workspaceName}").getAbsolutePath}"
    )
    val rootWorkplace = new File(_workspacePath).getAbsoluteFile.toPath()
    val workingWorksplace = rootWorkplace.resolve(_workspaceName)
    val rtlDir = workingWorksplace.resolve("rtl")

    rootWorkplace.toFile.mkdirs()
    FileUtils.deleteQuietly(workingWorksplace.toFile())
    workingWorksplace.toFile.mkdirs()
    rtlDir.toFile.mkdirs()

    val rtlFiles = new ArrayBuffer[String]()
//    val rtlPath = rtlDir.getAbsolutePath
    val rtlSources = (report.generatedSourcesPaths ++ report.blackboxesSourcesPaths)
    rtlSources.foreach { srcPath =>
      val src = new File(srcPath)
      val lines = Source.fromFile(src).getLines.toArray
      val w = new PrintWriter(src)
      for (line <- lines) {
        val str = if (line.contains("readmem")) {
          val exprPattern = """.*\$readmem.*\(\"(.+)\".+\).*""".r
          val absline = line match {
            case exprPattern(relpath) => {
              val windowsfix = relpath.replace(".\\", "")
              val abspath = new File(src.getParent + "/" + windowsfix).getAbsolutePath
              val ret = line.replace(relpath, abspath)
              ret.replace("\\", "\\\\") // windows escape "\"
            }
            case _ => new Exception("readmem abspath replace failed")
          }
          absline
        } else {
          line
        }
        w.println(str)
      }
      w.close()

      val dst = rtlDir.resolve(src.getName)
      FileUtils.copyFileToDirectory(src, rtlDir.toFile())
      rtlFiles.append(workingWorksplace.relativize(dst).toString)
    }

    _backend match {
      case SymbiYosysFormalBackend | GhdlFormalBackend =>
        println(f"[Progress] Yosys compilation started")
        val startAt = System.nanoTime()
        val vConfig = new SymbiYosysFormalBackendConfig(
          workspacePath = workingWorksplace.toString(),
          workspaceName = "formal",
          toplevelName = report.toplevelName,
          modesWithDepths = _modesWithDepths,
          timeout = _timeout,
          keepDebugInfo = _keepDebugInfo,
          skipWireReduce = _skipWireReduce,
          multiClock = _hasAsync,
          withGhdl = _backend == GhdlFormalBackend
        )
        vConfig.rtlSourcesPaths ++= rtlFiles
        vConfig.rtlIncludeDirs ++= report.rtlIncludeDirs
        if (_engines.nonEmpty)
          vConfig.engines = _engines.map(x =>
            x match {
              case e: SbyEngine => e
            }
          )

        val backend = new SymbiYosysFormalBackendImpl(vConfig)
        val deltaTime = (System.nanoTime() - startAt) * 1e-6
        println(f"[Progress] Yosys compilation done in $deltaTime%1.3f ms")
        backend
    }
  }
}
