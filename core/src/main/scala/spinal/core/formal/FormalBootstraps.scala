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
import spinal.core.internals.{PhaseContext, PhaseNetlist}
import spinal.core.{Component, GlobalData, SpinalConfig, SpinalReport, BlackBox}
import spinal.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import sys.process._

case class SpinalSymbiYosysBackendConfig[T <: Component](
    rtl: SpinalReport[T],
    workspacePath: String = "./",
    workspaceName: String = null,
    modes: ArrayBuffer[String] = ArrayBuffer[String]()
)

object SpinalSymbiYosysBackend {

  def apply[T <: Component](config: SpinalSymbiYosysBackendConfig[T]) = {

    import config._

    val vconfig = new SymbiYosysBackendConfig()
    vconfig.rtlIncludeDirs ++= rtl.rtlIncludeDirs
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    vconfig.toplevelName = rtl.toplevelName
    vconfig.workspaceName = workspaceName
    vconfig.workspacePath = workspacePath
    vconfig.modes ++= config.modes

    new SymbiYosysBackend(vconfig)
  }
}

/** Run formal verification
  */
abstract class FormalCompiled[T <: Component](val report: SpinalReport[T]) {
  def dut = report.toplevel

  val testNameMap = mutable.HashMap[String, Int]()

  def allocateTestName(name: String): String = {
    testNameMap.synchronized {
      val value = testNameMap.getOrElseUpdate(name, 0)
      testNameMap(name) = value + 1
      if (value == 0) {
        return name
      } else {
        val ret = name + "_" + value
        println(s"[Info] Test '$name' was reallocated as '$ret' to avoid collision")
        return ret
      }
    }
  }

  def doVerify(): Unit = doVerifyApi()
  def doVerify(name: String): Unit = doVerifyApi(name = name)

  def doVerifyApi(name: String = "test"): Unit = {
    GlobalData.set(report.globalData)

    val allocatedName = allocateTestName(name)

    println(f"[Progress] Start ${dut.definitionName} $allocatedName formal verification.")
  }
}

/** Formal verify Workspace
  */
object FormalWorkspace {
  private var uniqueId = 0

  def allocateUniqueId(): Int = {
    this.synchronized {
      uniqueId = uniqueId + 1
      uniqueId
    }
  }

  val workspaceMap = mutable.HashMap[(String, String), Int]()

  def allocateWorkspace(path: String, name: String): String = {
    workspaceMap.synchronized {
      val value = workspaceMap.getOrElseUpdate((path, name), 0)
      workspaceMap((path, name)) = value + 1
      if (value == 0) {
        return name
      } else {
        val ret = name + "_" + value
        println(s"[Info] Workspace '$name' was reallocated as '$ret' to avoid collision")
        return ret
      }
    }
  }
}

class SpinalFormalBackendSel
object SpinalFormalBackendSel {
  val SYMBIYOSYS = new SpinalFormalBackendSel
}

/** SpinalSim configuration
  */
case class SpinalFormalConfig(
    var _workspacePath: String = System.getenv().getOrDefault("SPINALSIM_WORKSPACE", "./simWorkspace"),
    var _workspaceName: String = null,
    var _spinalConfig: SpinalConfig = SpinalConfig(),
    var _additionalRtlPath: ArrayBuffer[String] = ArrayBuffer[String](),
    var _additionalIncludeDir: ArrayBuffer[String] = ArrayBuffer[String](),
    var _modes: ArrayBuffer[String] = ArrayBuffer[String]("bmc"),
    var _backend: SpinalFormalBackendSel = SpinalFormalBackendSel.SYMBIYOSYS
) {

  def withSymbiYosys: this.type = {
    _backend = SpinalFormalBackendSel.SYMBIYOSYS
    this
  }

  def addMode(mode: String): this.type = {
    _modes += mode
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

  def addRtl(that: String): this.type = {
    _additionalRtlPath += that
    this
  }

  def addIncludeDir(that: String): this.type = {
    _additionalIncludeDir += that
    this
  }

  def doVerify[T <: Component](report: SpinalReport[T]): Unit = compile(report).doVerify()
  def doVerify[T <: Component](report: SpinalReport[T], name: String): Unit =
    compile(report).doVerify(name)

  def doVerify[T <: Component](rtl: => T): Unit = compile(rtl).doVerify()
  def doVerify[T <: Component](rtl: => T, name: String): Unit = compile(rtl).doVerify(name)

  def compile[T <: Component](rtl: => T): FormalCompiled[T] = {
    this.copy().compileCloned(rtl)
  }

  def compileCloned[T <: Component](rtl: => T): FormalCompiled[T] = {
    val uniqueId = FormalWorkspace.allocateUniqueId()
    new File(s"tmp").mkdirs()
    new File(s"tmp/job_$uniqueId").mkdirs()
    val config = _spinalConfig
      .copy(targetDirectory = s"tmp/job_$uniqueId")
      .addTransformationPhase(new PhaseNetlist {
        override def impl(pc: PhaseContext): Unit = pc.walkComponents {
          case b: BlackBox if b.isBlackBox && b.isSpinalSimWb => b.clearBlackBox()
          case _                                              =>
        }
      })
    val report = _backend match {
      case SpinalFormalBackendSel.SYMBIYOSYS =>
        // config.generateVerilog(rtl)
        config.generateSystemVerilog(rtl)
    }
    report.blackboxesSourcesPaths ++= _additionalRtlPath
    report.blackboxesIncludeDir ++= _additionalIncludeDir
    compile[T](report)
  }

  def compile[T <: Component](report: SpinalReport[T]): FormalCompiled[T] = {
    if (_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty("user.home") + _workspacePath.drop(1)

    if (_workspaceName == null)
      _workspaceName = s"${report.toplevelName}"

    _workspaceName = FormalWorkspace.allocateWorkspace(_workspacePath, _workspaceName)

    println(
      f"[Progress] Formal verification workspace in ${new File(s"${_workspacePath}/${_workspaceName}").getAbsolutePath}"
    )
    new File(s"${_workspacePath}").mkdirs()
    FileUtils.deleteQuietly(new File(s"${_workspacePath}/${_workspaceName}"))
    new File(s"${_workspacePath}/${_workspaceName}").mkdirs()
    new File(s"${_workspacePath}/${_workspaceName}/rtl").mkdirs()

    val rtlDir = new File(s"${_workspacePath}/${_workspaceName}/rtl")
//    val rtlPath = rtlDir.getAbsolutePath
    report.generatedSourcesPaths.foreach { srcPath =>
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

      val dst = new File(rtlDir.getAbsolutePath + "/" + src.getName)
      FileUtils.copyFileToDirectory(src, rtlDir)
    }

    _backend match {
      case SpinalFormalBackendSel.SYMBIYOSYS =>
        println(f"[Progress] Yosys compilation started")
        val startAt = System.nanoTime()
        val vConfig = SpinalSymbiYosysBackendConfig[T](
          rtl = report,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          workspaceName = "formal",
          modes = _modes
        )
        val backend = SpinalSymbiYosysBackend(vConfig)
        val deltaTime = (System.nanoTime() - startAt) * 1e-6
        println(f"[Progress] Yosys compilation done in $deltaTime%1.3f ms")
        new FormalCompiled(report) {
          // override def newSimRaw(name: String, seed: Int): SimRaw = {
          //   val raw = new SimVerilator(backend, backend.instanciate(name, seed))
          //   raw.userData = backend.config.signals
          //   raw
          // }
        }
    }
  }
}
