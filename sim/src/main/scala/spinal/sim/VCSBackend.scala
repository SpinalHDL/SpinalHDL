package spinal.sim

import org.apache.commons.io.FileUtils
import spinal.sim.vpi.SharedMemIface

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.sys.process.{Process, ProcessLogger}

import scala.collection.mutable
case class VCSFlags(
                     vloganFlags    : mutable.ArrayBuffer[String],
                     vhdlFlags    : mutable.ArrayBuffer[String],
                     elaborateFlags    : mutable.ArrayBuffer[String],
                     runFlags    : mutable.ArrayBuffer[String]
                   ) {
  private def addFlags(container: mutable.ArrayBuffer[String], flags: String*): this.type = {
    flags.foreach {f=>
      container ++= f.split(" ")
    }
    this
  }

  def withDefaultVloganFlags(): this.type = {
    vloganFlags ++= List(
      "-nc",
      "-full64",
      "-sverilog",
      "+v2k",
      "-ntb",
      "-debug_access+all",
      "-l vlogan.log"
    )
    this
  }
  def withVloganFlags(flags: String*): this.type = addFlags(vloganFlags, flags:_*)

  def getVloganFlags: String = vloganFlags.mkString(" ")

  def withDefaultVhdlanFlags(): this.type = {
    vhdlFlags ++= List(
      "-nc",
      "-full64",
      "-debug_access+all",
      "-l vhdlan.log"
    )
    this
  }
  def withVhdlFlags(flags: String*): this.type = addFlags(vhdlFlags, flags:_*)
  def getVhdlFlags : String = vhdlFlags.mkString(" ")

  def withAnalysisFlags(flags: String*): this.type = {
    withVloganFlags(flags:_*)
    withVhdlFlags(flags:_*)
  }

  def withDefaultElabFlags(): this.type = {
    elaborateFlags ++= List(
      "-full64",
      "-quiet",
      "-debug_access+all",
      "-debug_acc+pp+dmptf",
      "-debug_region=+cell+encrypt",
      "-l vcs.log",
      "+vpi",
      "+vcs+initreg+random"
    )
    this
  }
  def withElabFlags(flags: String*): this.type = addFlags(elaborateFlags, flags:_*)
  def getElabFlags: String = elaborateFlags.mkString(" ")

  def withDefaultRunFlags(): this.type = {
    this
  }

  def withRunFlags(flags: String*): this.type = addFlags(runFlags, flags:_*)
  def getRunFlags: String = runFlags.mkString(" ")

  def withDefaultFlags(): this.type = {
    withDefaultVloganFlags()
    withDefaultVhdlanFlags()
    withDefaultElabFlags()
    withDefaultRunFlags()
    this
  }
}

object VCSFlags {
  def default: VCSFlags = VCSFlags(
    mutable.ArrayBuffer[String](),
    mutable.ArrayBuffer[String](),
    mutable.ArrayBuffer[String](),
    mutable.ArrayBuffer[String]()
  ).withDefaultFlags()

  /**
    * Back compatibility for old API
    * @param compileFlags - analysis flags used for both vlogan and vhdlan
    * @param elaborateFlags - vcs elaboration flags
    * @param runFlags - simv run flags
    * @return
    */
  def apply(
             compileFlags: List[String] = List[String](),
             elaborateFlags: List[String] = List[String](),
             runFlags: List[String] = List[String]()
           ) : VCSFlags = {
    VCSFlags.default.withAnalysisFlags(compileFlags:_*).withElabFlags(elaborateFlags:_*).withRunFlags(runFlags:_*)
  }
}

class VCSBackendConfig extends VpiBackendConfig {
  var flags: VCSFlags = null
  var vcsCC: Option[String] = None
  var vcsLd: Option[String] = None
  var waveDepth = 0
  var simSetupFile: String = _
  var envSetup: ()=> Unit = _
}

class VCSBackend(config: VCSBackendConfig) extends VpiBackend(config) {
  override def isBufferedWrite: Boolean = true

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.VPD, WaveFormat.DEFAULT, WaveFormat.FSDB, WaveFormat.NONE)

  val format = if (availableFormats contains config.waveFormat) {
    config.waveFormat
  } else {
    println("Wave format " + config.waveFormat + " not supported by VCS for now")
    WaveFormat.NONE
  }

  val vpiModuleName = "vpi_vcs.so"
  val vpiModulePath = pluginsPath + "/" + vpiModuleName
  val vpiModuleAbsPath = new File(vpiModulePath).getAbsolutePath.mkString
  val vpiInclude = sys.env.get("VCS_HOME") match {
    case Some(x) => x + "/include"
    case None => {
//      println("VCS_HOME not found")
      assert(assertion = false, "VCS_HOME not found! Setup your synopsys environment first.")
      ""
    }
  }

  val verdi_home = sys.env.get("VERDI_HOME") match {
    case Some(x) => x
    case None => {
      if (config.waveFormat == WaveFormat.FSDB) {
        var info = "$VERDI_HOME not found"
        if (!sys.env.contains("LD_LIBRARY_PATH")) {
          info += "\n $LD_LIBRARY_PATH not found"
        }
        println(s"[Error]: ${info}")
        " "
      }
    }
  }

  val timeScale = config.timePrecision match {
    case null => "1ns/1ns"
    case t => "1ns/" + t.replace(" ", "")
  }


  def compileVPI(): Unit = {
    if (Files.exists(Paths.get(vpiModulePath))) {
      return
    }
    val vpiPluginSource = Array("/VpiPlugin.cpp", "/SharedStruct.hpp")
    for (filename <- vpiPluginSource) {
      val cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
      val stream = getClass.getResourceAsStream(filename)
      cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
      cppSourceFile.close()
    }
    val vpiCFlags = s"-DVCS_PLUGIN -I$vpiInclude -fPIC -shared"
    val vpiCommand = Seq(
      CC,
      CFLAGS,
      vpiCFlags,
      "VpiPlugin.cpp",
      "-o",
      vpiModuleName
    ).mkString(" ")
    println(s"[VPI] $vpiCommand")
    doCmd(
      vpiCommand,
      new File(pluginsPath),
      s"Compilation of $vpiModuleName failed"
    )
  }

  private def vloganFlags(): String = config.flags.getVloganFlags

  private def vhdlanFlags(): String = config.flags.getVhdlFlags

  private def vcsFlags(): String = {
    val vpiFlags = List(
      "-load", s"$vpiModuleAbsPath:entry_point_cb"
    )
    val topFlags = List(
      "-o", toplevelName
    )
    val commonFlags = config.flags.elaborateFlags.toList ++ vpiFlags ++ topFlags :+ s"-timescale=$timeScale"
    val cc = config.vcsCC match {
      case Some(x) => List("-cc", x)
      case None    => List.empty
    }
    val ld = config.vcsLd match {
      case Some(x) => List("-ld", x)
      case None    => List.empty
    }

    val dump = waveFormat match {
      case WaveFormat.VCD => List(s"+vcs+dumpvars+$toplevelName.vcd")
      case WaveFormat.VPD => List(s"+vcs+vcdpluson")
      case WaveFormat.FSDB =>
        List(s"-P ${verdi_home}/share/PLI/VCS/LINUX64/novas.tab", s"$verdi_home/share/PLI/VCS/LINUX64/pli.a")
      case _ => List.empty
    }
    val cmd = (commonFlags ++ cc ++ ld ++ dump).mkString(" ")
    cmd
  }

  def setupEnvironment(): Unit = {
    if (config.simSetupFile != null) {
      val simSetupFile = new File(config.simSetupFile)
      FileUtils.copyFileToDirectory(simSetupFile, new File(workspacePath))
    }
    if (config.envSetup != null) {
      config.envSetup()
    }
  }

  def analyzeRTL(): Unit = {
    val verilogSourcePaths = rtlSourcesPaths.filter { s =>
      s.endsWith(".v") || s.endsWith(".sv") || s.endsWith(".vl") || s.endsWith(".vh") || s.endsWith(".svh") || s
        .endsWith(".vr")
    }
    val vhdlSourcePaths = rtlSourcesPaths.filter { s =>
      s.endsWith(".vhd") || s.endsWith(".vhdl")
    }

    config.rtlSourcesPaths
      .filter { s =>
        s.endsWith(".bin") || s.endsWith(".mem")
      }
      .foreach { path =>
        FileUtils.copyFileToDirectory(new File(path), new File(workspacePath))
      }

    val fileList = (verilogSourcePaths ++ vhdlSourcePaths).mkString("\n")
    val fileListFile = new PrintWriter(
      new File(s"$workspacePath/filelist.f")
    )
    fileListFile.write(fileList)
    fileListFile.close()

    val simWaveSource =
      s"""
         |`timescale $timeScale
         |module __simulation_def;
         |initial begin
         |  $$fsdbDumpfile("$toplevelName.fsdb");
         |  $$fsdbDumpvars(${config.waveDepth}, $toplevelName);
         |  $$fsdbDumpflush;
         |end
         |endmodule""".stripMargin
    val simulationDefSourceFile = new PrintWriter(
      new File(
        s"$workspacePath/rtl/" ++
          "__simulation_def.v"
      )
    )
    simulationDefSourceFile.write(simWaveSource)
    simulationDefSourceFile.close()

    val fsdbv = waveFormat match {
      case WaveFormat.FSDB => " ./rtl/__simulation_def.v"
      case _               => " "
    }
    val topUnits = waveFormat match {
      case WaveFormat.FSDB => toplevelName + " __simulation_def"
      case _               => toplevelName
    }

    def analyzeSource(): Unit = {
      if (vhdlSourcePaths.nonEmpty) {
        val vhdlanCommand = Seq(
          "vhdlan",
          vhdlanFlags(),
          vhdlSourcePaths.mkString(" ")
        ).mkString(" ")
        println(s"[VHDLAN] $vhdlanCommand")
        doCmd(
          vhdlanCommand,
          new File(workspacePath),
          s"Analysis of the $toplevelName failed"
        )
      }
      if (verilogSourcePaths.nonEmpty) {
        val vloganCommand = Seq(
          "vlogan",
          vloganFlags(),
          verilogSourcePaths.mkString(" ") + fsdbv
        ).mkString(" ")
        println(s"[VLOGAN] $vloganCommand")
        doCmd(
          vloganCommand,
          new File(workspacePath),
          s"Analysis of the $toplevelName failed"
        )
      }
    }

    def elaborate(): Unit = {
      val vcsCommand = Seq(
        "vcs",
        vcsFlags(),
        topUnits
      ).mkString(" ")
      println(s"[VCS] $vcsCommand")
      doCmd(
        vcsCommand,
        new File(workspacePath),
        s"Elaboration of $toplevelName failed"
      )
    }

    println(f"[Progress] VCS compilation started.")
    setupEnvironment()
    println(f"[Progress] VCS user environment setup.")
    val startAt = System.nanoTime()
    analyzeSource()
    elaborate()
    val deltaTime = (System.nanoTime() - startAt) * 1e-9
    println(f"[Progress] VCS compilation done in $deltaTime%1.3f s")

  }

  class LoggerPrintWithTerminationFilter extends ProcessLogger {
    var terminationOfSimulation: Boolean = false
    private def printWithFilter(s: => String): Unit = {
      if (s.contains("Unexpected termination of the simulation")) {
        terminationOfSimulation = true
      }
      println(s)
    }
    override def err(s: => String): Unit = { printWithFilter(s) }
    override def out(s: => String): Unit = { printWithFilter(s) }
    override def buffer[T](f: => T) = f
  }

  def runSimulation(sharedMemIface: SharedMemIface): Thread = {
    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      val logger = new LoggerPrintWithTerminationFilter()
      def run(): Unit = {
        val runCommand = Seq(s"./$toplevelName", config.flags.getRunFlags).mkString(" ")
        println(s"[SIMV] $runCommand")
        val retCode = Process(runCommand, new File(workspacePath)).!(logger)
        if (retCode != 0 && !logger.terminationOfSimulation) {
          iface.set_crashed(retCode)
          println(s"Simulation of $toplevelName failed")
        }
      }
    })

    thread.setDaemon(true)
    thread.start()
    thread
  }
}
