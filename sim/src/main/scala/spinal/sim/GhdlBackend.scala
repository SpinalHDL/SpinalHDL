package spinal.sim

import spinal.sim.vpi.SharedMemIface

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.sys.process._


class GhdlBackendConfig extends VpiBackendConfig {
  var ghdlPath: String = null
  var elaborationFlags: String = ""
}

class GhdlBackend(config: GhdlBackendConfig) extends VpiBackend(config) {
  import Backend._
  var ghdlPath = config.ghdlPath
  val elaborationFlags = config.elaborationFlags

  val availableFormats =
    Array(WaveFormat.VCD, WaveFormat.VCDGZ, WaveFormat.FST, WaveFormat.GHW, WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if (availableFormats contains waveFormat) {
    waveFormat
  } else {
    println("Wave format " + waveFormat + " not supported by GHDL")
    WaveFormat.NONE
  }

  if (!(Array(WaveFormat.DEFAULT, WaveFormat.NONE) contains format)) {
    wavePath = wavePath.split('.').init ++ Seq(format.ext) mkString "."
    if (format == WaveFormat.GHW) {
      runFlags += " --wave=" + wavePath
    } else {
      runFlags += " --" + format.ext + "=" + wavePath
    }
  }

  if (ghdlPath == null) ghdlPath = "ghdl"
  var vpiModuleName = "vpi_ghdl.vpi"

  def compileVPI() = {
    val vpiModulePath = pluginsPath + "/" + vpiModuleName
    if (!(Files.exists(Paths.get(vpiModulePath)) && useCache)) {

      for (filename <- Array("/VpiPlugin.cpp", "/SharedStruct.hpp")) {
        var cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
        var stream = getClass.getResourceAsStream(filename)
        cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
        cppSourceFile.close
      }

      doCmd(
        Seq(ghdlPath, "--vpi-compile", CC, "-c", CFLAGS + " -DGHDL_PLUGIN", "VpiPlugin.cpp", "-o", "VpiPlugin.o")
          .mkString(" "),
        new File(pluginsPath),
        "Compilation of VpiPlugin.o failed"
      )

      doCmd(
        Seq(ghdlPath, "--vpi-link", CC, CFLAGS, "VpiPlugin.o", LDFLAGS, "-o", vpiModuleName).mkString(" "),
        new File(pluginsPath),
        s"Compilation of $vpiModuleName failed"
      )
    }
  }

  def analyzeRTL(): Unit = {

    val vhdlSourcePaths = rtlSourcesPaths
      .filter { s =>
        (s.endsWith(".vhd") ||
        s.endsWith(".vhdl"))
      }
      .mkString(" ")

    doCmd(
      Seq(ghdlPath, "-a", analyzeFlags, "-fsynopsys", vhdlSourcePaths).mkString(" "),
      new File(workspacePath),
      s"Analyze step of vhdl files failed"
    )

    doCmd(
      Seq(ghdlPath, "-e", elaborationFlags, "-fsynopsys", toplevelName).mkString(" "),
      new File(workspacePath),
      s"Elaboration of $toplevelName failed"
    )
  }

  def runSimulation(sharedMemIface: SharedMemIface): Thread = {
    val vpiModulePath =
      if (!isWindows) pluginsPath + "/" + vpiModuleName
      else (pluginsPath + "/" + vpiModuleName).replaceAll("/C", raw"C:").replaceAll(raw"/", raw"\\")

    val pathStr = if (!isWindows) {
      sys.env("PATH")
    } else {
      val pathKey = sys.env.keysIterator.find(s => s.equalsIgnoreCase("PATH")).get
      sys.env(pathKey) + ";" + (ghdlPath + " --vpi-library-dir").!!.trim
    }

    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      def run(): Unit = {
        val retCode = Process(
          Seq(
            ghdlPath,
            "-r",
            elaborationFlags,
            "-fsynopsys",
            toplevelName,
            s"--vpi=${pwd + "/" + vpiModulePath}",
            runFlags
          ).mkString(" "),
          new File(workspacePath),
          "PATH" -> pathStr
        ).!(new LoggerPrint())

        if (retCode != 0) {
          iface.set_crashed(retCode)
          println(s"Simulation of $toplevelName failed")
        }
      }
    })

    thread.setDaemon(true)
    thread.start()
    thread
  }

  override def isBufferedWrite: Boolean = true
}

object GhdlBackend {

  def getMCODE(config: GhdlBackendConfig) = {
    if (config.ghdlPath == null) config.ghdlPath = "ghdl-mcode"
    val ghdlBackend = new GhdlBackend(config)
    ghdlBackend.vpiModuleName = "vpi_ghdl_mcode.vpi"
    ghdlBackend
  }

  def getGCC(config: GhdlBackendConfig) = {
    if (config.ghdlPath == null) config.ghdlPath = "ghdl-gcc"
    val ghdlBackend = new GhdlBackend(config)
    ghdlBackend.vpiModuleName = "vpi_ghdl_gcc.vpi"
    ghdlBackend
  }

  def getLLVM(config: GhdlBackendConfig) = {
    if (config.ghdlPath == null) config.ghdlPath = "ghdl-llvm"
    val ghdlBackend = new GhdlBackend(config)
    ghdlBackend.vpiModuleName = "vpi_ghdl_llvm.vpi"
    ghdlBackend
  }
}
