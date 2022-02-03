//package spinal.sim
//
//import spinal.sim.vpi.SharedMemIface
//
//import java.io.{File, PrintWriter}
//import java.nio.file.{Files, Paths}
//import scala.sys.process.Process
//
//class GhdlBackendConfig extends VpiBackendConfig {
//  var ghdlPath: String = null
//  var elaborationFlags: String = ""
//}
//
//class GhdlBackend(config: GhdlBackendConfig) extends VpiBackend(config) {
//  import config._
//  import Backend._
//  val elaborationFlags = config.elaborationFlags
//
//  val availableFormats = Array(WaveFormat.VCD, WaveFormat.VCDGZ,
//    WaveFormat.FST, WaveFormat.GHW,
//    WaveFormat.DEFAULT, WaveFormat.NONE)
//
//  val format = if(availableFormats contains waveFormat){
//    waveFormat
//  } else {
//    println("Wave format " + waveFormat + " not supported by GHDL")
//    WaveFormat.NONE
//  }
//
//  if(!(Array(WaveFormat.DEFAULT,
//    WaveFormat.NONE) contains format)) {
//
//    if(format == WaveFormat.GHW){
//      runFlags += " --wave=" + wavePath
//    } else {
//      runFlags += " --" + format.ext + "=" + wavePath
//    }
//  }
//
//  if(ghdlPath == null) ghdlPath = "ghdl"
//  var vpiModuleName = "vpi_ghdl.vpi"
//
//  def compileVPI() = {
//    val vpiModulePath = pluginsPath + "/" + vpiModuleName
//    if(!(Files.exists(Paths.get(vpiModulePath)) && useCache)) {
//
//      for(filename <- Array("/VpiPlugin.cpp",
//        "/SharedStruct.hpp")) {
//        var cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
//        var stream = getClass.getResourceAsStream(filename)
//        cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
//        cppSourceFile.close
//      }
//
//      assert(Process(Seq(ghdlPath,
//        "--vpi-compile",
//        CC,
//        "-c",
//        CFLAGS,
//        "VpiPlugin.cpp",
//        "-o",
//        "VpiPlugin.o").mkString(" "),
//        new File(pluginsPath)).! (new Logger()) == 0,
//        "Compilation of VpiPlugin.o failed")
//
//      assert(Process(Seq(ghdlPath,
//        "--vpi-link",
//        CC,
//        CFLAGS,
//        "VpiPlugin.o",
//        LDFLAGS,
//        "-o",
//        vpiModuleName).mkString(" "),
//        new File(pluginsPath)).! (new Logger()) == 0,
//        s"Compilation of $vpiModuleName failed")
//    }
//  }
//
//  def analyzeRTL() {
//
//    val vhdlSourcePaths = rtlSourcesPaths.filter { s => (s.endsWith(".vhd") ||
//      s.endsWith(".vhdl")) }
//      .mkString(" ")
//
//    assert(Process(Seq(ghdlPath,
//      "-a",
//      analyzeFlags,
//      vhdlSourcePaths).mkString(" "),
//      new File(workspacePath)).! (new Logger()) == 0,
//      s"Analyze step of vhdl files failed")
//
//    assert(Process(Seq(ghdlPath,
//      "-e",
//      elaborationFlags,
//      toplevelName).mkString(" "),
//      new File(workspacePath)).! (new Logger()) == 0,
//      s"Elaboration of $toplevelName failed")
//  }
//
//  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
//    val vpiModulePath = if(!isWindows) pluginsPath + "/" + vpiModuleName
//    else (pluginsPath + "/" + vpiModuleName).replaceAll("/C",raw"C:").replaceAll(raw"/",raw"\\")
//
//    val pathStr = if(!isWindows) sys.env("PATH")
////    else sys.env("PATH") + ";" + (ghdlPath + " --vpi-library-dir").!!.trim
//    else Process(sys.env("PATH") + ";" + (ghdlPath + " --vpi-library-dir")).!!.trim
//
//    val thread = new Thread(new Runnable {
//      val iface = sharedMemIface
//      def run(): Unit = {
//        val retCode = Process(Seq(ghdlPath,
//          "-r",
//          elaborationFlags,
//          toplevelName,
//          s"--vpi=${pwd + "/" + vpiModulePath}",
//          runFlags).mkString(" "),
//          new File(workspacePath),
//          "PATH" -> pathStr).! (new Logger())
//
//        if (retCode != 0) {
//          iface.set_crashed(retCode)
//          println(s"Simulation of $toplevelName failed")
//        }
//      }
//    }
//    )
//
//    thread.setDaemon(true)
//    thread.start()
//    thread
//  }
//
//  override def isBufferedWrite: Boolean = true
//}
//
//object GhdlBackend {
//
//  def getMCODE(config: GhdlBackendConfig) = {
//    if (config.ghdlPath == null) config.ghdlPath = "ghdl-mcode"
//    val ghdlBackend = new GhdlBackend(config)
//    ghdlBackend.vpiModuleName = "vpi_ghdl_mcode.vpi"
//    ghdlBackend
//  }
//
//  def getGCC(config: GhdlBackendConfig) = {
//    if (config.ghdlPath == null) config.ghdlPath = "ghdl-gcc"
//    val ghdlBackend = new GhdlBackend(config)
//    ghdlBackend.vpiModuleName = "vpi_ghdl_gcc.vpi"
//    ghdlBackend
//  }
//
//  def getLLVM(config: GhdlBackendConfig) = {
//    if (config.ghdlPath == null) config.ghdlPath = "ghdl-llvm"
//    val ghdlBackend = new GhdlBackend(config)
//    ghdlBackend.vpiModuleName = "vpi_ghdl_llvm.vpi"
//    ghdlBackend
//  }
//}
//
