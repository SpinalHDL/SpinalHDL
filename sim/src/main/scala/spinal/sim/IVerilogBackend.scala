//package spinal.sim
//
//import spinal.sim.vpi.SharedMemIface
//
//import java.io.{File, PrintWriter}
//import java.nio.file.{Files, Paths}
//import scala.sys.process.Process
//
///* README first!
//
//  This backend doesn't work because of incompatibilities between IVerilog and SpinalSim execution method
//
//*/
//class IVerilogBackendConfig extends VpiBackendConfig {
//  var binDirectory: String = ""
//}
//
//class IVerilogBackend(config: IVerilogBackendConfig) extends VpiBackend(config) {
//  import Backend._
//
//  val availableFormats = Array(WaveFormat.VCD,
//    WaveFormat.FST, WaveFormat.FST_SPEED, WaveFormat.FST_SPACE,
//    WaveFormat.LXT, WaveFormat.LXT_SPEED, WaveFormat.LXT_SPACE,
//    WaveFormat.LXT2, WaveFormat.LXT2_SPEED, WaveFormat.LXT2_SPACE,
//    WaveFormat.DEFAULT, WaveFormat.NONE)
//
//  val format = if(availableFormats contains config.waveFormat){
//    config.waveFormat
//  } else {
//    println("Wave format " + config.waveFormat + " not supported by IVerilog")
//    WaveFormat.NONE
//  }
//
//  var hasWave = false
//  if(!(Array(WaveFormat.DEFAULT,
//    WaveFormat.NONE) contains format)) {
//
//    runFlags += " -" +  format.ext
//    hasWave = true
//  }
//
//  val vpiModuleName = "vpi_iverilog.vpi"
//  val vpiModulePath = pluginsPath + "/" + vpiModuleName
//  val iverilogPath    = config.binDirectory + "iverilog"
//  val iverilogVpiPath = config.binDirectory + "iverilog-vpi"
//  val vvpPath         = config.binDirectory + "vvp"
//
//  val IVERILOGCFLAGS = "-Wstrict-prototypes".r
//    .replaceAllIn(Process(Seq(iverilogVpiPath,
//      "--cflags")).!!,
//      "")
//  val IVERILOGLDFLAGS = Process(Seq(iverilogVpiPath, "--ldflags")).!!
//  val IVERILOGLDLIBS = Process(Seq(iverilogVpiPath, "--ldlibs")).!!
//
//  def compileVPI() = {
//    val vpiModulePath = pluginsPath + "/" + vpiModuleName
//    if(!Files.exists(Paths.get(vpiModulePath))) {
//
//      for(filename <- Array("/VpiPlugin.cpp",
//        "/SharedStruct.hpp")) {
//        var cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
//        var stream = getClass.getResourceAsStream(filename)
//        cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
//        cppSourceFile.close
//      }
//
//      assert(Process(Seq(CC,
//        "-c",
//        IVERILOGCFLAGS,
//        CFLAGS + " -DIVERILOG_PLUGIN",
//        "VpiPlugin.cpp",
//        "-o",
//        "VpiPlugin.o").mkString(" "),
//        new File(pluginsPath)).! (new Logger()) == 0,
//        "Compilation of VpiPlugin.o failed")
//
//      assert(Process(Seq(CC,
//        IVERILOGCFLAGS,
//        CFLAGS,
//        "VpiPlugin.o",
//        IVERILOGLDFLAGS,
//        IVERILOGLDLIBS,
//        LDFLAGS,
//        "-o",
//        vpiModuleName).mkString(" "),
//        new File(pluginsPath)).! (new Logger()) == 0,
//        s"Compilation of $vpiModuleName failed")
//    }
//  }
//
//  def analyzeRTL() {
//    val verilogSourcePaths = rtlSourcesPaths.filter { s => (s.endsWith(".v") ||
//      s.endsWith(".sv") ||
//      s.endsWith(".vl")) }
//      .mkString(" ")
//
//    val simulationDefSource = s"""
//                                 |`timescale 1ns/1ns
//                                 |
//                                 |module __simulation_def;
//                                 |initial
//                                 | begin
//                                 |  ${ if(hasWave) "$dumpfile(\"" + wavePath + "\");" else ""}
//                                 |  ${ if(hasWave) "$dumpvars(0," + toplevelName + ");" else ""}
//                                 |  $$readmempath("./rtl/");
//                                 | end
//                                 |endmodule""".stripMargin
//
//    val simulationDefSourceFile = new PrintWriter(new File(s"${workspacePath}/rtl/" ++
//      "__simulation_def.v"))
//    simulationDefSourceFile.write(simulationDefSource)
//    simulationDefSourceFile.close
//
//    assert(Process(Seq(iverilogPath,
//      analyzeFlags,
//      "-o",
//      toplevelName + ".vvp",
//      "-s __simulation_def",
//      "-s",
//      toplevelName,
//      verilogSourcePaths,
//      s"./rtl/__simulation_def.v").mkString(" "),
//      new File(workspacePath)).! (new Logger()) == 0,
//      s"Analyze step of verilog files failed")
//  }
//
//  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
//    val vpiModulePath = if(!isWindows) pluginsPath + "/" + vpiModuleName
//    else (pluginsPath + "/" + vpiModuleName).replaceAll("/C",raw"C:").replaceAll(raw"/",raw"\\")
//
//    val pathStr = if(!isWindows) sys.env("PATH")
//
//    val thread = new Thread(new Runnable {
//      val iface = sharedMemIface
//      def run(): Unit = {
//        val retCode = Process(Seq(vvpPath,
//          "-M.",
//          s"-m${pwd + "/" +vpiModulePath}",
//          toplevelName + ".vvp",
//          runFlags).mkString(" "),
//          new File(workspacePath)).! (new Logger())
//        if (retCode != 0) {
//          iface.set_crashed(retCode)
//          println(s"Simulation of $toplevelName failed")
//        }
//      }
//    })
//
//    //    thread.setUncaughtExceptionHandler(new VpiThreadExceptionHandler)
//    thread.setDaemon(true)
//    thread.start()
//    thread
//  }
//
//  override def isBufferedWrite: Boolean = true
//}
