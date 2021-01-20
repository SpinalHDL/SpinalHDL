package spinal.sim

import java.nio.file.{Paths, Files}
import java.io.{File, PrintWriter}
import java.lang.Thread
import scala.io.Source
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process._

import spinal.sim.vpi._

case class VpiBackendConfig(
  val rtlSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
  var toplevelName: String   = null,
  var pluginsPath: String    = "simulation_plugins",
  var workspacePath: String  = null,
  var workspaceName: String  = null,
  var wavePath: String       = null,
  var waveFormat: WaveFormat = WaveFormat.NONE,
  var analyzeFlags: String   = "",
  var runFlags: String       = "",
  var sharedMemSize: Int     = 65536,
  var CC: String             = "g++",
  var CFLAGS: String         = "-std=c++11 -Wall -Wextra -pedantic -O2 -Wno-strict-aliasing", 
  var LDFLAGS: String        = "-lpthread ", 
  var useCache: Boolean      = false,
  var logSimProcess: Boolean = false
)

abstract class VpiBackend(val config: VpiBackendConfig) extends Backend {
  import Backend._
  val rtlSourcesPaths = config.rtlSourcesPaths 
  val toplevelName    = config.toplevelName    
  val pluginsPath     = config.pluginsPath     
  val workspacePath   = config.workspacePath   
  val workspaceName   = config.workspaceName   
  val wavePath        = config.wavePath        
  val waveFormat      = config.waveFormat      
  val analyzeFlags    = config.analyzeFlags
  var runFlags        = config.runFlags        
  val sharedMemSize   = config.sharedMemSize   
  val CC              = config.CC
  var CFLAGS          = config.CFLAGS
  var LDFLAGS         = config.LDFLAGS
  val useCache        = config.useCache              
  val logSimProcess   = config.logSimProcess

  val sharedExtension = if(isWindows) "dll" else (if(isMac) "dylib" else "so")
  val sharedMemIfaceName = "shared_mem_iface." + sharedExtension
  val sharedMemIfacePath = pluginsPath + "/" + sharedMemIfaceName
  var runIface = 0

  CFLAGS += " -fPIC -DNDEBUG -I " + pluginsPath
  LDFLAGS += (if(!isMac) " -shared" else "")
  LDFLAGS += (if(!isWindows && !isMac) " -lrt" else "")

  val jdk = System.getProperty("java.home").replace("/jre","").replace("\\jre","")

  CFLAGS += s" -I$jdk/include -I$jdk/include/${(if(isWindows) "win32" 
  else (if (isMac) "darwin" 
  else "linux"))}"

  class Logger extends ProcessLogger { 
    override def err(s: => String): Unit = { if(logSimProcess) println(s) }
    override def out(s: => String): Unit = { if(logSimProcess) println(s) }
    override def buffer[T](f: => T) = f
  }

  val pwd = new File(".").getAbsolutePath().mkString

  lazy val delayed_compilation: Unit = {
    if(!(Files.exists(Paths.get(sharedMemIfacePath)) && useCache)) {
      List("/SharedMemIface.cpp", 
        "/SharedMemIface.hpp", 
        "/SharedMemIface_wrap.cxx", 
        "/SharedStruct.hpp").foreach { filename =>
          val cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
          val stream = getClass.getResourceAsStream(filename)
          cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString) 
          cppSourceFile.close
        }

        assert(Process(Seq(CC, 
          "-c", 
          CFLAGS, 
          "SharedMemIface.cpp", 
          "-o",
          "SharedMemIface.o").mkString(" "), 
        new File(pluginsPath)).! (new Logger()) == 0, 
      "Compilation of SharedMemIface.cpp failed")

        assert(Process(Seq(CC, 
          "-c", 
          CFLAGS, 
          "SharedMemIface_wrap.cxx", 
          "-o",
          "SharedMemIface_wrap.o").mkString(" "), 
        new File(pluginsPath)).! (new Logger()) == 0, 
      "Compilation of SharedMemIface_wrap.cxx failed")

        assert(Process(Seq(CC, 
          CFLAGS + (if(isMac) " -dynamiclib " else ""),
          "SharedMemIface.o", 
          "SharedMemIface_wrap.o",
          LDFLAGS, 
          "-o",
          sharedMemIfaceName).mkString(" "),

        new File(pluginsPath)).! (new Logger()) == 0, 
      "Compilation of SharedMemIface." + sharedExtension + " failed")
    }

    System.load(pwd + "/" + sharedMemIfacePath)
    compileVPI()
    analyzeRTL() 
  }

  def clean() {
    FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
    FileUtils.cleanDirectory(new File(pluginsPath))
  }

  def compileVPI()  // Return the plugin name
  def analyzeRTL()
  def runSimulation(sharedMemIface: SharedMemIface) : Thread

  def instanciate_() : (SharedMemIface, Thread) = {
    delayed_compilation
    val shmemKey = Seq("SpinalHDL",
      runIface.toString,
      uniqueId.toString,
      hashCode().toString, 
      pwd.hashCode().toString,
      System.currentTimeMillis().toString,
      scala.util.Random.nextLong().toString).mkString("_")

    runIface += 1

    val sharedMemIface = new SharedMemIface(shmemKey, sharedMemSize)
    var shmemFile = new PrintWriter(new File(workspacePath + "/shmem_name"))
    shmemFile.write(shmemKey) 
    shmemFile.close
    val thread = runSimulation(sharedMemIface)
    sharedMemIface.check_ready 
    (sharedMemIface, thread)
  }

  def instanciate(seed : Long) : (SharedMemIface, Thread) = {
    val ret = if(useCache) {
      VpiBackend.synchronized {
        instanciate_
      }
    } else {
      this.synchronized {
        instanciate_
      }
    }
    ret._1.set_seed(seed)
    ret._1.eval
    ret
  }

  def instanciate() : (SharedMemIface, Thread) = instanciate(0x5EED5EED)
}

object VpiBackend {}

class GhdlBackendConfig extends VpiBackendConfig {
  var ghdlPath: String = null
  var elaborationFlags: String = ""
}

class GhdlBackend(config: GhdlBackendConfig) extends VpiBackend(config) {
  import Backend._
  var ghdlPath = config.ghdlPath
  val elaborationFlags = config.elaborationFlags

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.VCDGZ,
    WaveFormat.FST, WaveFormat.GHW, 
    WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if(availableFormats contains waveFormat){
    waveFormat  
  } else {
    println("Wave format " + waveFormat + " not supported by GHDL")
    WaveFormat.NONE
  }

  if(!(Array(WaveFormat.DEFAULT, 
    WaveFormat.NONE) contains format)) {

      if(format == WaveFormat.GHW){
        runFlags += " --wave=" + wavePath
      } else {
        runFlags += " --" + format.ext + "=" + wavePath
      }
    }

    if(ghdlPath == null) ghdlPath = "ghdl"
    var vpiModuleName = "vpi_ghdl.vpi"

  def compileVPI() = {
    val vpiModulePath = pluginsPath + "/" + vpiModuleName
    if(!(Files.exists(Paths.get(vpiModulePath)) && useCache)) {

      for(filename <- Array("/VpiPlugin.cpp", 
           "/SharedStruct.hpp")) {
             var cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
             var stream = getClass.getResourceAsStream(filename)
             cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString) 
             cppSourceFile.close
           }

           assert(Process(Seq(ghdlPath,
             "--vpi-compile",
             CC, 
             "-c", 
             CFLAGS, 
             "VpiPlugin.cpp",
             "-o",
             "VpiPlugin.o").mkString(" "), 
           new File(pluginsPath)).! (new Logger()) == 0, 
         "Compilation of VpiPlugin.o failed")

           assert(Process(Seq(ghdlPath,
             "--vpi-link",
             CC, 
             CFLAGS,
             "VpiPlugin.o",
             LDFLAGS,
             "-o",
             vpiModuleName).mkString(" "), 
           new File(pluginsPath)).! (new Logger()) == 0, 
         s"Compilation of $vpiModuleName failed")
    }
  } 

  def analyzeRTL() {

    val vhdlSourcePaths = rtlSourcesPaths.filter { s => (s.endsWith(".vhd") || 
    s.endsWith(".vhdl")) }
      .mkString(" ")

      assert(Process(Seq(ghdlPath,
        "-a",
        analyzeFlags,
        vhdlSourcePaths).mkString(" "), 
      new File(workspacePath)).! (new Logger()) == 0, 
    s"Analyze step of vhdl files failed") 

      assert(Process(Seq(ghdlPath,
        "-e",
        elaborationFlags,
        toplevelName).mkString(" "), 
      new File(workspacePath)).! (new Logger()) == 0,
    s"Elaboration of $toplevelName failed")
  }

  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
    val vpiModulePath = if(!isWindows) pluginsPath + "/" + vpiModuleName
    else (pluginsPath + "/" + vpiModuleName).replaceAll("/C",raw"C:").replaceAll(raw"/",raw"\\")

    val pathStr = if(!isWindows) sys.env("PATH")
    else sys.env("PATH") + ";" + (ghdlPath + " --vpi-library-dir").!!.trim

    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      def run(): Unit = {
        val retCode = Process(Seq(ghdlPath,
          "-r",
          elaborationFlags,
          toplevelName,
          s"--vpi=${pwd + "/" + vpiModulePath}",
          runFlags).mkString(" "), 
        new File(workspacePath),
        "PATH" -> pathStr).! (new Logger())

      if (retCode != 0) iface.set_crashed(retCode)
      assert(retCode == 0, s"Simulation of $toplevelName failed")
      }
    }
    )

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


/* README first!

  This backend doesn't work because of incompatibilities between IVerilog and SpinalSim execution method

*/
class IVerilogBackendConfig extends VpiBackendConfig {
  var binDirectory: String = ""
}

class IVerilogBackend(config: IVerilogBackendConfig) extends VpiBackend(config) {
  import Backend._
  import config._

  val availableFormats = Array(WaveFormat.VCD, 
                               WaveFormat.FST, WaveFormat.FST_SPEED, WaveFormat.FST_SPACE, 
                               WaveFormat.LXT, WaveFormat.LXT_SPEED, WaveFormat.LXT_SPACE, 
                               WaveFormat.LXT2, WaveFormat.LXT2_SPEED, WaveFormat.LXT2_SPACE, 
                               WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if(availableFormats contains config.waveFormat){
                config.waveFormat  
              } else {
                println("Wave format " + config.waveFormat + " not supported by IVerilog")
                WaveFormat.NONE
              }

  var hasWave = false
  if(!(Array(WaveFormat.DEFAULT, 
             WaveFormat.NONE) contains format)) {

      runFlags += " -" +  format.ext
      hasWave = true
    }

  val vpiModuleName = "vpi_iverilog.vpi"
  val vpiModulePath = pluginsPath + "/" + vpiModuleName
  val iverilogPath    = binDirectory + "iverilog"
  val iverilogVpiPath = binDirectory + "iverilog-vpi"
  val vvpPath         = binDirectory + "vvp"

  val IVERILOGCFLAGS = "-Wstrict-prototypes".r
                                            .replaceAllIn(Process(Seq(iverilogVpiPath, 
                                                                      "--cflags")).!!,
                                                          "")
  val IVERILOGLDFLAGS = Process(Seq(iverilogVpiPath, "--ldflags")).!!
  val IVERILOGLDLIBS = Process(Seq(iverilogVpiPath, "--ldlibs")).!!

  def compileVPI() = {
    val vpiModulePath = pluginsPath + "/" + vpiModuleName
    if(!Files.exists(Paths.get(vpiModulePath))) {

      for(filename <- Array("/VpiPlugin.cpp", 
                            "/SharedStruct.hpp")) {
             var cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
             var stream = getClass.getResourceAsStream(filename)
             cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString) 
             cppSourceFile.close
           }

           assert(Process(Seq(CC,
                              "-c", 
                              IVERILOGCFLAGS,
                              CFLAGS + " -DIVERILOG_PLUGIN", 
                              "VpiPlugin.cpp",
                              "-o",
                              "VpiPlugin.o").mkString(" "), 
                            new File(pluginsPath)).! (new Logger()) == 0, 
                  "Compilation of VpiPlugin.o failed")

            assert(Process(Seq(CC,
                              IVERILOGCFLAGS,
                              CFLAGS,
                              "VpiPlugin.o",
                              IVERILOGLDFLAGS,
                              IVERILOGLDLIBS,
                              LDFLAGS,
                              "-o",
                              vpiModuleName).mkString(" "), 
                            new File(pluginsPath)).! (new Logger()) == 0, 
                  s"Compilation of $vpiModuleName failed")
    }
  } 

  def analyzeRTL() {
    val verilogSourcePaths = rtlSourcesPaths.filter { s => (s.endsWith(".v") || 
                                                           s.endsWith(".sv") ||
                                                           s.endsWith(".vl")) }
                                            .mkString(" ")

    val simulationDefSource = s"""
                               |`timescale 1ns/1ns
                               |
                               |module __simulation_def;
                               |initial
                               | begin
                               |  ${ if(hasWave) "$dumpfile(\"" + wavePath + "\");" else ""}
                               |  ${ if(hasWave) "$dumpvars(0," + toplevelName + ");" else ""}
                               |  $$readmempath("./rtl/");
                               | end
                               |endmodule""".stripMargin

    val simulationDefSourceFile = new PrintWriter(new File(s"${workspacePath}/rtl/" ++ 
                                                           "__simulation_def.v"))
    simulationDefSourceFile.write(simulationDefSource) 
    simulationDefSourceFile.close

    assert(Process(Seq(iverilogPath,
                       analyzeFlags,
                       "-o",
                       toplevelName + ".vvp",
                       "-s __simulation_def",
                       "-s",
                       toplevelName,
                       verilogSourcePaths,
                       s"./rtl/__simulation_def.v").mkString(" "),
                     new File(workspacePath)).! (new Logger()) == 0, 
           s"Analyze step of verilog files failed") 
  }

  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
    val vpiModulePath = if(!isWindows) pluginsPath + "/" + vpiModuleName
    else (pluginsPath + "/" + vpiModuleName).replaceAll("/C",raw"C:").replaceAll(raw"/",raw"\\")

    val pathStr = if(!isWindows) sys.env("PATH")

    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      def run(): Unit = {
        val retCode = Process(Seq(vvpPath,
                                "-M.",
                                s"-m${pwd + "/" +vpiModulePath}",
                                toplevelName + ".vvp",
                                runFlags).mkString(" "), 
                              new File(workspacePath)).! (new Logger())
      if (retCode != 0) iface.set_crashed(retCode)
      assert(retCode == 0, s"Simulation of $toplevelName failed")
      }
    })

    thread.setDaemon(true)
    thread.start()
    thread
  }

  override def isBufferedWrite: Boolean = true
}



