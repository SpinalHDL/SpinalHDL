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
  val rtlIncludeDirs : ArrayBuffer[String] = ArrayBuffer[String](),
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
  var CFLAGS: String         = "-std=c++11 -Wall -Wextra -pedantic -O2 -Wno-strict-aliasing -Wno-write-strings", 
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

  val includes = Seq(
    s"$jdk/include",
    s"$jdk/include/${(if(isWindows) "win32"
                      else (if (isMac) "darwin"
                      else "linux"))}"
  )

  CFLAGS += " " + includes.map(path =>
    if (isWindows)
      '"' + path + '"'
    else
      path
  ).map(path => s"-I$path").mkString(" ") + " "

  def doCmd(command: String, cwd: File, message : String) = {
    val logger = new Logger()
    if(Process(command, cwd)! (logger) != 0){
      println(logger.logs)
      throw new Exception(message)
    }
  }
  def doCmd(command: String, cwd: File, extraEnv: Seq[(String, String)], message : String) = {
    val logger = new Logger()
    if(Process(command, cwd, extraEnv :_*)! (logger) != 0){
      println(logger.logs)
      throw new Exception(message)
    }
  }

  class Logger extends ProcessLogger {
    val logs = new StringBuilder()
    override def err(s: => String): Unit = { logs ++= (s) }
    override def out(s: => String): Unit = { logs ++= (s) }
    override def buffer[T](f: => T) = f
  }

  class LoggerPrint extends ProcessLogger {
    override def err(s: => String): Unit = { println(s) }
    override def out(s: => String): Unit = { println(s) }
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

      doCmd(
        Seq(CC,
          "-c",
          CFLAGS,
          "SharedMemIface.cpp",
          "-o",
          "SharedMemIface.o"
        ).mkString(" "),
        new File(pluginsPath),
        "Compilation of SharedMemIface.cpp failed"
      )

      doCmd(
        Seq(CC,
          "-c",
          CFLAGS,
          "SharedMemIface_wrap.cxx",
          "-o",
          "SharedMemIface_wrap.o"
        ).mkString(" "),
        new File(pluginsPath),
       "Compilation of SharedMemIface_wrap.cxx failed"
      )

      doCmd(
        Seq(CC,
          CFLAGS + (if(isMac) " -dynamiclib " else ""),
          "SharedMemIface.o",
          "SharedMemIface_wrap.o",
          LDFLAGS,
          "-o",
          sharedMemIfaceName
        ).mkString(" "),
        new File(pluginsPath),
      "Compilation of SharedMemIface." + sharedExtension + " failed")
    }

    System.load(pwd + "/" + sharedMemIfacePath)
    compileVPI()
    analyzeRTL()
  }

  def clean() : Unit = {
    FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
    FileUtils.cleanDirectory(new File(pluginsPath))
  }

  def compileVPI() : Unit   // Return the plugin name
  def analyzeRTL() : Unit
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
        instanciate_()
      }
    } else {
      this.synchronized {
        instanciate_()
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

          doCmd(
            Seq(ghdlPath,
             "--vpi-compile",
             CC, 
             "-c", 
             CFLAGS + " -DGHDL_PLUGIN", 
             "VpiPlugin.cpp",
             "-o",
             "VpiPlugin.o"
            ).mkString(" "),
           new File(pluginsPath),
           "Compilation of VpiPlugin.o failed"
          )

        doCmd(
          Seq(ghdlPath,
           "--vpi-link",
           CC,
           CFLAGS,
           "VpiPlugin.o",
           LDFLAGS,
           "-o",
           vpiModuleName
          ).mkString(" "),
           new File(pluginsPath),
         s"Compilation of $vpiModuleName failed"
        )
    }
  } 

  def analyzeRTL() : Unit = {

    val vhdlSourcePaths = rtlSourcesPaths.filter { s => (s.endsWith(".vhd") || 
    s.endsWith(".vhdl")) }
      .mkString(" ")

    doCmd(
      Seq(ghdlPath,
        "-a",
        analyzeFlags,
        "-fsynopsys",
        vhdlSourcePaths
      ).mkString(" "),
      new File(workspacePath),
      s"Analyze step of vhdl files failed"
    )

    doCmd(
      Seq(ghdlPath,
        "-e",
        elaborationFlags,
        "-fsynopsys",
        toplevelName
      ).mkString(" "),
      new File(workspacePath),
      s"Elaboration of $toplevelName failed"
    )
  }

  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
    val vpiModulePath = if(!isWindows) pluginsPath + "/" + vpiModuleName
    else (pluginsPath + "/" + vpiModuleName).replaceAll("/C",raw"C:").replaceAll(raw"/",raw"\\")


    val pathStr = if(!isWindows) {
      sys.env("PATH")
    } else {
      val pathKey = sys.env.keysIterator.find(s => s.equalsIgnoreCase("PATH")).get
      sys.env(pathKey) + ";" + (ghdlPath + " --vpi-library-dir").!!.trim
    }

    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      def run(): Unit = {
        val retCode = Process(Seq(ghdlPath,
          "-r",
          elaborationFlags,
          "-fsynopsys",
          toplevelName,
          s"--vpi=${pwd + "/" + vpiModulePath}",
          runFlags).mkString(" "),
        new File(workspacePath),
        "PATH" -> pathStr).! (new LoggerPrint())

        if (retCode != 0) {
          iface.set_crashed(retCode)
          println(s"Simulation of $toplevelName failed")
        }
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

      doCmd(
        Seq(CC,
          "-c",
          IVERILOGCFLAGS,
          CFLAGS + " -DIVERILOG_PLUGIN",
          "VpiPlugin.cpp",
          "-o",
          "VpiPlugin.o"
        ).mkString(" "),
        new File(pluginsPath),
        "Compilation of VpiPlugin.o failed"
      )

      doCmd(
        Seq(CC,
          IVERILOGCFLAGS,
          CFLAGS,
          "VpiPlugin.o",
          IVERILOGLDFLAGS,
          IVERILOGLDLIBS,
          LDFLAGS,
          "-o",
          vpiModuleName
        ).mkString(" "),
        new File(pluginsPath),
        s"Compilation of $vpiModuleName failed"
      )
    }
  } 

  def analyzeRTL() : Unit = {
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

    config.rtlSourcesPaths.filter(s => s.endsWith(".bin") || s.endsWith(".mem")).foreach(path =>  FileUtils.copyFileToDirectory(new File(path), new File(workspacePath)))

    val verilogIncludePaths = config.rtlIncludeDirs.map("-I " + new File(_).getAbsolutePath).mkString(" ")

    doCmd(
      Seq(iverilogPath,
        analyzeFlags,
        "-o",
        toplevelName + ".vvp",
        "-s __simulation_def",
        "-s",
        toplevelName,
        verilogIncludePaths,
        verilogSourcePaths,
        s"./rtl/__simulation_def.v"
      ).mkString(" "),
      new File(workspacePath),
      s"Analyze step of verilog files failed"
    )
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
                              new File(workspacePath)).! (new LoggerPrint())
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

class VCSBackendConfig extends VpiBackendConfig {
  var vcsCC: Option[String] = None
  var vcsLd: Option[String] = None
  var elaborationFlags: String = ""
  var waveDepth = 0
  var simSetupFile: String = _
  var envSetup: ()=> Unit = _
}

class VCSBackend(config: VCSBackendConfig) extends VpiBackend(config) {
  import Backend._
  import config._
  override def isBufferedWrite: Boolean = true

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.VPD, WaveFormat.DEFAULT, WaveFormat.FSDB, WaveFormat.NONE)

  val format = if(availableFormats contains config.waveFormat) {
    config.waveFormat
  } else {
    println("Wave format " + config.waveFormat + " not supported by VCS for now")
    WaveFormat.NONE
  }

  val vpiModuleName = "vpi_vcs.so"
  val vpiModulePath = pluginsPath + "/" + vpiModuleName
  val vpiModuleAbsPath = new File(vpiModulePath).getAbsolutePath().mkString
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
      if(config.waveFormat == WaveFormat.FSDB){
        var info = "$VERDI_HOME not found"
        if(sys.env.get("LD_LIBRARY_PATH") == None) {
          info += "\n $LD_LIBRARY_PATH not found"
        }
        println(s"[Error]: ${info}")
        " "
      }
    }
  }

  def compileVPI(): Unit = {
    if(Files.exists(Paths.get(vpiModulePath))) {
      return
    }
    val vpiPluginSource = Array("/VpiPlugin.cpp", "/SharedStruct.hpp")
    for(filename <- vpiPluginSource) {
      val cppSourceFile = new PrintWriter(new File(pluginsPath + "/" + filename))
      val stream = getClass.getResourceAsStream(filename)
      cppSourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
      cppSourceFile.close
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

  private def vloganFlags(): String = {
    val commonFlags = List(
      "-nc",
      "-full64",
      "-sverilog",
      "+v2k",
      "-ntb",
      "-debug_access+all",
      "-timescale=1ns/1ps",
      "-l vlogan.log"
    )
    val analysisFlags = List(config.analyzeFlags)
    (commonFlags ++ analysisFlags).mkString(" ")
  }

  private def vhdlanFlags(): String = {
    val commonFlags = List(
      "-nc",
      "-full64",
      "-debug_access+all",
      "-timescale=1ns/1ps",
      "-l vhdlan.log"
    )
    val analysisFlags = List(config.analyzeFlags)
    (commonFlags ++ analysisFlags).mkString(" ")
  }

  private def vcsFlags(): String = {
    val commonFlags = List(
      "-full64",
      "-quiet",
      "-timescale=1ns/1ps",
      "-debug_access+all",
      "-debug_acc+pp+dmptf",
      "-debug_region=+cell+encrypt",
      "-l vcs.log",
      "+vpi",
      "+vcs+initreg+random",
      "-load",
      s"$vpiModuleAbsPath:entry_point_cb",
      "-o",
      toplevelName
    )
    val cc = config.vcsCC match {
      case Some(x) => List("-cc", x)
      case None => List.empty
    }
    val ld = config.vcsLd match {
      case Some(x) => List("-ld", x)
      case None => List.empty
    }

    val dump = waveFormat match {
      case WaveFormat.VCD => List(s"+vcs+dumpvars+$toplevelName.vcd")
      case WaveFormat.VPD => List(s"+vcs+vcdpluson")
      case WaveFormat.FSDB =>
        List(s"-P ${verdi_home}/share/PLI/VCS/LINUX64/novas.tab", s"$verdi_home/share/PLI/VCS/LINUX64/pli.a")
      case _ => List.empty
    }
    val elaborateFlags = List(config.elaborationFlags)
    val cmd = (commonFlags ++ cc ++ ld ++ dump ++ elaborateFlags).mkString(" ")
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
    val verilogSourcePaths = rtlSourcesPaths.filter {
      s => s.endsWith(".v") || s.endsWith(".sv") || s.endsWith(".vl") || s.endsWith(".vh") || s.endsWith(".svh") || s.endsWith(".vr")
    }//.mkString(" ")
    val vhdlSourcePaths = rtlSourcesPaths.filter {
      s => s.endsWith(".vhd") || s.endsWith(".vhdl")
    }//.mkString(" ")

    config.rtlSourcesPaths.filter {
      s => s.endsWith(".bin") || s.endsWith(".mem")
    }.foreach {
      path => FileUtils.copyFileToDirectory(new File(path), new File(workspacePath))
    }

    val fileList = (verilogSourcePaths ++ vhdlSourcePaths).mkString("\n")
    val fileListFile = new PrintWriter(
      new File(s"$workspacePath/filelist.f")
    )
    fileListFile.write(fileList)
    fileListFile.close()

    val simWaveSource =
      s"""
         |`timescale 1ns/1ps
         |module __simulation_def;
         |initial begin
         |  $$fsdbDumpfile("$toplevelName.fsdb");
         |  $$fsdbDumpvars($waveDepth, $toplevelName);
         |  $$fsdbDumpflush;
         |end
         |endmodule""".stripMargin
    val simulationDefSourceFile = new PrintWriter(new File(s"$workspacePath/rtl/" ++
      "__simulation_def.v"))
    simulationDefSourceFile.write(simWaveSource)
    simulationDefSourceFile.close()

    val fsdbv = waveFormat match {
      case WaveFormat.FSDB => " ./rtl/__simulation_def.v"
      case _=> " "
    }
    val topUnits = waveFormat match {
      case WaveFormat.FSDB => toplevelName + " __simulation_def"
      case _=> toplevelName
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
      if (verilogSourcePaths.nonEmpty){
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
    private def printWithFilter(s: => String): Unit =  {
      if (s.contains("Unexpected termination of the simulation")) {
        terminationOfSimulation = true
      }
      println(s)
    }
    override def err(s: => String): Unit = { printWithFilter(s) }
    override def out(s: => String): Unit = { printWithFilter(s) }
    override def buffer[T](f: => T) = f
  }

  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
    val thread = new Thread(new Runnable {
      val iface = sharedMemIface
      val logger = new LoggerPrintWithTerminationFilter()
      def run(): Unit = {
        val runCommand = Seq(s"./$toplevelName", config.runFlags).mkString(" ")
        println(s"[SIMV] $runCommand")
        val retCode = Process(runCommand, new File(workspacePath)).! (logger)
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
