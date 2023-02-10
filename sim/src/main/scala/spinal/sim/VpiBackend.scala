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
  var logSimProcess: Boolean = false,
  var timePrecision: String  = null
)

abstract class VpiBackend(val config: VpiBackendConfig) extends Backend {
  import Backend._
  val rtlSourcesPaths = config.rtlSourcesPaths 
  val toplevelName    = config.toplevelName    
  val pluginsPath     = config.pluginsPath     
  val workspacePath   = config.workspacePath   
  val workspaceName   = config.workspaceName   
  var wavePath        = config.wavePath        
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


