package spinal.sim

import java.nio.file.Paths
import java.io.{File, PrintWriter}
import java.lang.Thread
import scala.io.Source
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process._
import spinal.sim.xsi._

import scala.util.Properties

case class XSimBackendConfig(
                             val rtlIncludeDirs : ArrayBuffer[String] = ArrayBuffer[String](),
                             val rtlSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var xciSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var bdSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var xilinxDevice:String = "xc7vx485tffg1157-1",
                             var CC: String             = "g++",
                             var toplevelName: String   = null,
                             var workspacePath: String  = null,
                             var workspaceName: String  = null,
                             var wavePath: String       = null,
                             var waveFormat: WaveFormat = WaveFormat.NONE,
                             var userSimulationScript: String = null,
                             var xelabFlags: Array[String] = null
                           )

class XSimBackend(config: XSimBackendConfig) extends Backend {
  import Backend._
  override def isBufferedWrite: Boolean = false
  val rtlIncludeDirs = config.rtlIncludeDirs
  val rtlSourcesPaths = config.rtlSourcesPaths
  val xciSourcesPaths = config.xciSourcesPaths
  val bdSourcesPaths = config.bdSourcesPaths
  val CC = config.CC
  val toplevelName = config.toplevelName
  val workspacePath = config.workspacePath
  val workspaceName = config.workspaceName
  val wavePath = config.wavePath
  val waveFormat = config.waveFormat
  val xilinxDevice = config.xilinxDevice

  val format: WaveFormat = {
    val availableFormats = Array(WaveFormat.WDB, WaveFormat.DEFAULT, WaveFormat.NONE)
    if (availableFormats contains waveFormat){
      if (format == WaveFormat.DEFAULT) {
        WaveFormat.WDB
      } else {
        waveFormat
      }
    } else {
      println("Wave format " + waveFormat + " not supported by GHDL")
      WaveFormat.NONE
    }
  }
  val hasWave = if (format != WaveFormat.NONE) { "true" } else { "false" }

  def vivadoPath(s: String): String = {
    Paths.get(s).toAbsolutePath.toString.replace("\\", "/")
  }
  val rtlAbsoluteSourcesPaths = (rtlSourcesPaths ++ rtlIncludeDirs) map vivadoPath
  val xciAbsoluteSourcesPaths = xciSourcesPaths map vivadoPath
  val bdAbsoluteSourcesPaths = bdSourcesPaths map vivadoPath

  val scriptName = "spinal_xsim.tcl"
  val headerName = "spinal_xsim.h"
  val workPath = s"${workspacePath}/${workspaceName}"
  val projectName = toplevelName + "_xsim"
  val scriptPath = new File(s"${workPath}/$scriptName").getAbsolutePath
  val headerPath = new File(s"${workPath}/$headerName").getAbsolutePath
  val fixScriptPath = new File(s"${workPath}/sed.sh").getAbsolutePath

  val scriptSuffix = {
    if (isWindows) {
      ".bat"
    } else {
      ".sh"
    }
  }
  val simulatePath = new File(s"${workPath}/${projectName}.sim/sim_1/behav/xsim").getAbsolutePath
  val simulateKernelPath = new File(s"${simulatePath}/xsim.dir/${toplevelName}_behav").getAbsolutePath
  val compilePath = s"${simulatePath}/compile.${scriptSuffix}"
  val elaboratePath = s"${simulatePath}/elaborate.${scriptSuffix}"

  val vivadoInstallPath = sys.env.get("VIVADO_HOME") match {
    case Some(x) => x
    case None => {
      assert(assertion = false, "VIVADO_HOME not found! Setup your vivado environment first.")
      ""
    }
  }

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
    override def err(s: => String): Unit = { logs ++= (s + Properties.lineSeparator) }
    override def out(s: => String): Unit = { logs ++= (s + Properties.lineSeparator) }
    override def buffer[T](f: => T) = f
  }

  def generateScript() = {
    val findFiles =
      """
         |proc findFiles { basedir pattern } {
         |
         |   # Fix the directory name, this ensures the directory name is in the
         |   # native format for the platform and contains a final directory seperator
         |   set basedir [string trimright [file join [file normalize $basedir] { }]]
         |   set fileList {}
         |
         |   # Look in the current directory for matching files, -type {f r}
         |   # means ony readable normal files are looked at, -nocomplain stops
         |   # an error being thrown if the returned list is empty
         |   foreach fileName [glob -nocomplain -type {f r} -path $basedir $pattern] {
         |       lappend fileList $fileName
         |   }
         |
         |   # Now look for any sub direcories in the current directory
         |   foreach dirName [glob -nocomplain -type {d  r} -path $basedir *] {
         |       # Recusively call the routine on the sub directory and append any
         |       # new files to the results
         |       set subDirList [findFiles $dirName $pattern]
         |       if { [llength $subDirList] > 0 } {
         |           foreach subDirFile $subDirList {
         |               lappend fileList $subDirFile
         |           }
         |       }
         |   }
         |   return $fileList
         |}
         |
         |""".stripMargin

    val createProject = s"create_project -force $projectName -part $xilinxDevice"
    val importRtl = rtlAbsoluteSourcesPaths map { p =>
      s"import_files -force $p"
    }
    val importXsi = xciAbsoluteSourcesPaths map { p =>
      s"import_files -force -quiet [findFiles $p *.xci]"
    }
    val importBd = bdAbsoluteSourcesPaths map { p =>
      s"import_files -force -quiet [findFiles $p *.bd]"
    }
    val generateSimulateScriptPre =
      s"""
        |update_compile_order -fileset sources_1
        |set_property top $toplevelName [get_fileset sim_1]
        |""".stripMargin
    val generateSimulateScriptPost =
      s"""
        |launch_simulation -scripts_only
        |close_project
        |""".stripMargin

    if (config.userSimulationScript == null) {
      config.userSimulationScript = ""
    }

    val script = Seq(findFiles,
      createProject,
      importRtl.mkString("\n"),
      importXsi.mkString("\n"),
      importBd.mkString("\n"),
      generateSimulateScriptPre,
      config.userSimulationScript,
      generateSimulateScriptPost).mkString("\n")

    val outFile = new java.io.FileWriter(scriptPath)
    outFile.write(script)
    outFile.flush()
    outFile.close()

    val vivadoScriptPath = scriptPath.replace("\\", "/")
    val command = {
      val baseCommand = s"vivado -mode batch -source $vivadoScriptPath"
      if (isWindows) {
        s"sh -c \'${baseCommand}\'"
      } else {
        baseCommand
      }
    };
    doCmd(command,
      new File(workPath),
      "Generation of vivado script failed")

    // Fix elaborate
    val additionalElaborateCommand = (List(
      "-dll"
    ) ++ config.xelabFlags).mkString(" ")
    val fixElaborateCommand = if (isWindows) {
      s"sed \'/^call xelab/ s/$$/ ${additionalElaborateCommand}/\' -i elaborate.bat"
    } else {
      s"sed \'/^xelab/ s/$$/ ${additionalElaborateCommand}/\' -i elaborate.sh"
    }
    val outFile2 = new java.io.FileWriter(fixScriptPath)
    outFile2.write(fixElaborateCommand)
    outFile2.flush()
    outFile2.close()

    doCmd(s"sh $fixScriptPath",
      new File(simulatePath),
      "Fix vivado elaborate command failed")
  }

  def getScriptCommand(cmd: String) = {
    if (isWindows) {
      s"cmd /k ${cmd}.bat"
    } else {
      s"bash ${cmd}.sh"
    }
  }

  def runScript() = {
    doCmd(getScriptCommand("compile"),
      new File(simulatePath),
      "run compile failed")
    doCmd(getScriptCommand("elaborate"),
      new File(simulatePath),
      "run elaborate failed")
    val simDesignDir = s"${workspacePath}/${workspaceName}/${toplevelName}_xsim.sim/sim_1/behav/xsim/xsim.dir"
    FileUtils.copyDirectory(new File(simDesignDir), new File("xsim.dir"))
  }

  def compileDriver() = {
    val java_home = System.getProperty("java.home")
    assert(java_home != "" && java_home != null, "JAVA_HOME need to be set")
    val jdk = java_home.replace("/jre","").replace("\\jre","")
    val jdkIncludes = if (isWindows) {
      FileUtils.copyDirectory(new File(s"$jdk\\include"), new File(s"${workspacePath}\\${workspaceName}\\jniIncludes"))
      s"jniIncludes"
    } else {
      jdk + "/include"
    }

    val xsim = Paths.get(vivadoInstallPath, "data", "xsim").toAbsolutePath.toString
    val xsimIncludes = if (isWindows) {
      FileUtils.copyDirectory(new File(s"$xsim\\include"), new File(s"${workspacePath}\\${workspaceName}\\xsimIncludes"))
      s"xsimIncludes"
    } else {
      xsim + "/include"
    }

    val dylibSuffix = if (isWindows) {
      "dll"
    } else {
      "so"
    }

    val simKernel = s"librdi_simulator_kernel.${dylibSuffix}"
    val simDesign = s"xsim.dir/${toplevelName}_behav/xsimk.${dylibSuffix}"
    val header =
      s"""
         |#define SIM_KERNEL "${simKernel}"
         |#define SIM_DESIGN "${simDesign}"
         |#define SIM_WAVE   "${wavePath}"
         |#define SIM_HAS_WAVE ${hasWave}
         |""".stripMargin

    val outFile = new java.io.FileWriter(headerPath)
    outFile.write(header)
    outFile.flush()
    outFile.close()

    val cppSourceList = List(
      "XSIIface_wrap.cxx",
      "XSIIface.cpp",
      "xsi_loader.cpp"
    )
    val cppHeaderList = List(
      "XSIIface.hpp",
      "xsi_loader.h",
      "xsi_shared_lib.h"
    )

    (cppSourceList ++ cppHeaderList) foreach { name =>
      val sourceFile = new PrintWriter(new File(s"${workspacePath}/${workspaceName}/${name}"))
      val stream = getClass.getResourceAsStream(s"/${name}")
      sourceFile.write(scala.io.Source.fromInputStream(stream).mkString)
      sourceFile.close
    }

    val driver = s"spinal_xsim.${dylibSuffix}"
    val driverPath = new File(s"${workspacePath}/${workspaceName}/${driver}").getAbsolutePath
    val ldFlags = if (isLinux) List("-lrt", "-ldl") else List()
    val cFlags = List(
      s"-I$jdkIncludes",
      s"-I$xsimIncludes",
      s"-I$jdkIncludes/${if(isWindows)"win32" else (if(isMac) "darwin" else "linux")}",
      "-fPIC",
      "-m64",
      "-shared",
      "-Wno-attributes",
      s"-o ${driver}",
      cppSourceList.mkString(" "),
      ldFlags.mkString(" ")
    )

    doCmd(
      Seq(CC,
        cFlags.mkString(" ")
      ).mkString(" "),
      new File(s"${workspacePath}/${workspaceName}"),
      "Compilation of driver failed"
    )
    System.load(driverPath)
  }

  def getInterface() = {
    val xsiIface = new XSIIface()
    xsiIface
  }

  FileUtils.deleteQuietly(new File(s"${workPath}"))
  new File(workPath).mkdirs()
  generateScript()
  runScript()
  compileDriver()
}
