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

import scala.collection.mutable
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
                             var xelabFlags: Array[String] = null,
                             var timePrecision: String = null
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
      println("Wave format " + waveFormat + " not supported by XSim")
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
  val simulateDir        = s"${workPath}/${toplevelName}_xsim.sim/sim_1/behav/xsim"
  val simulatePath       = new File(simulateDir).getAbsolutePath
  val compilePath        = s"${simulatePath}/compile.${scriptSuffix}"
  val elaboratePath      = s"${simulatePath}/elaborate.${scriptSuffix}"

  val isMsys = sys.env.get("MSYSTEM").isDefined

  val msys2Shell = {
    if (isWindows) {
      if (isMsys) { // SBT Called from Msys
        "sh"
      } else { // SBT Called from CMD
        val msys2ShellPath =
          sys.env.getOrElse("MSYS2_ROOT", "C:\\msys64") + "\\msys2_shell.cmd"
        if (!new File(msys2ShellPath).exists) {
          throw new Exception(
            "MSYS2 Shell not found! Please Setup you MSYS2_ROOT Environment Variable."
          )
        }
        msys2ShellPath + " -defterm -here -no-start -mingw64"
      }
    } else {
      ""
    }
  }

  val vivadoHome = sys.env.getOrElse(
    "VIVADO_HOME",
    throw new Exception(
      "VIVADO_HOME not found! Setup your vivado environment first."
    )
  )
  val vivadoBinPath = new File(s"${vivadoHome}/bin").getAbsolutePath

  def doCmd(command: String, cwd: File, message : String, extraEnv: Seq[(String, String)] = Seq.empty) : Unit = {
    val logger = new Logger()
    if(Process(command, cwd, extraEnv :_*)! (logger) != 0){
      println(logger.logs)
      throw new Exception(message)
    }
  }
  def doCmdVivado(command: String, cwd: File, message: String, extraEnv: Seq[(String, String)] = Seq.empty) = {
    val cmdVivado =
      if (isWindows) {
        if (isMsys) {
          s"sh ${vivadoBinPath}/setEnvAndRunCmd.sh ${command}"
        } else {
          s"cmd /c ${vivadoBinPath}/setEnvAndRunCmd.bat ${command}"
        }
      } else {
        s"${vivadoBinPath}/setEnvAndRunCmd.sh ${command}"
      }

    doCmd(cmdVivado, cwd, message, extraEnv)
  }
  def doCmdMys2(command: String, cwd: File, message: String, extraEnv: Seq[(String, String)] = Seq.empty) : Unit = {
    val cmdMsys2 = s""" $msys2Shell -c "$command" """.trim
    doCmd(cmdMsys2, cwd, message, extraEnv)
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
      s"import_files -force -quiet [findFiles $p *.{xci,xcix}]"
    }
    val importBd = bdAbsoluteSourcesPaths map { p =>
      s"import_files -force -quiet [findFiles $p *.bd]"
    }

    val moreOptions = mutable.ArrayBuffer[String]()
    if (config.xelabFlags != null) {
      moreOptions ++= config.xelabFlags
    }
    if (config.timePrecision != null) {
      val timePrecision = config.timePrecision.replace(" ","")
      moreOptions += "-override_timeprecision"
      moreOptions += "-timescale " + timePrecision + "/" + timePrecision
      moreOptions += "-timeprecision_vhdl " + timePrecision
    }

    val generateSimulateScriptPre =
      s"""
        |update_compile_order -fileset sources_1
        |set_property top $toplevelName [get_fileset sim_1]
        |set_property -name {xelab.dll} -value {1} -objects [get_filesets sim_1]
        |set_property -name {xelab.more_options} -value {${moreOptions.mkString(" ")}} -objects [get_filesets sim_1]
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

    var vivadoScriptPath = scriptPath.replace("\\", "/")
    if (isWindows) vivadoScriptPath = s""""$vivadoScriptPath""""
    val command =  s""" vivado -mode batch -source $vivadoScriptPath """.trim
    doCmdVivado(command,
      new File(workPath),
      "Generation of vivado script failed")
  }

  def getScriptCommand(cmd: String) = {
    if (isWindows) {
      s"${simulatePath}\\${cmd}.bat"
    } else {
      s"${simulatePath}/${cmd}.sh"
    }
  }

  def runScript() = {
    doCmdVivado(getScriptCommand("compile"),
      new File(simulatePath),
      "run compile failed")
    doCmdVivado(getScriptCommand("elaborate"),
      new File(simulatePath),
      "run elaborate failed")
      val simDesignDir = s"${simulatePath}/xsim.dir"
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

    val xsim = Paths.get(vivadoHome, "data", "xsim").toAbsolutePath.toString
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

    val gccCmd = Seq(CC, cFlags.mkString(" ")).mkString(" ")
    if (isWindows) {
      doCmdMys2(
        gccCmd,
        new File(s"${workspacePath}/${workspaceName}"),
        "Compilation of driver failed"
      )
    } else {
      doCmd(
        gccCmd,
        new File(s"${workspacePath}/${workspaceName}"),
        "Compilation of driver failed"
      )
    }
    System.load(driverPath)
  }

  def getInterface() = {
    val xsiIface = new XSIIface()
    xsiIface
  }

  FileUtils.deleteQuietly(new File(s"${workPath}"))
  FileUtils.deleteQuietly(new File("xsim.dir"))
  new File(workPath).mkdirs()
  generateScript()
  runScript()
  compileDriver()
}
