package spinal.sim

import java.nio.file.{Paths, Files, Path}
import java.io.{File, PrintWriter}
import java.lang.Thread
import scala.io.Source
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process._

case class XSimBackendConfig(
                             val rtlIncludeDirs : ArrayBuffer[String] = ArrayBuffer[String](),
                             val rtlSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var xciSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var bdSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
                             var toplevelName: String   = null,
                             var pluginsPath: String    = "simulation_plugins",
                             var workspacePath: String  = null,
                             var workspaceName: String  = null,
                             var wavePath: String       = null,
                             var waveFormat: WaveFormat = WaveFormat.NONE
                           )

class XSimBackend(config: XSimBackendConfig) extends Backend {
  import Backend._
  override def isBufferedWrite: Boolean = false
  val rtlIncludeDirs = config.rtlIncludeDirs
  val rtlSourcesPaths = config.rtlSourcesPaths
  val xciSourcesPaths = config.xciSourcesPaths
  val bdSourcesPaths = config.bdSourcesPaths
  val toplevelName = config.toplevelName
  val pluginsPath = config.pluginsPath
  val workspacePath = config.workspacePath
  val workspaceName = config.workspaceName
  val wavePath = config.wavePath
  val waveFormat = config.waveFormat

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

  def vivadoPath(s: String): String = {
    Paths.get(s).toAbsolutePath.toString.replace("\\", "/")
  }
  val rtlAbsoluteSourcesPaths = (rtlSourcesPaths ++ rtlIncludeDirs) map vivadoPath
  val xciAbsoluteSourcesPaths = xciSourcesPaths map vivadoPath
  val bdAbsoluteSourcesPaths = bdSourcesPaths map vivadoPath

  val scriptName = s"spinal_xsim.tcl"
  val workPath = s"${workspacePath}/${workspaceName}"
  val projectName = toplevelName + "_xsim"
  val scriptPath = new File(s"${workPath}/$scriptName").getAbsolutePath

  val scriptSuffix = {
    if (isWindows) {
      ".bat"
    } else {
      ".sh"
    }
  }
  val simulatePath = new File(s"${workPath}/${projectName}.sim/sim_1/behav/xsim").getAbsolutePath
  val compilePath = s"${simulatePath}/compile.${scriptSuffix}"
  val elaboratePath = s"${simulatePath}/elaborate.${scriptSuffix}"

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

    val createProject = s"create_project -force $projectName"
    val importRtl = rtlAbsoluteSourcesPaths map { p =>
      s"import_files -force $p"
    }
    val importXsi = xciAbsoluteSourcesPaths map { p =>
      s"import_files -force -quiet [findFiles $p *.xci]"
    }
    val importBd = bdAbsoluteSourcesPaths map { p =>
      s"import_files -force -quiet [findFiles $p *.bd]"
    }
    val generateSimulateScript =
      s"""
        |update_compile_order -fileset sources_1
        |set_property top $toplevelName [get_fileset sim_1]
        |launch_simulation -scripts_only
        |close_project
        |""".stripMargin
    val script = Seq(findFiles,
      createProject,
      importRtl.mkString("\n"),
      importXsi.mkString("\n"),
      importBd.mkString("\n"),
      generateSimulateScript).mkString("\n")

    val outFile = new java.io.FileWriter(scriptPath)
    outFile.write(script)
    outFile.flush()
    outFile.close()

    val vivadoScriptPath = scriptPath.replace("\\", "/")
    doCmd(s"sh -c \'vivado -mode batch -source $vivadoScriptPath\'",
      new File(workPath),
      "Generation of vivado script failed")

    // Fix elaborate
    val fixElaborateCommand = if (isWindows) {
      "sed \'/^call xelab/ s/$/ -dll/\' -i elaborate.bat"
    } else {
      "sed \'/^xelab/ s/$/ -dll/\' -i elaborate.sh"
    }

    doCmd(fixElaborateCommand,
      new File(simulatePath),
      "Fix vivado elaborate command failed")
  }

  def getScriptCommand(cmd: String) = {
    if (isWindows) {
      s"cmd /k ${cmd}.bat"
    } else {
      s"sh ${cmd}.sh"
    }
  }

  def runScript() = {
    doCmd(getScriptCommand("compile"),
      new File(simulatePath),
      "run compile failed")
    doCmd(getScriptCommand("elaborate"),
      new File(simulatePath),
      "run elaborate failed")
  }

  FileUtils.deleteQuietly(new File("workPath"))
  new File(workPath).mkdirs()
  generateScript()
  runScript()
}
