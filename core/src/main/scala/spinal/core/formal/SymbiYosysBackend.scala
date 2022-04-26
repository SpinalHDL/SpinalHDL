package spinal.core.formal

import java.io.{BufferedInputStream, File, FileFilter, FileInputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import org.apache.commons.io.FileUtils

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import sys.process._

class SpinalSbyException extends Exception {}

object SbyMode extends Enumeration {
  type SbyMode = Value

  val Bmc = Value("bmc")
  val Prove = Value("prove")
  val Live = Value("live")
  val Cover = Value("cover")
  val Equiv = Value("equiv")
  val Synth = Value("synth")
}

sealed trait SbyEngine extends FormalEngin {
  def command: String
}

object SmtBmcSolver extends Enumeration {
  type SmtBmcSolver = Value

  val Yices = Value("yices")
  val Boolector = Value("booleactor")
  val Z3 = Value("z3")
  val mathsat = Value("mathsat")
  val cvc4 = Value("cvc4")
}

case class SmtBmc(
    nomem: Boolean = false,
    syn: Boolean = false,
    stbv: Boolean = false,
    stdt: Boolean = false,
    nopresat: Boolean = false,
    unroll: Option[Boolean] = None,
    dumpsmt2: Boolean = false,
    progress: Boolean = true,
    solver: SmtBmcSolver.SmtBmcSolver = SmtBmcSolver.Yices
) extends SbyEngine {
  def command: String = {
    "smtbmc" +
      (if (nomem) { " --nomem" }
       else { "" }) +
      (if (syn) { " --syn" }
       else { "" }) +
      (if (stbv) { " --stbv" }
       else { "" }) +
      (if (stdt) { " --stdt" }
       else { "" }) +
      (if (nopresat) { " --nopresat" }
       else { "" }) +
      (unroll
        .map(on => {
          if (on) { " --unroll" }
          else { "--nounroll" }
        })
        .getOrElse("")) +
      (if (dumpsmt2) { " --dumpsmt2" }
       else { "" }) +
      (if (progress) { " --progress" }
       else { "" }) +
      s" $solver"
  }
}

case class Aiger() extends SbyEngine {
  def command: String = { "aiger" }
}

case class Abc() extends SbyEngine {
  def command: String = { "abc" }
}

class SymbiYosysBackendConfig(
    val rtlSourcesPaths: ArrayBuffer[String] = ArrayBuffer[String](),
    val rtlIncludeDirs: ArrayBuffer[String] = ArrayBuffer[String](),
    var engines: ArrayBuffer[SbyEngine] = ArrayBuffer(SmtBmc()),
    var modesWithDepths: HashMap[String, Int] = HashMap[String, Int](),
    var timeout: Option[Int] = None,
    var multiClock: Boolean = false,
    var toplevelName: String = null,
    var workspacePath: String = null,
    var workspaceName: String = null,
    var keepDebugInfo: Boolean = false
)

class SymbiYosysBackend(val config: SymbiYosysBackendConfig) extends FormalBackend {
  val workspaceName = config.workspaceName
  val workspacePath = config.workspacePath
  val workDir = s"${workspacePath}/${workspaceName}"
  val sbyFileName = s"${config.toplevelName}.sby"
  val sbyFilePath = new File(s"${workDir}/$sbyFileName").getAbsolutePath

  def clean(): Unit = {
    FileUtils.deleteQuietly(new File(workDir))
  }

  def genSby(): Unit = {
    val localSources = config.rtlSourcesPaths.map(f => new File(f).getAbsolutePath).mkString("\n")
    val read = config.rtlSourcesPaths
      .map(f => {
        Paths.get(f).getFileName
      })
      .mkString(" ")
    val engineCmds = config.engines.map(engine => engine.command).mkString("\n")

    if (!config.modesWithDepths.nonEmpty) config.modesWithDepths("bmc") = 100
    var modes = config.modesWithDepths.keys
    val taskCmd = modes.mkString("\n")
    val modeCmd = modes.map(x => s"$x: mode $x").mkString("\n")
    val depthCmd = modes.map(x => s"$x: depth ${config.modesWithDepths(x)}").mkString("\n")

    val script = "[tasks]\n" +
      taskCmd +
      "\n\n" +
      "[options]\n" +
      modeCmd +
      "\n" +
      depthCmd +
      "\n" +
      (config.timeout.map(t => s"timeout $t\n").getOrElse("")) +
      (if (config.multiClock) { "multiclock on\n" }
       else { "" }) +
      "\n" +
      "[engines]\n" +
      engineCmds +
      "\n\n" +
      "[script]\n" +
      s"read -formal $read\n" +
      s"prep -top ${config.toplevelName}\n" +
      "\n" +
      "[files]\n" +
      localSources

    new File(workDir).mkdir()
    Files.write(
      Paths.get(sbyFilePath),
      script.getBytes()
    )
  }

  class Logger extends ProcessLogger {
    override def err(s: => String): Unit = { if (!s.startsWith("ar: creating ")) println(s) }
    override def out(s: => String): Unit = {}
    override def buffer[T](f: => T) = f
  }

  def doVerify(name: String): Unit = {
    println(f"[Progress] Start ${config.toplevelName} formal verification with $name.")
    val isWindows = System.getProperty("os.name").toLowerCase.contains("windows")
    val command = if (isWindows) "sby.exe" else "sby"
    val success = Process(Seq(command, "-f", sbyFilePath), new File(workspacePath)).!(new Logger()) == 0
    if(!success){
      val logFileName = config.modesWithDepths.head._1 match {
        case "bmc" => "logfile.txt"
        case "prove" => "logfile_basecase.txt"
        case "cover" => "logfile.txt"
      }
      val logFile = new File(workDir + s"/${config.toplevelName}_${config.modesWithDepths.head._1}/engine_0/$logFileName")
      if(logFile.exists()){
        val pattern = """(\w+.sv):(\d+).(\d+)-(\d+).(\d+)""".r
        for (line <- Source.fromFile(logFile).getLines) {
          println(line)
          if(line.contains("Assert failed in") || line.contains("Unreached cover statement")){
            pattern.findFirstMatchIn(line) match {
              case Some(x) => {
                val sourceName = x.group(1)
                val assertLine = x.group(2).toInt
                val source = Source.fromFile(workspacePath + "/rtl/"+sourceName).getLines.drop(assertLine)
                println("--   " + source.next().dropWhile(_ == ' '))
              }
              case None =>
            }
          }
        }
      }
      throw new Exception("SymbiYosys failure")
    }
    if (!config.keepDebugInfo) clean()
  }

  def checks(): Unit = {
    config.modesWithDepths.keySet.map(x => SbyMode.withName(x))
  }

  checks()
  genSby()
}
