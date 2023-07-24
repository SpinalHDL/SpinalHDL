package spinal.core.formal

import java.io.{BufferedInputStream, File, FileFilter, FileInputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import org.apache.commons.io.FileUtils

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
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
  /** bitwuzla: is untested
   */
  val bitwuzla = Value("bitwuzla")
  /** Boolector: is untested
   */
  val Boolector = Value("boolector")
  val Z3 = Value("z3")
  /** mathsat: is untested
   */
  val mathsat = Value("mathsat")
  val cvc4 = Value("cvc4")
  /** cvc5: is untested
   */
  val cvc5 = Value("cvc5")
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
    var modesWithDepths: LinkedHashMap[String, Int] = LinkedHashMap[String, Int](),
    var timeout: Option[Int] = None,
    var multiClock: Boolean = false,
    var toplevelName: String = null,
    var workspacePath: String = null,
    var workspaceName: String = null,
    var keepDebugInfo: Boolean = false,
    var skipWireReduce: Boolean = false
)

class SymbiYosysBackend(val config: SymbiYosysBackendConfig) extends FormalBackend {
  val workspaceName = new File(config.workspaceName).toPath()
  val workspacePath = new File(config.workspacePath).toPath()
  val workDir = workspaceName.resolve(workspacePath)
  val sbyFileName = s"${config.toplevelName}.sby"
  val sbyFilePath = workDir.resolve(sbyFileName)

  def clean(): Unit = {
    FileUtils.deleteQuietly(workDir.toFile())
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
    val skipWireReduce: String = if (config.skipWireReduce) "-ifx" else ""

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
      s"prep ${skipWireReduce} -top ${config.toplevelName}\n" +
      "\n" +
      "[files]\n" +
      localSources

    workDir.toFile.mkdir()
    Files.write(
      sbyFilePath,
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
    val success = Process(Seq(command, "-f", sbyFilePath.toString()), workspacePath.toFile()).!(new Logger()) == 0
    if(!success){


      var assertedLines = ArrayBuffer[String]()
      def analyseLog(file : File): Unit ={
        if(file.exists()){
          val pattern = """(\w+.sv):(\d+).(\d+)-(\d+).(\d+)""".r
          for (line <- Source.fromFile(file).getLines) {
            println(line)
            if(line.contains("Assert failed in") || line.contains("Unreached cover statement")){
              pattern.findFirstMatchIn(line) match {
                case Some(x) => {
                  val sourceName = x.group(1)
                  val assertLine = x.group(2).toInt
                  val source = Source.fromFile(workspacePath.resolve(Paths.get("rtl", sourceName)).toFile()).getLines.drop(assertLine)
                  val assertString = source.next().dropWhile(_ == ' ')
                  assertedLines += assertString
                  println("--   " +assertString)
                }
                case None =>
              }
            }
          }
        }
      }
      for((name, depth) <- config.modesWithDepths) {
        val logFileName = name match {
          case "bmc" => Seq("logfile.txt")
          case "prove" => Seq("logfile_basecase.txt", "logfile_induction.txt")
          case "cover" => Seq("logfile.txt")
        }
        for (e <- logFileName) analyseLog(workDir.resolve(Paths.get(s"${config.toplevelName}_${name}", "engine_0", e)).toFile())
      }

      val assertedLinesFormated = assertedLines.map{l =>
        val splits = l.split("// ")
        "(" + splits(1).replace(":L", ":") + ")  "
      }
      val assertsReport = assertedLinesFormated.map(e => s"\tissue.triggered.from${e}\n").mkString("")
      val proofAt = "\tproof in " + workDir + s"/${config.toplevelName}_${config.modesWithDepths.head._1}/engine_0"
      throw new Exception("SymbiYosys failure\n" + assertsReport + proofAt)
    }
    if (!config.keepDebugInfo) clean()
  }

  def checks(): Unit = {
    config.modesWithDepths.keySet.map(x => SbyMode.withName(x))
  }

  checks()
  genSby()
}
