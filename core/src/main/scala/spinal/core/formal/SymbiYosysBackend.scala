package spinal.core.formal

import java.io.{File, PrintWriter, BufferedInputStream, FileInputStream, FileFilter}
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import org.apache.commons.io.FileUtils

import scala.collection.mutable.{ArrayBuffer, HashMap}
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
    solver: SmtBmcSolver.SmtBmcSolver = SmtBmcSolver.Z3
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
    val modes: ArrayBuffer[String] = ArrayBuffer[String](),
    var depth: Int = 100,
    var timeout: Option[Int] = None,
    var multiClock: Boolean = false,
    var toplevelName: String = null,
    var workspacePath: String = null,
    var workspaceName: String = null
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
    val modeCmd = config.modes.map(x => s"mode $x").mkString("\n") + "\n"
    val script = "[options]\n" +
      modeCmd +
      s"depth ${config.depth}\n" +
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
    assert(
      Process(Seq(command, sbyFilePath), new File(workspacePath)).!(new Logger()) == 0,
      "SymbiYosys invocation failed"
    )
  }

  def checks(): Unit = {
    config.modes.map(x => SbyMode.withName(x))
  }

  clean()
  checks()
  genSby()
}
