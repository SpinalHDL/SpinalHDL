package spinal.lib.eda.yosys

import java.io.File
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._

trait YosysScript
case class Command(command: String, opt: String*) extends YosysScript {
  override def toString(): String = (command +: opt).mkString(" ").toString
}

case class Input(file: String, frontend: String, opt: String*)
    extends YosysScript {
  override def toString(): String =
    (("read_" + frontend + " " + file) +: opt).mkString(" ").toString
}

case class Output(file: String, backend: String, opt: String*)
    extends YosysScript {
  override def toString(): String =
    (("write_" + backend + " " + file) +: opt).mkString(" ").toString
}

case class Yosys(scriptName: String = "yosys.sy", workDir: String = ".")
    extends Executable
    with Makable {
  val commands = mutable.ListBuffer[YosysScript]()
  override val logFile = "yosys.log"
  override def runComand = {
    assert(
      commands(0).isInstanceOf[Input],
      "You must start with an Input directive"
    )
    save(scriptName)
    s"yosys -s ${scriptName}"
  }
  override def target: String = {
    val ret = commands.collect { case o: Output => o }
    assert(
      ret.length == 1,
      "Yosys: You must have one Output directive"
    )
    ret(0).file
  }

  // override def prerequisite: Seq[String] = {
  //   val ret = commands.collect { case out: Input => out }
  //   ret.map(_.file)
  // }
  def addCommand(command: Command*): Unit = commands ++ command
  def addCommand(command: String, opt: String*): Unit =
    commands += Command(command, opt: _*)
  def addInputFile(command: Input*) = commands ++ command
  def addInputFile(file: String, frontend: String, opt: String*): Unit =
    commands += Input(file, frontend, opt: _*)
  def addOutputFile(command: Output*) = commands ++ command
  def addOutputFile(file: String, backend: String, opt: String*): Unit =
    commands += Output(file, backend, opt: _*)

  def workPath(path: String) = this.copy(workDir = path)

  def append(yosys: Yosys*): Unit = commands ++= yosys.flatMap(_.commands)
  def +(yosys: Yosys): Yosys = {
    this.append(yosys)
    this
  }

  def save(file: String = "yosys.sy") = {
    val sy = new PrintWriter(new File(workDir, file))
    commands.foreach(c => sy.println(c.toString))
    sy.close()
  }

  override def toString(): String =
    commands.mkString("yosys -p'", "; ", "'") //I want to belive
}
