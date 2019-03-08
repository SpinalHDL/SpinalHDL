package spinal.lib.eda.yosys

import java.io.File
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import spinal.core._

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

object Yosys{
  def load_SystemVerilog[T <: Component](report: SpinalReport[T]): Yosys = {
    val command = Yosys()
    report.rtlSourcesPaths.foreach(command.addInputFile(_,"verilog", "-sv"))
    command + setTop(report.toplevelName)
}

  def setTop(top: String): Yosys = {
      val command = Yosys()
      command.addCommand("prep", "-top", top)
      command
  }

  def load_verilog(): Yosys = {
      val command = Yosys()
      val verilogFiles = command.getAllPrerequisiteFromName(".*.v")
      verilogFiles.foreach(command.addInputFile(_,"verilog"))
      command
  }

  def export_smt2(file: String = "model.smt2"): Yosys = {
      val ret = Yosys()
      ret.addOutputFile(file,"smt2")
      ret
  }

  def model(mode: String, multiclock: Boolean = false,memoryMap: Boolean= false): Yosys = {
    val modelGen = Yosys()
    if(memoryMap) modelGen.addCommand("memory_map") else modelGen.addCommand("memory_nordff")
    modelGen.addCommand("memory_nordff")
    if (multiclock) {
    modelGen.addCommand("clk2fflogic")
    } else {
    modelGen.addCommand("async2sync")
    modelGen.addCommand("chformal", "-assume", "-early")
    }
    if (mode == Mode.bmc || mode == Mode.prove)
    modelGen.addCommand("chformal", "-live", "-fair", "-cover", "-remove")
    if (mode == Mode.cover)
    modelGen.addCommand("chformal", "-live", "-fair", "-remove")
    if (mode == Mode.live) {
    modelGen.addCommand("chformal", "-assert2assume")
    modelGen.addCommand("chformal", "-cover", "-remove")
    }
    modelGen.addCommand("opt_clean")
    modelGen.addCommand("setundef", "-anyseq")
    modelGen.addCommand("opt", "-keepdc", "-fast")
    modelGen.addCommand("check")
    modelGen.addCommand("hierarchy", "-simcheck")
    // modelGen.commands.foreach(_.getClass.toString)
    modelGen
  }

  def synthesize(target: String): Yosys = {
    val command = Yosys()
    command.addCommand("synth_" + target)
    command.addOutputFile("syntetized.json", "json")
    command
  }

  def formal[T <: Component](report: SpinalReport[T],mode: String = Mode.bmc, multiclock: Boolean = false,memoryMap: Boolean= false) = load_SystemVerilog(report)+ model(mode,multiclock,memoryMap) + export_smt2(report.toplevelName + ".smt2")
}

case class Yosys(scriptName: String = "yosys.sy", workDir: String = ".")
    extends Executable
    with Makeable {
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

  // override def prerequisite: Seq[String] = {
  //   val ret = commands.collect { case out: Input => out }
  //   ret.map(_.file)
  // }
  def addCommand(command: Command*): Unit = commands ++= command
  def addCommand(command: String, opt: String*): Unit =
    commands += Command(command, opt: _*)
  def addInputFile(command: Input*) = commands ++= command
  def addInputFile(file: String, frontend: String, opt: String*): Unit =
    commands += Input(file, frontend, opt: _*)
  def addOutputFile(command: Output*) = commands ++= command
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

  override def toString(): String = commands.mkString("yosys -p'", "; ", "'")

  def target = commands.collect{ case o: Output => o }.map(_.file)
  override def needs = commands.collect{case i: Input => i}.map(_.file)

}
