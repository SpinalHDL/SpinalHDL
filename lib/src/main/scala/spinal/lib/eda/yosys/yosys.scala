package spinal.lib.eda.yosys

import java.io.File
import java.nio.file.{Path, Paths}
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import spinal.core._

import org.apache.commons.io.FilenameUtils

trait YosysScript

/** Class that rappresent a yosys command
  *
  * @param command  yosys command to execute
  * @param opt      the parameters for the yosys command
  */
case class Command(command: String, opt: String*) extends YosysScript {
  override def toString(): String = (command +: opt).mkString(" ")
}

/** Class that rappresent a yosys frontend command
  *
  * @param file the file name or path
  * @param frontend the frontend to use to interpreting the input file
  * @param opt the parameters for the yosys frontend
  */
case class Input(file: Path, frontend: String, opt: String*)
    extends YosysScript {
  override def toString(): String =
    s"""read_${frontend} ${opt.mkString(" ")} ${file.normalize.toString}"""
}

/** Class that rappresent a yosys backend command
  *
  * @param file the file name or path
  * @param backend the backend to use when writing the output file
  * @param opt the parameters for the yosys frontend
  */
case class Output(file: Path, backend: String, opt: String*)
    extends YosysScript {
  override def toString(): String =
    s"""write_${backend} ${opt.mkString(" ")} ${file.normalize.toString}"""

    def outputFolder(path: Path): Output = Output(file=path.resolve(file),backend,opt:_*)
}

object Yosys {
  def load_SystemVerilog[T <: Component](report: SpinalReport[T]): Yosys = {
    val command = Yosys()
    report.rtlSourcesPaths.foreach(x => command.addInputFile(Paths.get(x), "verilog", "-sv"))
    command + setTop(report.toplevelName)
  }

  def setTop(top: String): Yosys = {
    val command = Yosys()
    command.addCommand("prep", "-top", top)
    command
  }

  def load_verilog(): Yosys = {
    val command = Yosys()
    val verilogFiles = command.getAllPrerequisiteFromExtension(".*.v")
    verilogFiles.foreach(command.addInputFile(_, "verilog"))
    command
  }

  def export_smt2(file: String = "model.smt2"): Yosys = {
    val ret = Yosys()
    ret.addOutputFile(Paths.get(file), "smt2")
    ret
  }

  def model(mode: String,
            multiclock: Boolean = false,
            memoryMap: Boolean = false): Yosys = {
    val modelGen = Yosys()
    if (memoryMap) modelGen.addCommand("memory_map")
    else modelGen.addCommand("memory_nordff")
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
    modelGen
  }

  def synthesize(target: String): Yosys = {
    val command = Yosys()
    command.addCommand("synth_" + target)
    command.addOutputFile("syntetized.json", "json")
    command
  }

  def formal[T <: Component](report: SpinalReport[T],
                             mode: String = Mode.bmc,
                             multiclock: Boolean = false,
                             memoryMap: Boolean = false,
                             workDir: String = ".") = {
    load_SystemVerilog(report) +
      model(mode, multiclock, memoryMap) +
      export_smt2(report.toplevelName + ".smt2")
  }
}

/** A class used to define a yosys script
  * {{{
  * val input = Yosys().addInputFile("test.v", "verilog")
  * input.addCommand("prep", "-top", top)
  *
  * val show = Yosys().addCommand("show")
  *
  * val json = Yosys().addOutputFile("design.json","json")
  *
  * // you can combine yosys script togheder
  * val graph = input + show
  *
  * // you can change the output directory
  * val output = input + json
  * output.outputFolder("test/json")
  * //this will make so the file "design.json" will be saved in "test/json/design.json"
  * }}}
  */
case class Yosys( passFile: Option[Path] = None,
                  logFile: Option[Path] = None,
                  phony: Option[String] = None,
                  commands: mutable.ListBuffer[YosysScript] = mutable.ListBuffer[YosysScript](),
                  prerequisite: mutable.MutableList[Makeable] = mutable.MutableList[Makeable]())
    extends Makeable
    with MakeableLog with MakeablePhony with PassFail{

  // override def runComand = {
  //   assert(
  //     commands(0).isInstanceOf[Input],
  //     "You must start with an Input directive"
  //   )
  //   save(scriptName)
  //   s"yosys -s ${scriptName}"
  // }

  // override def prerequisite: Seq[String] = {
  //   val ret = commands.collect { case out: Input => out }
  //   ret.map(_.file)
  // }

  /** Add a yosys command to the command list
    *
    * @param command add a command to the command list
    */
  def addCommand(command: Command*): Unit = commands ++= command

  /** Add a yosys command to the command list
    *
    * @param command  yosys command to execute
    * @param opt      the parameters for the yosys command
    */
  def addCommand(command: String, opt: String*): Unit =
    commands += Command(command, opt: _*)

  /** Add a yosys frontend command to the command list
    *
    * @param command  add yosys frontend command to the command list
    */
  def addInputFile(command: Input*) = commands ++= command

  /** Add a yosys frontend command to the command list
    *
    * @param file the file name or path
    * @param frontend the frontend to use to interpreting the input file
    * @param opt the parameters for the yosys frontend
    */
  def addInputFile(file: Path, frontend: String, opt: String*): Unit =
    commands += Input(file, frontend, opt: _*)

  /** Add a yosys frontend command to the command list
    *
    * @param file the file name as a String
    * @param frontend the frontend to use to interpreting the input file
    * @param opt the parameters for the yosys frontend
    */
  def addInputFile(file: String, frontend: String, opt: String*): Unit =
    commands += Input(Paths.get(file), frontend, opt: _*)


  /** Add a yosys backend command to the command list
    *
    * @param command  add yosys backend command to the command list
    */
  def addOutputFile(command: Output*) = commands ++= command

  /** Add a yosys frontend command to the command list
    *
    * @param file the file name or path
    * @param backend the backend to use when writing the output file
    * @param opt the parameters for the yosys frontend
    */
  def addOutputFile(file: Path, backend: String, opt: String*): Unit =
    commands += Output(file, backend, opt: _*)

  /** Add a yosys backend command to the command list
    *
    * @param file the file name as a String
    * @param backend the backend to use when writing the output file
    * @param opt the parameters for the yosys backend
    */
  def addOutputFile(file: String, backend: String, opt: String*): Unit =
    commands += Output(Paths.get(file), backend, opt: _*)

  /** append a yosys command to this yosys istance, return this istance
    *
    * @param yosys the yosys command to append
    */
  def append(yosys: Yosys*): Unit = commands ++= yosys.flatMap(_.commands)

  /** append a yosys command to this yosys istance, return this istance
    *
    * @param yosys the yosys command to append
    */
  def +(yosys: Yosys): Yosys = {
    this.append(yosys)
    this
  }

  // def save(file: String = "yosys.sy") = {
  //   val sy = new PrintWriter(Paths.get(workDir, file).toFile)
  //   commands.foreach(c => sy.println(c.toString))
  //   sy.close()
  // }

    /** change the destination directory of all output file
    *
    * @param path the pat of the destination directory
    */
  def outputFolder(path: Path): Yosys ={
    val com = commands.map{ x =>
      x match{
       case o: Output => o.outputFolder(path)
       case _ => x
      }
    }
    this.copy(commands=com)
  }

  def phony(name: String): Yosys = this.copy(phony=Some(name))
  def log(file: Path = Paths.get(this.getClass.getSimpleName + ".log")): Yosys = this.copy(logFile=Some(file))
  def pass(file: Path = Paths.get("PASS")): Yosys = this.copy(passFile=Some(file))

  override def toString(): String = commands.mkString("yosys -p'", "; ", "'")

  //needed for Makable
  override def target = super.target ++ commands.collect { case o: Output => o }.map(_.file)
  override def needs = commands.collect { case i: Input => i }.map(x => FilenameUtils.getExtension(x.file.toString))
}
