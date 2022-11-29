package spinal.lib.eda.symbiflow

import spinal.lib.eda.common._
import java.io.File
import java.nio.file.{Path, Paths}
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import spinal.core._

import org.apache.commons.io.FilenameUtils

trait YosysScript

/** Class that represents a yosys command
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
                  logFile: Option[Path] = Some(Paths.get("yosys.log")),
                  phony: Option[String] = None,
                  _binaryPath: Path = Paths.get("yosys"),
                  commands: mutable.ListBuffer[YosysScript] = mutable.ListBuffer[YosysScript](),
                  _outputFolder: Path = Paths.get("."),
                  _copyPath: Path = Paths.get("source"),
                  prerequisite: mutable.MutableList[Makeable] = mutable.MutableList[Makeable]())
    extends Makeable
    with MakeableLog with PassFail{

    def copySourceTo(path: Path): Yosys = this.copy(_copyPath=path)
  // /** @inheritdoc */
  // def workDir(path: Path) = this.copy(workDirPath = path)

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

  /** change the destination directory of all output file
    *
    * @param path the pat of the destination directory
    */
  def outputFolder(path: Path): Yosys =this.copy(_outputFolder=path)

  /** @inheritdoc */
  def phony(name: String): Yosys = this.copy(phony=Some(name))

  /** @inheritdoc */
  def log(file: Path = Paths.get(this.getClass.getSimpleName + ".log")): Yosys = this.copy(logFile=Some(file))

  /** @inheritdoc */
  def pass(file: Path = Paths.get("PASS")): Yosys = this.copy(passFile=Some(file))

  /** @inheritdoc */
  def binaryPath(path: Path) = this.copy(_binaryPath=path)
  //override def toString(): String = commands.mkString("yosys -p'", "; ", "'")
  override def toString(): String = {
    val ret = scala.collection.mutable.ListBuffer[String]()
    commands foreach {
    case i: Input => ret += Input(_copyPath.resolve(i.file),i.frontend, i.opt :_*).toString //@TODO: resolve harcode sorce
    case o: Output => ret += Output(_outputFolder.resolve(o.file),o.backend, o.opt :_*).toString
    case a => ret += a.toString
    }
    ret.mkString("yosys -p'", "; ", "'")
  }

  //needed for Makable
  /** @inheritdoc */
  override def target = super.target ++ commands.collect { case o: Output => o }.map(_.file)

  /** @inheritdoc */
  override def needs = commands.collect { case i: Input => i }.map(x => FilenameUtils.getExtension(x.file.toString))
}
