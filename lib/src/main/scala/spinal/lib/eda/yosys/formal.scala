package spinal.lib.eda.yosys

import scala.collection._
import java.nio.file.{Path, Paths}
import org.apache.commons.io.FilenameUtils

object Mode extends Enumeration {
  val bmc: String = ""
  val cover: String = "-c"
  val prove: String = "-i"
  val live: String = "-g"
}

object Solver extends Enumeration {
  val z3: String = "z3"
  val boolector: String = "boolector"
  val cvc4: String = "cvc4"
  val yices: String = "yices"
  val abc: String = "abc"
  val mathsat: String = "mathsat"
}

/** Create the command line string for formal verification (yosys-smtbmc) */
case class FormalCommand(_smt2: Option[Path]=None,
                         _top: String = "",
                         _solver: String = Solver.abc,
                         _step: Long = 20,
                         _skip: Long = 0,
                         _stepSize: Long = 1,
                         _mode: String = Mode.bmc,
                         _dumpVCD: Option[Path] = None,
                         _dumpVerilogTB: Option[Path] = None,
                         _dumpSmtc: Option[Path] = None,
                         _append: Long = 0,
                         _opt: String = "",
                         passFile: Option[Path] = None,
                         logFile: Option[Path] = None,
                         phony: Option[String] = None,
                         prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]())
    extends Makeable with MakeablePhony with MakeableLog with PassFail{

  /** Specify the smt2 input file
    *
    * @param path the path were the smt2 model reside
    */
  def smt2(path: Path) = this.copy(_smt2 = Some(path))

  /** Specify the top-level module
    *
    * @param top the name of the top level module
    */
  def top(top: String) = this.copy(_top = top)

  /** Specify the sat solver to use
    * deafult: abc
    *
    * @param solver the name of the solver to use
    */
  def solver(solver: String) = this.copy(_solver = solver)

  /** Specify the path where to save the vcd trace
    * default: not generate
    *
    * @param path the path where to save the vcd trace
    */
  def dumpVCD(path: Path) = this.copy(_dumpVCD = Some(path))

  /** Specify the path where to save the verilog testbench
    * default: not generate
    *
    * @param path the path where to save the verilog testbench
    */
  def dumpVerilogTB(path: Path) = this.copy(_dumpVerilogTB = Some(path))

  /** Specify the path where to save the smtc model
    * default: not generate
    *
    * @param path he path where to save the smtc model
    */
  def dumpSMTC(path: Path) = this.copy(_dumpSmtc = Some(path))

  /** Specify for how many step to check the model
    * default: step = 20, skip = 0, stepSize = 1
    *
    * @param step the number of step to check
    * @param skip the number of the to ignore before checking
    * @param stepSize the number of how many step ignore after a check
    */
  def step(step: Long, skip: Long = 0, stepSize: Long = 1) =
    this.copy(_step = step, _skip = skip, _stepSize = stepSize)

  /** Specify how many step continue after the model is checked, usefull in cover mode
    * default: 0
    *
    * @param step the number of step to append
    */
  def append(step: Long) = this.copy(_append = step)

  /** Bounded model chawcking mode*/
  def bmc = this.copy(_mode = Mode.bmc)

  /** Cover mode*/
  def cover = this.copy(_mode = Mode.cover)

  /** Prove/K-induction mode*/
  def prove = this.copy(_mode = Mode.prove)

  /** Liveness check mode*/
  def live = this.copy(_mode = Mode.live)

  /** @inheritdoc */
  override def outputFolder(path: Path): FormalCommand ={
    val old = super.outputFolder(path).asInstanceOf[this.type]
    val newVcd  = if(_dumpVCD.nonEmpty)       Some(path.resolve(_dumpVCD.get)) else None
    val newTB   = if(_dumpVerilogTB.nonEmpty) Some(path.resolve(_dumpVerilogTB.get)) else None
    val newSMTC = if(_dumpSmtc.nonEmpty)      Some(path.resolve(_dumpSmtc.get)) else None
    // val newPass = if(passFile.nonEmpty)      Some(path.resolve(passFile.get)) else None
    // val newLog = if(passFile.nonEmpty)      Some(path.resolve(passFile.get)) else None
    old.copy(_dumpVCD=newVcd,_dumpVerilogTB=newTB,_dumpSmtc=newSMTC)
  }


  /** @inheritdoc */
  def phony(name: String): FormalCommand = this.copy(phony=Some(name))

  /** @inheritdoc */
  def log(file: Path = Paths.get(this.getClass.getSimpleName + ".log")): FormalCommand = this.copy(logFile=Some(file))

  /** @inheritdoc */
  def pass(file: Path = Paths.get("PASS")): FormalCommand = this.copy(passFile=Some(file))

  override def toString(): String = {
    val ret = new StringBuilder("yosys-smtbmc ")
    ret.append(s"-s ${_solver} ")
    ret.append("--presat --unroll --noprogress ")
    ret.append(s"-t ${_skip}:${_stepSize}:${_step} ")
    ret.append(s"-t ${_step} ")
    ret.append(s"${_mode} ")
    ret.append(s"--append ${_append} ")
    if (_top.nonEmpty) ret.append(s"-m ${_top} ")
    if (_dumpSmtc.isDefined) ret.append(s"--dump-smtc ${_dumpSmtc.get} ")
    if (_dumpVCD.isDefined) ret.append(s"--dump-vcd ${_dumpVCD.get} ")
    if (_dumpVerilogTB.isDefined) ret.append("--dump-vlogtb ${dumpVerilogTB.get} ")
    if (_opt.nonEmpty) ret.append(s"-S ${_opt} ")
    ret.append(_smt2.get)

    ret.toString()
  }

  //make stuff
  // override def target = super.target ++ List(_dumpVerilogTB,_dumpVCD,_dumpSmtc).flatten

  /** @inheritdoc */
  override def target = super.target ++ List(_dumpVerilogTB,_dumpSmtc).flatten

  /** @inheritdoc */
  override def needs = List("smt2")

  /** @inheritdoc */
  override def makeComand: String = this.smt2(getPrerequisiteFromExtension("smt2")).toString
}
