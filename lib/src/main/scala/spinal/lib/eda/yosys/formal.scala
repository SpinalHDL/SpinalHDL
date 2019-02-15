package spinal.lib.eda.yosys

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

case class FormalCommand(_smt2: String="",
                         _top: String = "",
                         _solver: String = Solver.abc,
                         _step: Long = 20,
                         _skip: Long = 0,
                         _stepSize: Long = 1,
                         _mode: String = Mode.bmc,
                         _dumpVCD: String = "",
                         _dumpVerilogTB: String = "",
                         _dumpSmtc: String = "",
                         _append: Long = 0,
                         _opt: String = "",
                         workDir: String = ".")
    extends Executable with Makable{

  override val logFile = s"yosys-{solver}.log"
  def smt2(path: String) = this.copy(_smt2 = path)
  def top(path: String) = this.copy(_top = path)
  def solver(path: String) = this.copy(_solver = path)
  def dumpVCD(path: String) = this.copy(_dumpVCD = path)
  def dumpVerilogTB(path: String) = this.copy(_dumpVerilogTB = path)
  def dumpSMTC(path: String) = this.copy(_dumpSmtc = path)
  def step(step: Long, skip: Long = 0, stepSize: Long = 1) =
    this.copy(_step = step, _skip = skip, _stepSize = stepSize)
  def append(step: Long) = this.copy(_append = step)
  def bmc = this.copy(_mode = Mode.bmc)
  def cover = this.copy(_mode = Mode.cover)
  def prove = this.copy(_mode = Mode.prove)
  def live = this.copy(_mode = Mode.live)

  override def toString(): String = {
    val ret = new StringBuilder("yosys-smtbmc ")
    ret.append(s"-s ${_solver} ")
    ret.append("--presat --unroll --noprogress ")
    ret.append(s"-t ${_skip}:${_stepSize}:${_step} ")
    ret.append(s"-t ${_step} ")
    ret.append(s"${_mode} ")
    ret.append(s"--append ${_append} ")
    if (_top.nonEmpty) ret.append(s"-m ${_top} ")
    if (_dumpSmtc.nonEmpty) ret.append(s"--dump-smtc ${_dumpSmtc} ")
    if (_dumpVCD.nonEmpty) ret.append(s"--dump-vcd ${_dumpVCD} ")
    if (_dumpVerilogTB.nonEmpty) ret.append("--dump-vlogtb ${dumpVerilogTB} ")
    if (_opt.nonEmpty) ret.append(s"-S ${_opt} ")
    ret.append(_smt2)

    ret.toString()
  }

  //make stuff
  def target = if(_dumpVerilogTB.isEmpty) "formal" else _dumpVerilogTB
  override def makeComand: String =
    this.copy(_smt2 = getPrerequisiteFromName(".*.smt2")).toString
}
