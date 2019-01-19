package spinal.lib.eda.yosys

object Mode extends Enumeration{
  val bmc: String = ""
  val cover: String = "-c"
  val prove: String = "-i"
  val live: String = "-g"
}

object Solver extends Enumeration{
  val z3: String = "z3"
  val boolector: String = "boolector"
  val cvc4: String = "cvc4"
  val yices: String = "yices"
  val abc: String = "abc"
  val mathsat: String = "mathsat"
}

case class FormalCommand( smt2: String,
                          top: String = "",
                          solver: String = Solver.abc,
                          step: Int = 20,
                          skip: Int = 0,
                          stepSize: Int = 1,
                          mode: String = Mode.bmc,
                          dumpVCD: String = "",
                          dumpVerilogTB: String = "",
                          dumpSmtc: String = "",
                          append: Int = 0,
                          opt: String = "",
                          workDir: String=".") extends Executable{

  override val logFile = s"yosys-{solver}.log"
  def dumpTrace(path: String) = this.copy(dumpVCD = path)
  def dumpVerilogTestBench(path: String) = this.copy(dumpVerilogTB = path)
  def use(solver: String) = this.copy(solver = solver)
  def step(step: Int,skip: Int = 0, stepSize: Int = 1) = this.copy(step = step,skip = skip, stepSize = stepSize)
  def bmc   = this.copy(mode = Mode.bmc)
  def cover = this.copy(mode = Mode.cover)
  def prove = this.copy(mode = Mode.prove)
  def live  = this.copy(mode = Mode.live)

  override def toString(): String = {
    val ret = new StringBuilder("yosys-smtbmc ")
                                ret.append(s"-s ${solver} ")
                                ret.append("--presat --unroll --noprogress ")
                                ret.append(s"-t ${skip}:${stepSize}:${step} ")
                                ret.append(s"-t ${step} ")
                                ret.append(s"${mode} ")
                                ret.append(s"--append ${append} ")
    if(top.nonEmpty)            ret.append(s"-m ${top} ")
    if(dumpSmtc.nonEmpty)       ret.append(s"--dump-smtc ${dumpSmtc} ")
    if(dumpVCD.nonEmpty)        ret.append(s"--dump-vcd ${dumpVCD} ")
    if(dumpVerilogTB.nonEmpty)  ret.append("--dump-vlogtb ${dumpVerilogTB} ")
    if(opt.nonEmpty)            ret.append(s"-S ${opt} ")
                                ret.append(smt2)

    ret.toString()
  }
}

