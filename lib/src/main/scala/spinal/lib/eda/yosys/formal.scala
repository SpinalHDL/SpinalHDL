package spinal.lib.eda.yosys
import scala.collection.mutable._
import java.io.File
import java.io.PrintWriter
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import spinal.core._
import scala.sys.process._

object Mode extends Enumeration{
  val bmc: String = ""
  val cover: String = "-c"
  val prove: String = "-i"
  val live: String = "-g"
}

object Solver extends Enumeration{
  val z3: String = "z3"
}

case class FormalCommand( smt2: String,
                          top: String = "",
                          solver: String = Solver.z3,
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

  override val logFile = "yosys-smtbmc.log"
  def dumpTrace(path: String) = this.copy(dumpVCD = path)
  def dumpVerilogTestBench(path: String) = this.copy(dumpVerilogTB = path)
  def bmc = this.copy(mode = Mode.bmc)
  def cover = this.copy(mode = Mode.cover)
  def prove = this.copy(mode = Mode.prove)

  override def toString(): String = {
    val ret = new StringBuilder("yosys-smtbmc ")
                                ret.append(s"-s ${solver} ")
                                ret.append("--presat --noprogress ")
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

