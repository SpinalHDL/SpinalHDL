package spinal.tester.code

import spinal.core._

class UIntExtend extends Component{
  val a,b            = in UInt(16 bits)
  val addExpand      = a +! b
  val subExpand      = a -! a
  val sat4           = a.sat(4)
  val round2         = a.round(2)
  val roundnoExpand2 = a.roundNoExpand(2)
  val floor4         = a.floor(4)
  val trim4          = a.trim(4)
  val assint         = a.asSInt
  val tosint         = a.toSInt
  val expands        = a.expand
}

class SIntExtend extends Component{
  val a,b = in SInt(8 bits)
  val addExpand         = a +! b
  val subExpand         = a +! b
  val sat4              = a.sat(4)
  val symsat4           = a.satWithSym(4)
  val round5            = a.round(5)
  val roundnoExpand5    = a.roundNoExpand(5)
  val sround2           = a.sround(2)
  val sroundnoExpand2   = a.sroundNoExpand(2)
  val floor3            = a.floor(3)
  val trim4             = a.trim(4)
  val expands           = a.expand
  val abs               = a.abs
  val symabs            = a.absWithSym
  val sym               = a.symmetry
}

object getRtlCode {
  def main(args: Array[String]) {
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new UIntExtend)
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new SIntExtend)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new UIntExtend)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new SIntExtend)
  }
}
