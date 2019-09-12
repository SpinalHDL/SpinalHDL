package spinal.tester.code

import spinal.core._

class UIntExtend extends Component {
  val a, b = in UInt (16 bits)
  val c = UInt()
  c:= a * b
  val csat = c.sat(3)
  val addExpand      = a +^ b
  val subExpand      = a -^ a
  val addSated       = a +| b
  val subSated       = a -| a
  val sat4           = a.sat(4)
  val sat0           = out UInt()
  val sat18          = a.sat(-2)
  sat0 := a.sat(0)
  val round2         = a.round(2)
  val round0         = out(UInt())
  val round20        = a.ceil(-4)
  round0 := a.round(0)
  val roundcarry     = a.round(2, false)
  val ceil2          = a.ceil(2)
  val ceil0          = out UInt()
  val ceil19         = a.ceil(-3)
  ceil0 := a.ceil(0)
  val ceil2carry     = a.ceil(2,false)
  val floor4         = a.floor(4)
  val trim4          = a.trim(4)
  val assint         = a.asSInt
  val tosint         = a.toSInt
  val expands        = a.expand
  val fixto10_2      = a.fixTo(10 downto 2, RoundToInf)
  val fixto10_2r     = a.fixTo(10 downto 2, RoundToInf)
  val fixto10_2def   = a.fixTo(10 downto 2)
}

class SIntExtend extends Component{
  val a,b = in SInt(8 bits)
  val addExpand         = a +^ b
  val subExpand         = a -^ b
  val addsated          = a +| b
  val subsated          = a -| b
  val sat4              = a.sat(4)
  val sat10             = a.sat(-2)
  val sat0              = out SInt()
  sat0 := a.sat(0)
  val symsat4           = a.satWithSym(4)
  val round5            = a.round(5)
  val roundcarrary      = a.round(5,false)
  val round0            = out(SInt())
  val round10           = a.round(-2)
  round0 := a.round(0)
  val ceil2             = a.ceil(2)
  val ceil2carry        = a.ceil(2,false)
  val ceil0             = out SInt()
  ceil0 := a.ceil(0)
  val ceil10            = a.ceil(-2)
  val floor3            = a.floor(3)
  val trim4             = a.trim(4)
  val expands           = a.expand
  val abs               = a.abs
  val symabs            = a.absWithSym
  val sym               = a.symmetry
  val fixto7_0          = a.fixTo(7 downto 0, RoundToInf)
  val fixto7_2          = a.fixTo(7 downto 2, RoundToInf)
  val fixto5_0          = a.fixTo(5 downto 0, RoundToInf)
  val fixto5_2          = a.fixTo(5 downto 2, RoundToInf)
  val fixto5_2f         = a.fixTo(5 downto 2, Floor)
  val fixto5_2sr        = a.fixTo(5 downto 2,RoundUpp)
  val fixto5_2def       = a.fixTo(5 downto 2)
}

object getRtlCode {
  def main(args: Array[String]) {
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new UIntExtend)
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new SIntExtend)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new UIntExtend)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new SIntExtend)
  }
}
