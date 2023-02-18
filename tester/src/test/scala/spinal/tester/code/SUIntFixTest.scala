package spinal.tester.code

import spinal.core._

class UIntFixTry extends Component {
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
  val tosint         = a.intoSInt
  val expands        = a.expand
  val fixto10_2      = a.fixTo(10 downto 2, RoundType.ROUNDTOINF)
  val fixto10_2r     = a.fixTo(10 downto 2, RoundType.ROUNDTOINF)
  val fixto10_2def   = a.fixTo(10 downto 2)
}

class SIntFixTry extends Component{
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
  val fixto7_0          = a.fixTo(7 downto 0, RoundType.ROUNDTOINF)
  val fixto7_2          = a.fixTo(7 downto 2, RoundType.ROUNDTOINF)
  val fixto5_0          = a.fixTo(5 downto 0, RoundType.ROUNDTOINF)
  val fixto5_2          = a.fixTo(5 downto 2, RoundType.ROUNDTOINF)
  val fixto5_2f         = a.fixTo(5 downto 2, RoundType.FLOOR)
  val fixto5_2sr        = a.fixTo(5 downto 2, RoundType.ROUNDUP)
  val fixto5_2def       = a.fixTo(5 downto 2)
}

class UIntFixTry2 extends Component {
  val a = in UInt (16 bits)
  val b = out(a.fixTo(13 downto 4,RoundType.CEIL))
  val c = out(a.fixTo(8 downto 1,RoundType.FLOOR))
  val c0 = a.floor(1)
  val c1 = out(c0.sat(7))
}

class SIntFixTry2 extends Component {
  val a = in SInt (16 bits)
  val b = out(a.fixTo(15 downto 0, RoundType.FLOOR))
  val c = out(a.sat(0))
  val d = out(a.symmetry)
}

class UIntFixRegression(roundType: RoundType) extends Component {
  val a = in UInt (16 bits)
  val b0 = out(a.fixTo(15 downto 0, roundType))
  val b1 = out(a.fixTo(18 downto 3, roundType))
  val b2 = out(a.fixTo(10 downto 1, roundType))
  val b3 = out(a.fixTo(3  downto 0, roundType))
  val b4 = out(a.fixTo(15 downto 3, roundType))
  val b5 = out(a.fixTo(23 downto 5, roundType))
  val b6 = out(a.fixTo(0  downto 0, roundType))
}

class SIntFixRegression(roundType: RoundType) extends Component {
  val a = in SInt (16 bits)
  val b0 = out(a.fixTo(15 downto 0, roundType))
  val b1 = out(a.fixTo(18 downto 3, roundType))
  val b2 = out(a.fixTo(10 downto 1, roundType))
  val b3 = out(a.fixTo(3  downto 0, roundType))
  val b4 = out(a.fixTo(15 downto 3, roundType))
  val b5 = out(a.fixTo(23 downto 5, roundType))
  val b6 = out(a.fixTo(1  downto 0, roundType))
}

object genRtlCode {
  def main(args: Array[String]) {
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new UIntFixTry2)
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new SIntFixTry2)
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new UIntFixTry)
    SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new SIntFixTry)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new UIntFixTry)
    SpinalConfig(mode = VHDL,    targetDirectory="tmp/").generate(new SIntFixTry)
    import spinal.core.RoundType._
    val roundList = List(CEIL,FLOOR,FLOORTOZERO,CEILTOINF,ROUNDUP,ROUNDDOWN,ROUNDTOZERO,ROUNDTOINF)
    for(roundType <- roundList){
      SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new UIntFixRegression(roundType))
      SpinalConfig(mode = Verilog, targetDirectory="tmp/").generate(new SIntFixRegression(roundType))
      println(s"gen: ${roundType}")
    }
  }
}
