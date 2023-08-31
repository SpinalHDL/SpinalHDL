package spinal.lib.tools

import spinal.core._
import spinal.lib.{StreamFifoCC, master, slave}
import spinal.tester.SpinalAnyFunSuite


class HDElkDiagramGenTesterA_MYSub0(cd: ClockDomain) extends Component {
  val io = new Bundle{
    val ai = in UInt(8 bits)
    val ao = out UInt(8 bits)
  }
  io.ao := RegNext(io.ai) init(0)
}

class HDElkDiagramGenTesterA_MYSub1(cd: ClockDomain) extends Component {
  val io = new Bundle{
    val ai = in UInt(8 bits)
    val ao = out UInt(8 bits)
    val a2 = out UInt(8 bits)
  }
  io.ao := RegNext(io.ai) init(0)
  val cd2 = ClockDomain.external("adc")
  //alow another clockDomain not confict to default clockdomain
  val area = new ClockingArea(cd2){
    val tmp = RegNext(io.ai) init(0)
    val tmp2 = tmp + (RegNext(io.ai) init(0))
  }
  io.a2 := area.tmp2
}
class HDElkDiagramGenTesterA extends Component {
  val io = new Bundle{
    val a = in UInt(8 bits)
    val b0 = out UInt(8 bits)
    val b1 = out UInt(8 bits)
    val b2 = out UInt(8 bits)
  }
  val cd0 = ClockDomain.external("cp")
  val cd1 = ClockDomain.external("ap")
  val u_sub0 = cd0(new HDElkDiagramGenTesterA_MYSub0(cd0))
  val u_sub1 = cd1(new HDElkDiagramGenTesterA_MYSub1(cd1))
  u_sub0.io.ai := io.a
  u_sub1.io.ai := io.a
  io.b0 := u_sub0.io.ao
  io.b1 := u_sub1.io.ao
  io.b2 := u_sub1.io.a2
  val tmp = RegNext(io.a) init(0)
}


case class HDElkDiagramGenTesterB_FilterConfig(iqWidth: Int,
                                               tapNumbers: Int = 33,
                                               hwFreq: HertzNumber = 200 MHz,
                                               sampleFreq: HertzNumber = 1.92 MHz)

class HDElkDiagramGenTesterB(fc: HDElkDiagramGenTesterB_FilterConfig) extends Component{
  val din   = slave Flow(Bits(32 bits))
  val dout  = master Flow(Bits(32 bits))
  val flush = in Bool()

  val clockSMP = ClockDomain.external("smp")
  val clockHW = ClockDomain.external("hw")

  val u_fifo_in = StreamFifoCC(
    dataType = Bits(32 bits),
    depth = 8,
    pushClock = clockSMP,
    popClock = clockDomain
  )

  u_fifo_in.io.push << din.toStream
  dout << u_fifo_in.io.pop.toFlow

}
class HDElkDiagramGenTester extends SpinalAnyFunSuite{
  import spinal.lib.tools.HDElkDiagramGen
  test("test HDElkDiagramGenTesterA"){
    HDElkDiagramGen(SpinalVerilog(new HDElkDiagramGenTesterA))
  }
  test("test HDElkDiagramGenTesterB"){
    val fc = HDElkDiagramGenTesterB_FilterConfig(8)
    val rtl=SpinalVerilog(new HDElkDiagramGenTesterB(fc))
    HDElkDiagramGen(rtl)
  }
}
