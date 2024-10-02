package spinal.lib.com.eth

import spinal.core._
import spinal.lib._


case class Gmii() extends Bundle with IMasterSlave {
  val tx = GmiiTx()
  val rx = GmiiRx()

  override def asMaster(): Unit = {
    master(tx)
    slave(rx)
  }
}

case class GmiiTx() extends Bundle with IMasterSlave {
  val clk = Bits(2 bits)
  val d = Bits(8 bits)
  val ctl = Bits(2 bits)

  override def asMaster(): Unit = {
    out(d, ctl, clk)
  }

  def fromTxStream() = new Area {
    val interframe = MacTxInterFrame(8)
    val o = interframe.io.output
    val EN = RegNext(o.valid)
    val ER = False
    val data = RegNext(o.data)
//    val ER = RegNext(o.valid && o.error)
//    val data = RegNext(o.error.mux[Bits](B"x0F", o.data))
    d := data
    ctl(0) := EN
    ctl(1) := EN ^ ER
    clk(0) := True
    clk(1) := False
    val input = interframe.io.input
  }
}

case class GmiiRx() extends Bundle with IMasterSlave {
//  val D = Bits(8 bits)
//  val CRS_DV = Bool()
//  val ER = p.withEr generate Bool()
//
  override def asMaster(): Unit = {
//    out(D, CRS_DV)
//    outWithNull(ER)
  }
//
//  def toRxFlow() = {
//    val ret = Flow(Fragment(PhyRx(p.dataWidth)))
//    val s1 = Flow(PhyRx(p.dataWidth))
//    val s2 = RegNext(s1)
//    val s3 = RegNext(s2)
//
//    s1.data := RegNext(D)
//    s1.valid := RegNext(CRS_DV)
//    if (p.withEr) {
//      s1.error := RegNext(ER)
//    } else {
//      s1.error := Bool(false)
//    }
//
//    ret.fragment := s3.payload
//    ret.valid := s2.valid || s3.valid
//    ret.last := !s1.valid && !s2.valid && s3.valid
//
//    ret
//  }
//
//  def simReceive(frame : Seq[Int], cd : ClockDomain): Unit ={
//    import spinal.core.sim._
//    def nibble(value : Int): Unit ={
//      D   #= value
//      CRS_DV #= true
//      ER  #= false
//      cd.waitSampling()
//      CRS_DV  #= false
//    }
//    for(byte <- frame){
//      nibble(byte & 0xF)
//      nibble((byte >> 4) & 0xF)
//    }
//    cd.waitSampling(6)
//  }
}