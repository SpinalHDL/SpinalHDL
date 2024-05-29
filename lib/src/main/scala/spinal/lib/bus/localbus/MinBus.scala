package spinal.lib.bus.localbus

import spinal.core._
import spinal.lib._

case class MinBusConfig(aw: Int,
                        dw: Int,
                        pw: Int = 4,
                        withStrb: Boolean = false
                       ){
  val sw  = dw >> 3
}

case class MinBus(c: MinBusConfig) extends Interface with IMasterSlave {
  val ce   = Bool()
  val wr   = Bool()
  val rdy  = Bool()
  val addr = UInt(c.aw bit)
  val wdat = Bits(c.dw bit)
  val strb = Bits(c.sw bit)
  val prot = Bits(c.pw bit)
  val rvld = Bool()
  val rdat = Bits(c.dw bit)

  override def asMaster(): Unit = {
    out(ce, wr, addr, wdat, strb, prot)
    in(rdy, rvld, rdat)
  }

  @modport
  def mst() = asMaster()

  @modport
  def slv() = asSlave()
}
