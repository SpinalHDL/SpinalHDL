package spinal.lib.bus.localbus

import spinal.core._
import spinal.lib._

case class MemBusConfig(aw: Int, dw: Int)

case class MemBus(c: MemBusConfig) extends Interface with IMasterSlave {
  val ce    = Bool()
  val wr    = Bool()
  val addr  = UInt(c.aw bit)
  val wdat  = Bits(c.dw bit)
  val rdat  = Bits(c.dw bit)

  addGeneric("AW", c.aw)
  addGeneric("DW", c.dw)

  tieGeneric(addr, "AW")
  tieGeneric(wdat, "DW")
  tieGeneric(rdat, "DW")

  override def asMaster(): Unit = {
    out(ce, wr, addr, wdat)
    in(rdat)
  }

  def <<(that: MemBus): Unit = that >> this

  def >>(that: MemBus): Unit = {
    that.ce   := this.ce
    that.wr   := this.wr
    that.addr := this.addr
    that.wdat := this.wdat
    this.rdat := that.rdat
  }

  @modport
  def slv = this.asSlave()

  @modport
  def mst = this.asMaster()
}
