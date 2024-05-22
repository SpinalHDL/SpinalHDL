package spinal.lib.bus.localbus

import spinal.core._
import spinal.lib._

case class mbusConfig(aw: Int, dw: Int)

case class MemBus(c: mbusConfig) extends Interface with IMasterSlave {
  val ce    = Bool()
  val wr    = Bool()
  val addr  = UInt(c.aw bit)
  val wdat  = Bits(c.dw bit)
  val rdat  = Bits(c.dw bit)

  tieGeneric(addr, addGeneric("AW", c.aw))
  tieGeneric(wdat, addGeneric("DW", c.dw))
  tieGeneric(rdat, addGeneric("DW", c.dw))

  override def asMaster(): Unit = {
    out(ce, wr, addr, wdat)
    in(rdat)
  }

  def << (sink: MemBus): Unit = {
    sink.ce   := this.ce
    sink.wr   := this.wr
    sink.addr := this.addr
    sink.wdat := this.wdat
    this.rdat := sink.rdat
  }

  def >> (sink: MemBus): Unit = {
    sink << this
  }

  @modport
  def slv = this.asSlave()

  @modport
  def mst = this.asMaster()
}
