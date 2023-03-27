package spinal.lib.bus.amba4.axilite


import spinal.core._
import spinal.lib._

case class AxiLite4WriteOnly(config: AxiLite4Config) extends Bundle with IMasterSlave {
  val aw = Stream(AxiLite4Ax(config))
  val w  = Stream(AxiLite4W(config))
  val b  = Stream(AxiLite4B(config))


  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b

  def >>(that: AxiLite4): Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that: AxiLite4): Unit = that >> this

  def >>(that: AxiLite4WriteOnly): Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that: AxiLite4WriteOnly): Unit = that >> this

  override def asMaster(): Unit = {
    master(aw,w)
    slave(b)
  }
}
