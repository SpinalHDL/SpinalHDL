package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._


case class Axi4WriteOnly(config: Axi4Config) extends Bundle with IMasterSlave {

  val aw = Stream(Axi4Aw(config))
  val w = Stream(Axi4W(config))
  val b = Stream(Axi4B(config))


  def writeCmd = aw
  def writeData = w
  def writeRsp = b

  def <<(that : Axi4) : Unit = that >> this
  def >> (that : Axi4) : Unit = {
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
  }

  def <<(that : Axi4WriteOnly) : Unit = that >> this
  def >> (that : Axi4WriteOnly) : Unit = {
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
  }


  override def asMaster(): this.type = {
    master(aw, w)
    slave(b)
    this
  }
}