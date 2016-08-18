package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4Shared(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{
  val arw = Stream(Axi4Arw(config))
  val w  = Stream(Axi4W(config))
  val b   = Stream(Axi4B(config))
  val r   = Stream(Axi4R(config))

  def sharedCmd = arw
  def writeData = w
  def writeRsp = b
  def readRsp = r


  def <<(that : Axi4Shared) : Unit = that >> this
  def >> (that : Axi4Shared) : Unit = {
    this.sharedCmd drive that.sharedCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
    that.readRsp drive this.readRsp
  }



  override def asMaster(): this.type = {
    master(arw,w)
    slave(b,r)
    this
  }
}