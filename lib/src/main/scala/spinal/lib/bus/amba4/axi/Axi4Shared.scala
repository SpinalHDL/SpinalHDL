package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4Shared(config: Axi4Config) extends Bundle with IMasterSlave {
  val as = Stream(Axi4As(config))
  val w  = Stream(Axi4W(config))
  val b   = Stream(Axi4B(config))
  val r   = Stream(Axi4R(config))

  def sharedCmd = as
  def writeData = w
  def writeRsp = b
  def readRsp = r


  override def asMaster(): this.type = {
    master(as,w)
    slave(b,r)
    this
  }
}