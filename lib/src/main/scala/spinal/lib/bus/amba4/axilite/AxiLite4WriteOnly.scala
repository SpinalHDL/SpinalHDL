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



  override def asMaster(): Unit = {
    master(aw,w)
    slave(b)
  }
}
