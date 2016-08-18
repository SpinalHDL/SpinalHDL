package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

case class AxiLite4ReadOnly(config: AxiLite4Config) extends Bundle with IMasterSlave {
  val ar = Stream(AxiLite4Ax(config))
  val r  = Stream(AxiLite4R(config))

  def readCmd   = ar
  def readRsp   = r

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }
}
