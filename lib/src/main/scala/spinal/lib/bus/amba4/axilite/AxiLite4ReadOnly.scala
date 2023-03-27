package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

case class AxiLite4ReadOnly(config: AxiLite4Config) extends Bundle with IMasterSlave {
  val ar = Stream(AxiLite4Ax(config))
  val r  = Stream(AxiLite4R(config))

  def readCmd   = ar
  def readRsp   = r

  def >>(that: AxiLite4): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: AxiLite4): Unit = that >> this

  def >>(that: AxiLite4ReadOnly): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: AxiLite4ReadOnly): Unit = that >> this

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }
}
