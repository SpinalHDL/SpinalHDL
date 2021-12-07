package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._


case class Axi4WriteOnly(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{

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

  def awValidPipe() : Axi4WriteOnly = {
    val sink = Axi4WriteOnly(config)
    sink.aw << this.aw.validPipe()
    sink.w  << this.w
    sink.b  >> this.b
    sink
  }

  def setIdle(): this.type = {
    this.writeCmd.setIdle()
    this.writeData.setIdle()
    this.writeRsp.setBlocked()
    this
  }

  def setBlocked(): this.type = {
    this.writeCmd.setBlocked()
    this.writeData.setBlocked()
    this.writeRsp.setIdle()
    this
  }

  def toAxi4(): Axi4 = {
    val ret = Axi4(config)
    this >> ret
  
    ret.readCmd.setIdle()
    ret.readRsp.setBlocked()

    ret
  }

  def toFullConfig(): Axi4WriteOnly = {
    val ret = Axi4WriteOnly(config.toFullConfig())
    ret << this
    ret
  }

  override def asMaster(): Unit = {
    master(aw, w)
    slave(b)
  }
}