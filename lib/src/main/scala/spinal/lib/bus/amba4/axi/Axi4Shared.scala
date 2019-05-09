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


  def arwValidPipe() : Axi4Shared = {
    val sink = Axi4Shared(config)
    sink.arw << this.arw.validPipe()
    sink.w  << this.w
    sink.r  >> this.r
    sink.b  >> this.b
    sink
  }

  def toAxi4() : Axi4 = {
    val ret = Axi4(config)
    ret.ar.payload.assignSomeByName(this.arw.payload)
    ret.aw.payload.assignSomeByName(this.arw.payload)
    ret.ar.valid := this.arw.valid && !this.arw.write
    ret.aw.valid := this.arw.valid && this.arw.write
    this.arw.ready := this.arw.write ? ret.aw.ready | ret.ar.ready
    this.w >> ret.w
    this.r << ret.r
    this.b << ret.b
    ret
  }

  def toAxi4ReadOnly() : Axi4ReadOnly = {
    val ret = Axi4ReadOnly(config)
    ret.ar.payload.assignSomeByName(this.arw.payload)
    ret.ar.valid := this.arw.valid
    this.arw.ready := ret.ar.ready
    this.r << ret.r
    ret
  }

  def toAxi4WriteOnly() : Axi4WriteOnly = {
    val ret = Axi4WriteOnly(config)
    ret.aw.payload.assignSomeByName(this.arw.payload)
    ret.aw.valid := this.arw.valid && this.arw.write
    this.arw.ready := ret.aw.ready
    this.w >> ret.w
    this.b << ret.b
    ret
  }


  def toFullConfig(): Axi4Shared = {
    val ret = Axi4Shared(config.toFullConfig())
    ret << this
    ret
  }

  override def asMaster(): Unit = {
    master(arw,w)
    slave(b,r)
  }
}