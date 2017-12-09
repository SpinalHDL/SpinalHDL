package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4ReadOnly(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{
  val ar = Stream(Axi4Ar(config))
  val r = Stream(Axi4R(config))

  def readCmd = ar
  def readRsp = r


  def <<(that : Axi4) : Unit = that >> this
  def >> (that : Axi4) : Unit = {
    this.readCmd drive that.readCmd
    that.readRsp drive this.readRsp
  }

  def <<(that : Axi4ReadOnly) : Unit = that >> this
  def >> (that : Axi4ReadOnly) : Unit = {
    this.readCmd drive that.readCmd
    that.readRsp drive this.readRsp
  }

  def arValidPipe() : Axi4ReadOnly = {
    val sink = Axi4ReadOnly(config)
    sink.ar << this.ar.validPipe()
    sink.r  >> this.r
    sink
  }


  def toFullConfig(): Axi4ReadOnly = {
    val ret = Axi4ReadOnly(config.toFullConfig())
    ret << this
    ret
  }

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }
}