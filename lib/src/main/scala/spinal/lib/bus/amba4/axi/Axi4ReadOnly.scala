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

  def setIdle(): this.type = {
    this.readCmd.setIdle()
    this.readRsp.setBlocked()
    this
  }

  def setBlocked(): this.type = {
    this.readCmd.setBlocked()
    this.readRsp.setIdle()
    this
  }

  def toAxi4(): Axi4 = {
    val ret = Axi4(config)
    this >> ret
  
    ret.writeCmd.setIdle()
    ret.writeData.setIdle()
    ret.writeRsp.setBlocked()

    ret
  }

  def toFullConfig(): Axi4ReadOnly = {
    val ret = Axi4ReadOnly(config.toFullConfig())
    ret << this
    ret
  }

  def pipelined(
    ar: StreamPipe = StreamPipe.NONE,
    r: StreamPipe = StreamPipe.NONE
  ): Axi4ReadOnly = {
    val ret = cloneOf(this)
    ret.ar << this.ar.pipelined(ar)
    ret.r.pipelined(r) >> this.r
    ret
  }

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }

  def formalContext(maxBursts: Int = 16, maxStrbs: Int = 256, optimize: Boolean = true) = new Area {
    val addrChecker = ar.payload.formalContext()

    def withAsserts(maxStallCycles: Int = 0) = {
      ar.withAsserts()
      ar.withTimeoutAssumes(maxStallCycles)
      r.withAssumes()
      r.withTimeoutAsserts(maxStallCycles)

      when(ar.valid) {
        addrChecker.withAsserts()
      }    
    }

    def withAssumes(maxStallCycles: Int = 0) = {
      ar.withAssumes()
      ar.withTimeoutAsserts(maxStallCycles)
      r.withAsserts()
      r.withTimeoutAssumes(maxStallCycles)

      when(ar.valid) {
        addrChecker.withAssumes()
      }
    }

    def withCovers() = {
      ar.withCovers(2)
      when(ar.fire) {
        addrChecker.withCovers()
      }
      r.withCovers(2)
      when(r.fire) {
        r.payload.withCovers()
      }
    }
  }
}
