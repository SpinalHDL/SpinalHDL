package spinal.lib.bus.amba4.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3Config

case class Apb4Config(addressWidth  : Int,
                      dataWidth     : Int,
                      selWidth      : Int = 1,
                      useStrb       : Boolean = true,
                      useSlaveError : Boolean = true
                     ){
  def strbWidth: Int = dataWidth>>3
}

object Apb4{
  def apply(addressWidth: Int, dataWidth: Int) = new Apb4(Apb4Config(addressWidth = addressWidth, dataWidth = dataWidth))
  def apply(c: Apb3Config): Apb4 = new Apb4(Apb4Config(c.addressWidth, c.dataWidth, c.selWidth, false, c.useSlaveError))
}

/**
  * AMBA 4 APB Protocol interface
  */
case class Apb4(c: Apb4Config) extends Bundle with IMasterSlave {
  val PADDR      = UInt(c.addressWidth bits)
  val PSEL       = Bits(c.selWidth bits)
  val PENABLE    = Bool()
  val PREADY     = Bool()
  val PWRITE     = Bool()
  val PSTRB      = Bits(c.strbWidth bits)
  val PPROT      = Bits(3 bits)
  val PWDATA     = Bits(c.dataWidth bits)
  val PRDATA     = Bits(c.dataWidth bits)
  val PSLVERR  = if(c.useSlaveError) Bool() else null

  override def asMaster(): Unit = {
    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA, PSTRB, PPROT)
    in(PREADY, PRDATA)
    if(c.useSlaveError) in(PSLVERR)
  }

  def << (that: Apb4): Unit = that >> this

  def >> (that: Apb4): Unit = {
    //    assert(this.config.addressWidth >= sink.config.addressWidth, "APB3 mismatch width address")
    assert(this.c.selWidth == that.c.selWidth, "APB4 mismatch sel width")

    that.PADDR   := this.PADDR.resized
    that.PSEL    := this.PSEL
    that.PENABLE := this.PENABLE
    this.PREADY  := that.PREADY
    that.PWRITE  := this.PWRITE
    that.PWDATA  := this.PWDATA
    that.PSTRB   := this.PSTRB
    that.PPROT   := this.PPROT
    this.PRDATA  := that.PRDATA

    if(PSLVERR != null) {
      this.PSLVERR := (if (that.PSLVERR != null) that.PSLVERR else False)
    }
  }

  def m2sPipe(): Apb4 ={
    val that = Apb4(c)
    that.PADDR    := RegNext(this.PADDR )
    that.PWRITE   := RegNext(this.PWRITE)
    that.PPROT    := RegNext(this.PPROT )
    that.PSTRB    := RegNext(this.PSTRB )
    that.PWDATA   := RegNext(this.PWDATA)
    that.PSEL     := RegNext(this.PREADY ? B(0) | this.PSEL)
    that.PENABLE  := RegNext(this.PENABLE && !this.PREADY)
    this.PRDATA   := that.PRDATA
    this.PREADY   := that.PREADY && that.PENABLE
    if(PSLVERR != null) { this.PSLVERR := that.PSLVERR }
    that
  }

  def crossClockDomainToggle(inClk: ClockDomain, outClk: ClockDomain): Apb4 = {
//    val cc = new Apb4CCToggle(this.c, inClk, outClk)
//    cc.io.input <> this
//    return cc.io.output
    this
  }
}
