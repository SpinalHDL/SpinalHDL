package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._

case class Apb3Config(
  addressWidth: Int,
  dataWidth: Int,
  selWidth : Int = 1,
  useSlaveError : Boolean = true
)

object Apb3{
  def apply( addressWidth: Int,
             dataWidth: Int) = {
    new Apb3(
      Apb3Config(
        addressWidth = addressWidth,
        dataWidth = dataWidth
      )
    )
  }
}

case class Apb3(config: Apb3Config) extends Bundle with IMasterSlave {
  val PADDR      = UInt(config.addressWidth bit)
  val PSEL       = Bits(config.selWidth bits)
  val PENABLE    = Bool
  val PREADY     = Bool
  val PWRITE     = Bool 
  val PWDATA     = Bits(config.dataWidth bit)
  val PRDATA     = Bits(config.dataWidth bit)
  val PSLVERROR  = if(config.useSlaveError) Bool else null
  override def asMaster(): Unit = {
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
    if(config.useSlaveError) in(PSLVERROR)
  }

  def << (sink : Apb3) : Unit = sink >> this
  def >> (sink : Apb3): Unit ={
    assert(this.config.addressWidth >= sink.config.addressWidth)
    assert(this.config.selWidth == sink.config.selWidth)

    sink.PADDR := this.PADDR.resized
    sink.PSEL := this.PSEL
    sink.PENABLE := this.PENABLE
    this.PREADY := sink.PREADY
    sink.PWRITE := this.PWRITE
    sink.PWDATA := this.PWDATA
    this.PRDATA := sink.PRDATA
    if(PSLVERROR != null)
      this.PSLVERROR := (if(sink.PSLVERROR != null) sink.PSLVERROR else False)
  }
}