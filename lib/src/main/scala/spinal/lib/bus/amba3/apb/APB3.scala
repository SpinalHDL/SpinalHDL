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
//  def apply(config: Apb3Config) = new Apb3(config)
  def apply( addressWidth: Int,
             dataWidth: Int) = {
    new Apb3(
      Apb3Config(
        addressWidth = addressWidth,
        dataWidth = dataWidth,
        selWidth = 1,
        useSlaveError = false
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
}