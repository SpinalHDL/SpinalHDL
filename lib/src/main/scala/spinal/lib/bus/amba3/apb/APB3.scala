package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._

case class Apb3Config(
  addressWidth: Int,
  dataWidth: Int,
  selWidth : Int,
  useSlaveError : Boolean
)

case class Apb3(val c: Apb3Config) extends Bundle with IMasterSlave {
  val PADDR    = UInt(c.addressWidth bit)
  val PSEL    = Bits(c.selWidth bits)
  val PENABLE = Bool
  val PREADY  = Bool
  val PWRITE  = Bool
  val PWDATA  = Bits(c.dataWidth bit)
  val PRDATA  = Bits(c.dataWidth bit)
  val PSLVERROR  = if(c.useSlaveError) Bool else null
  override def asMaster(): Apb3.this.type = {
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
    if(c.useSlaveError) in(PSLVERROR)
    this
  }
}