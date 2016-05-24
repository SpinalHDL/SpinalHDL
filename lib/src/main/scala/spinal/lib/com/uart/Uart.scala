package spinal.lib.com.uart

import spinal.core._
import spinal.lib._


case class Uart() extends Bundle with IMasterSlave {
  val txd = Bool
  val rxd = Bool

  override def asMaster(): this.type = {
    out(txd)
    in(rxd)
    this
  }
}


object UartStopType extends SpinalEnum(sequancial) {
  val eStop1bit, eStop2bit = newElement()

  def toBitCount(that : SpinalEnumCraft[UartStopType.type]) = that.mux(
    eStop1bit -> U"0",
    eStop2bit -> U"1",
    default   -> U"0"
  )
}

object UartParityType extends SpinalEnum(sequancial) {
  val eParityNone, eParityEven, eParityOdd = newElement()
}

