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


object UartParityType extends SpinalEnum(sequancial) {
  val NONE, EVEN, ODD = newElement()
}


object UartStopType extends SpinalEnum(sequancial) {
  val ONE, TWO = newElement()
  def toBitCount(that : T) : UInt = (that === ONE) ? U"0" | U"1"
}
