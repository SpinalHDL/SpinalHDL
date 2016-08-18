package spinal.lib.com.uart

import spinal.core._
import spinal.lib._


case class Uart() extends Bundle with IMasterSlave {
  val txd = Bool
  val rxd = Bool

  override def asMaster(): Unit = {
    out(txd)
    in(rxd)
  }
}


object UartParityType extends SpinalEnum(binarySequancial) {
  val NONE, EVEN, ODD = newElement()
}


object UartStopType extends SpinalEnum(binarySequancial) {
  val ONE, TWO = newElement()
  def toBitCount(that : C) : UInt = (that === ONE) ? U"0" | U"1"
}
