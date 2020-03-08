package spinal.lib.com.uart

import spinal.core._
import spinal.lib._


case class Uart(ctsGen : Boolean = false, rtsGen : Boolean = false) extends Bundle with IMasterSlave {
  val txd = Bool
  val rxd = Bool
  val cts = ctsGen generate Bool()
  val rts = rtsGen generate Bool()

  override def asMaster(): Unit = {
    out(txd)
    in(rxd)
    outWithNull(rts)
    inWithNull(cts)
  }
}


object UartParityType extends SpinalEnum(binarySequential) {
  val NONE, EVEN, ODD = newElement()
}


object UartStopType extends SpinalEnum(binarySequential) {
  val ONE, TWO = newElement()
  def toBitCount(that : C) : UInt = (that === ONE) ? U"0" | U"1"
}
