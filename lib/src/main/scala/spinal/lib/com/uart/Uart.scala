package spinal.lib.com.uart

import spinal.core._
import spinal.lib._

object Uart{
  def apply() = new Uart()
}
class Uart extends Bundle with IMasterSlave {
  val txd = Bool
  val rxd = Bool

  override def asMaster(): this.type = {
    out(txd)
    in(rxd)
    this
  }
  override def asSlave(): this.type = asMaster().flip()
}


object UartStopType extends SpinalEnum(sequancial) {
  val eStop1bit, eStop2bit = newElement()

  def toBitCount(that : SpinalEnumCraft[UartStopType.type]) = SpinalMap(that,U"0",eStop1bit -> U"0",eStop2bit -> U"1")
//  val toBitCount = SpinalMap(
//    (()=> eStop1bit()) -> (() => U(0)),
//    (()=> eStop2bit()) -> (() => U(1))
//  )
}

object UartParityType extends SpinalEnum(sequancial) {
  val eParityNone, eParityEven, eParityOdd = newElement()
}

