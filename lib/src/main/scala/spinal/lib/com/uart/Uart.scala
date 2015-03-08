package spinal.lib.com.uart

import spinal._
import spinal.lib._


class Uart extends Bundle with Interface {
  val txd = Bool()
  val rxd = Bool()

  override def asMaster: this.type = {
    out(txd)
    in(rxd)
    this
  }
  override def asSlave: this.type = asMaster.flip
}


object UartStopType extends SpinalEnum {
  val eStop1bit, eStop2bit = Value

  val toBitCount = SpinalMap( //TODO better
    (()=> eStop1bit()) -> (() => UInt(0)),
    (()=> eStop2bit()) -> (() => UInt(1))
  )
}

object UartParityType extends SpinalEnum {
  val eParityNone, eParityEven, eParityOdd = Value
}

