package spinal.lib.com.pmod

import spinal.core._
import spinal.lib._

case class SnapOff() extends PMODBundle {
  override def build() = {
    in(pin4, pin9, pin10)
    out(pin1, pin2, pin3, pin7, pin8)
  }
}

case class SnapOffCtrl() extends Component {
  val io = new Bundle {
    val pins = pmod(SnapOff())
    val led1 = in(Bool)
    val led2 = in(Bool)
    val led3 = in(Bool)
    val led4 = in(Bool)
    val led5 = in(Bool)
    val button1 = out(Bool)
    val button2 = out(Bool)
    val button3 = out(Bool)
  }

  io.pins.pin1 := io.led2.asBits
  io.pins.pin2 := io.led3.asBits
  io.pins.pin3 := io.led5.asBits
  io.pins.pin7 := io.led1.asBits
  io.pins.pin8 := io.led4.asBits

  io.button1 := io.pins.pin9.as(Bool)
  io.button2 := io.pins.pin4.as(Bool)
  io.button3 := io.pins.pin10.as(Bool)
}
