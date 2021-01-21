package spinal.lib.com.pmod

import spinal.core._
import spinal.lib._

case class DIPSwitch() extends PMODBundle {
  override def asMaster() = this.asInput()
}

case class DIPSwitchCtrl(msbFirst: Boolean = true) extends Component {
  val io = new Bundle {
    val pins = master(DIPSwitch())
    val output = out(UInt(8 bits))
  }

  val bits =
    Cat(
      io.pins.pin1,
      io.pins.pin2,
      io.pins.pin3,
      io.pins.pin4,
      io.pins.pin7,
      io.pins.pin8,
      io.pins.pin9,
      io.pins.pin10
    )

  val value = if (msbFirst) {
    bits.asUInt
  } else {
    bits.asUInt.flip
  }

  io.output := value
}
