package spinal.lib.experimental.com.serial.UnderTest

import spinal.core._
import spinal.lib._

case class SerialSafeLayerParam(bitWidth: Int = 8,
                                magicCode: Int = 0x55)


object SerialSafeLayerRxState extends SpinalEnum {
  val eIdle,eData = newElement()
}

class SerialSafelLayerRx(p: SerialSafeLayerParam) extends Component {
  import p._
  val io = new Bundle {
    val lowLayer = slave Flow (Bits(bitWidth bit))
    val highLayer = master Stream Fragment(Bits(bitWidth bit))
  }
}


class SerialSafeLayerTx(p: SerialSafeLayerParam) extends Component {
  import p._
  val io = new Bundle {
    val highLayer = slave Stream Fragment(Bits(bitWidth bit))
    val lowLayer = master Stream (Bits(bitWidth bit))
  }
}