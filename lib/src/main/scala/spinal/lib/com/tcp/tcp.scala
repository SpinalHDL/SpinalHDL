package spinal.lib.com.tcp

import spinal.core._
import spinal.lib._


//For internal event between RX and TX component
case class TcpRxToTx() extends Bundle {
  val close = Bool
  val open = Bool
  val miss = Bool
  val rxPtr = UInt(16 bit)
  val otherRxPtr = Flow(UInt(16 bit))
}

class TcpRx extends Component {
  val io = new Bundle {
    val rxToTx = out(TcpRxToTx())

    val inFrame = slave Stream Fragment(Bits(8 bit))    // PayloadLength / src address / Data
    val outFrame = master Stream(Bits(8 bit))           // User Data
  }
}


class TcpTx extends Component {
  val io = new Bundle {
    val rxToTx = in(TcpRxToTx())

    val inFrame = slave Stream(Bits(8 bit))            // User Data
    val outFrame = master Stream Fragment(Bits(8 bit)) // PayloadLength / dst address / Data
  }
}