package spinal.lib.com.tcp

import spinal.core._
import spinal.lib._

//https://en.wikipedia.org/wiki/Transmission_Control_Protocol#/media/File:Tcp_state_diagram_fixed_new.svg
object TcpServerState extends SpinalEnum {
  val eClosed, eListen, eSynReceived, eEtablished, eCloseWait, eLastAck = newElement()
}


class TcpStateMachine extends Component {
  val io = new Bundle {

  }
}


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

    val inFrame = slave Stream Fragment(Bits(8 bit))
    // PayloadLength / src address / Data
    val outFrame = master Stream (Bits(8 bit)) // User Data
  }
}


class TcpTx extends Component {
  val io = new Bundle {
    val rxToTx = in(TcpRxToTx())

    val inFrame = slave Stream (Bits(8 bit))
    // User Data
    val outFrame = master Stream Fragment(Bits(8 bit)) // PayloadLength / dst address / Data
  }
}