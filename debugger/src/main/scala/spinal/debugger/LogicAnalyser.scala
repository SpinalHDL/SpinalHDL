package spinal.debugger

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


object LogicAnalyser {

}

class LogicAnalyserParameter {
  var memAddressWidth = 8

  val dataList = ArrayBuffer[Data]()
  def probe(that: Data) {
    dataList += that
  }
}

class LogicAnalyser(p: LogicAnalyserParameter) extends Component {
  val io = new Bundle {
    val packetSlave = slave.Flow.Fragment(Bits(8 bit))
    val packetMaster = master Handshake Fragment(Bits(8 bit))
  }


  val trigger = new Area{
    val event = CounterFreeRun(16) === UInt(0)
  }
}