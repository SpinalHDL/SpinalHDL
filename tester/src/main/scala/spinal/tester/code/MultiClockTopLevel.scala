package spinal.tester.code

import spinal.core._
import spinal.lib._

//Define custom data types
class MyDataType extends Bundle{
  val a = UInt(8 bit)
  val b = Bool
}

class MultiClockTopLevel extends Component {
  val io = new Bundle {
    val clkA = in Bool
    val resetA = in Bool
    val clkB = in Bool
    val resetB = in Bool

    //Create stream interface (valid, ready, data)
    val slaveInteface = slave Stream(new MyDataType)
    val masterInterface = master Stream(new MyDataType)
  }

  //Create clock domains from inputs clocks and resets
  val clockDomainA = ClockDomain(io.clkA,io.resetA)
  val clockDomainB = ClockDomain(io.clkB,io.resetB)

  //Create a fifo able to cross clock domain a stream of MyDataType
  val fifo = new StreamFifoCC(new MyDataType,16,clockDomainA,clockDomainB)
  fifo.io.push << io.slaveInteface    //Easy connection provided by Stream library
  fifo.io.pop >> io.masterInterface
}


object MultiClockTopLevel {
  def main(args: Array[String]) {
    SpinalVhdl(new MultiClockTopLevel)
  }
}