
package spinal.tester.code


import spinal.core._
import spinal.debugger._
import spinal.lib._


object DebuggerTestCode {


  class DebuggerTopLevel extends Component {

    val io = new Bundle {
      val input = in UInt (4 bit)
      val output = out UInt (4 bit)


      val slavePort = slave Flow Fragment(Bits(8 bit))
      val masterPort = master Stream Fragment(Bits(8 bit))
      //      val uart = master(new Uart)
    }

    val subComponentA = new SubComponentA
    subComponentA.io.input := io.input
    io.output := subComponentA.io.output


    val logicAnalyser = LogicAnalyserBuilder()
      .setSampleCount(256)
      .exTrigger(io.input.msb)
      .exTrigger(io.input.lsb)
      .probe(subComponentA.internalA)
      .probe(subComponentA.internalB)
      .build



    io.slavePort >> logicAnalyser.io.slavePort
    io.masterPort << logicAnalyser.io.masterPort


    //    val uartCtrl = new UartCtrl()
    //    uartCtrl.io.clockDivider := 100
    //    uartCtrl.io.config.dataLength := 7
    //    uartCtrl.io.config.parity := UartParityType.eParityNone
    //    uartCtrl.io.config.stop := UartStopType.eStop1bit
    //    uartCtrl.io.uart <> io.uart
    //    uartCtrl.io.read.toFlowFragmentBits() >> logicAnalyser.io.packetSlave
    //    uartCtrl.io.write << logicAnalyser.io.packetMaster.toStreamBits()
  }


  class SubComponentA extends Component {
    val io = new Bundle {
      val input = in UInt (4 bit)
      val output = out UInt (4 bit)
    }

    val internalA = RegInit(U"4x0")
    internalA := internalA + io.input

    val internalB = RegNext(internalA)

    io.output := internalA
  }

  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new DebuggerTopLevel)
    println("DONE")
  }

}

