
package spinal.debugger

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._


object DebuggerOnUart {


  class DebuggerOnUart extends Component {
    val io = new Bundle {
      val conds = in Bits (4 bit)
      val input = in UInt (4 bit)
      val output = out UInt (4 bit)

      val uart = master(new Uart)
    }

    val input = BufferCC(io.input)
    val conds = BufferCC(io.conds)



    val subComponentA = new SubComponentA
    subComponentA.io.input := io.input
    io.output := subComponentA.io.output


    val condsParity = io.conds.asBools.reduceLeft(_ ^ _)

    val counter = CounterFreeRun(1024)

    val uartCtrl = new UartCtrl()
    uartCtrl.io.config.clockDivider := BigInt((50e6 / 57.6e3 / 8).toLong)
    uartCtrl.io.config.frame.dataLength := 7
    uartCtrl.io.config.frame.parity := UartParityType.NONE
    uartCtrl.io.config.frame.stop := UartStopType.ONE
    uartCtrl.io.uart <> io.uart

    val (uartFlowFragment, uartSoftReset) = uartCtrl.io.read.toFlow.toFlowFragmentBitsAndReset()

    val debugger = new ResetArea(uartSoftReset, false) {
      val logicAnalyser = LogicAnalyserBuilder()
        .setSampleCount(256)
        .exTrigger(conds.msb)
        .exTrigger(conds.lsb)
        .probe(conds)
        .probe(counter)
        .probe(input)
        .probe(subComponentA.internalA)
        .probe(condsParity)
        .probe(subComponentA.internalB)
        .build


      uartFlowFragment >> logicAnalyser.io.slavePort
      uartCtrl.io.write << logicAnalyser.io.masterPort.toStreamBits()
    }
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

  //TODO will generate a warning that is not nice in therm of lisibility
  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new DebuggerOnUart)
    println("DONE")
  }

}

