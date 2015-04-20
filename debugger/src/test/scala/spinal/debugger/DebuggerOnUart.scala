/* SpinalHDL
* Copyright (c) Dolu, All rights reserved.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 3.0 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library.
*/

package spinal.debugger

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._


object DebuggerOnUart {


  class DebuggerOnUart extends Component {
    val io = new Bundle {
      val input = in UInt (4 bit)
      val output = out UInt (4 bit)

      val uart = master(new Uart)
    }

    val subComponentA = new SubComponentA
    subComponentA.io.input := io.input
    io.output := subComponentA.io.output


    val uartCtrl = new UartCtrl()
    uartCtrl.io.clockDivider := BigInt((50e6 / 57.6e3 / 8).toLong)
    uartCtrl.io.config.dataLength := 7
    uartCtrl.io.config.parity := UartParityType.eParityNone
    uartCtrl.io.config.stop := UartStopType.eStop1bit
    uartCtrl.io.uart <> io.uart

    val (uartFlowFragment, uartSoftReset) = uartCtrl.io.read.toFlowFragmentBitsAndReset()


    val logicAnalyserParameter = new LogicAnalyserParameter(log2Up(256), Seq(
      ProbeAdd(subComponentA.internalA),
      ProbeAdd(subComponentA.internalB)
    ))

    val logicAnalyser = new LogicAnalyser(logicAnalyserParameter)

    uartFlowFragment >> logicAnalyser.io.slavePort
    uartCtrl.io.write << logicAnalyser.io.masterPort.toHandshakeBits()
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
    SpinalVhdl(new DebuggerOnUart)
    println("DONE")
  }

}

