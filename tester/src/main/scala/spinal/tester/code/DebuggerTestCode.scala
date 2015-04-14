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
      val masterPort = master Handshake Fragment(Bits(8 bit))
      //      val uart = master(new Uart)
    }

    val subComponentA = new SubComponentA
    subComponentA.io.input := io.input
    io.output := subComponentA.io.output


    val logicAnalyserParameter = new LogicAnalyserParameter
    logicAnalyserParameter.probe(subComponentA.internalA)
    logicAnalyserParameter.probe(subComponentA.internalB)

    val logicAnalyser = new LogicAnalyser(logicAnalyserParameter)

    io.slavePort >> logicAnalyser.io.slavePort
    io.masterPort << logicAnalyser.io.masterPort


    //    val uartCtrl = new UartCtrl()
    //    uartCtrl.io.clockDivider := 100
    //    uartCtrl.io.config.dataLength := 7
    //    uartCtrl.io.config.parity := UartParityType.eParityNone
    //    uartCtrl.io.config.stop := UartStopType.eStop1bit
    //    uartCtrl.io.uart <> io.uart
    //    uartCtrl.io.read.toFlowFragmentBits() >> logicAnalyser.io.packetSlave
    //    uartCtrl.io.write << logicAnalyser.io.packetMaster.toHandshakeBits()
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

