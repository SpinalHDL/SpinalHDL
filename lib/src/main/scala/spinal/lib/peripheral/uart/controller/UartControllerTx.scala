/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */

package spinal.lib.peripheral.uart.controller

import spinal.core._
import spinal.lib._
import spinal.lib.peripheral.uart.UartStopType._
import spinal.lib.{Counter, slave}
import spinal.lib.fsm._
import spinal.lib.peripheral.uart.UartParameter
import spinal.lib.peripheral.uart._


object UartCtrlTxState extends SpinalEnum {
  val IDLE, START, DATA, PARITY, STOP = newElement()
}

class UartControllerTx(parameter: UartParameter) extends Component {
  val io = new Bundle {
    val frameStorage = in(UartControllerFrameStorage(parameter))
    val samplingTick = in Bool
    val write = slave Stream(Bits(parameter.dataWidthMax bits))
    val txd = out Bool
  }

  // Provide one clockDivider.tick each rxSamplePerBit pulse of io.samplingTick
  // Used by the stateMachine as a baud rate time reference
  val clockDivider = new Area {
    val counter = Counter(parameter.rxSamplePerBit)
    val tick = counter.willOverflow
    when (io.samplingTick) {
      counter.increment()
    }
  }

  // Count up each clockDivider.tick, used by the state machine to count up data bits and stop bits
  val tickCounter = new Area {
    val value = Reg(UInt(Math.max(log2Up(parameter.dataWidthMax), 2) bit))
    def reset() = value := 0

    when (clockDivider.tick) {
      value := value + 1
    }
  }

  val stateMachine = new Area {
    import UartCtrlTxState._

    val state = RegInit(IDLE)
    val parity = Reg(Bool)
    val txd = True

    when (clockDivider.tick) {
      parity := parity ^ txd
    }

    io.write.ready := False
    switch(state) {
      is (IDLE){
        when (io.write.valid && clockDivider.tick){
          state := START
        }
      }
      is (START) {
        txd := False
        when (clockDivider.tick) {
          state := DATA
          parity := io.frameStorage.parity === UartParityType.ODD
          tickCounter.reset()
        }
      }
      is (DATA) {
        txd := io.write.payload(tickCounter.value)
        when (clockDivider.tick) {
          when (tickCounter.value === io.frameStorage.dataLength) {
            io.write.ready := True
            tickCounter.reset()
            when (io.frameStorage.parity === UartParityType.NONE) {
              state := STOP
            } otherwise {
              state := PARITY
            }
          }
        }
      }
      is (PARITY) {
        txd := parity
        when (clockDivider.tick) {
          state := STOP
          tickCounter.reset()
        }
      }
      is (STOP) {
        when (clockDivider.tick) {
          when (tickCounter.value === toBitCount(io.frameStorage.stop)) {
            state := io.write.valid ? START | IDLE
          }
        }
      }
    }
  }

  io.txd := RegNext(stateMachine.txd) init(True)
}
