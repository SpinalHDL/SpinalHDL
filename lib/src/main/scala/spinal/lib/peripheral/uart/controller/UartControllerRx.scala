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
import spinal.lib.peripheral.uart.UartParameter
import spinal.lib.peripheral.uart._


object UartCtrlRxState extends SpinalEnum {
  val IDLE, START, DATA, PARITY, STOP = newElement()
}

class UartControllerRx(parameter: UartParameter) extends Component {

  val io = new Bundle {
    val frameStorage = in(UartControllerFrameStorage(parameter))
    val samplingTick = in Bool
    val read         = master Flow(Bits(parameter.dataWidthMax bits))
    val rxd          = in Bool
  }

  // Implement the rxd sampling with a majority vote over samplingSize bits
  // Provide a new sampler.value each time sampler.tick is high
  val sampler = new Area {
    val synchroniser = BufferCC(io.rxd, init=False)
    val samples      = History(that = synchroniser, length = parameter.samplingSize, when = io.samplingTick, init = True)
    val value        = RegNext(MajorityVote(samples)) init(True)
    val tick         = RegNext(io.samplingTick) init(False)
  }

  // Provide a bitTimer.tick each rxSamplePerBit
  // reset() can be called to recenter the counter over a start bit.
  val bitTimer = new Area {
    val counter = Reg(UInt(log2Up(parameter.rxSamplePerBit) bit))
    def reset() = counter := parameter.preSamplingSize + (parameter.samplingSize - 1) / 2 - 1
    val tick = False
    when (sampler.tick) {
      counter := counter - 1
      when (counter === 0) {
        tick := True
        if (!isPow2(parameter.rxSamplePerBit))
          counter := parameter.rxSamplePerBit - 1
      }
    }
  }

  // Provide bitCounter.value that count up each bitTimer.tick, Used by the state machine to count data bits and stop bits
  // reset() can be called to reset it to zero
  val bitCounter = new Area {
    val value = Reg(UInt(Math.max(log2Up(parameter.dataWidthMax), 2) bit))
    def reset() = value := 0

    when(bitTimer.tick) {
      value := value + 1
    }
  }

  val stateMachine = new Area {
    import UartCtrlRxState._

    val state   = RegInit(IDLE)
    val parity  = Reg(Bool)
    val shifter = Reg(io.read.payload)
    val validReg = RegNext(False) init(False)
    io.read.valid := validReg

    //Parity calculation
    when (bitTimer.tick) {
      parity := parity ^ sampler.value
    }

    switch (state) {
      is (IDLE) {
        when (sampler.tick && !sampler.value) {
          state := START
          bitTimer.reset()
        }
      }
      is (START) {
        when (bitTimer.tick) {
          state := DATA
          bitCounter.reset()
          parity := io.frameStorage.parity === UartParityType.ODD
          when (sampler.value === True) {
            state := IDLE
          }
        }
      }
      is (DATA) {
        when (bitTimer.tick) {
          shifter(bitCounter.value) := sampler.value
          when (bitCounter.value === io.frameStorage.dataLength) {
            bitCounter.reset()
            when (io.frameStorage.parity === UartParityType.NONE) {
              state := STOP
              validReg := True
            } otherwise {
              state := PARITY
            }
          }
        }
      }
      is (PARITY) {
        when (bitTimer.tick) {
          bitCounter.reset()
          when (parity === sampler.value) {
            state := STOP
            validReg := True
          } otherwise {
            state := IDLE
          }
        }
      }
      is (STOP) {
        when (bitTimer.tick) {
          when (!sampler.value) {
            state := IDLE
          } elsewhen (bitCounter.value === toBitCount(io.frameStorage.stop)) {
            state := IDLE
          }
        }
      }
    }
  }
  io.read.payload := stateMachine.shifter
}
