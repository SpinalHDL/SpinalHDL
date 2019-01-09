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


package spinal.lib.peripheral.timer.controller

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}
import spinal.lib.peripheral.timer.{MachineTimerParameter, MachineTimerConfig}


case class MachineTimerControllerStorage(parameter: MachineTimerParameter) extends Bundle {
  val compare = UInt(64 bits)
}

case class MachineTimerController(parameter: MachineTimerParameter) extends Component {
  val io = new Bundle {
    val storage = in(MachineTimerControllerStorage(parameter))
    val counter = out UInt(64 bits)
    val clear = in Bool
    val interrupt = out Bool
  }

  val counter = Reg(UInt(64 bits)) init(1)
  io.counter := counter
  val hit = RegInit(False)

  counter := counter + 1
  when (io.clear) {
    hit := False
  } .elsewhen (counter === io.storage.compare) {
    hit := True
  }
  io.interrupt := hit

  def driveFrom(
    busCtrl: BusSlaveFactory,
    parameter: MachineTimerParameter,
    config: MachineTimerConfig,
    baseAddress: Int = 0
  ) = new Area {
    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val storage = Reg(io.storage)
    storage.compare init(0)

    if (busCtrl.busDataWidth == 16) {
      for (i <- 0 to 3) {
        val offsetLow = i * 16
        val offsetHigh = offsetLow + 16 - 1
        busCtrlWrapped.read(io.counter(offsetHigh downto offsetLow), 0x00 + i * 2)
        busCtrlWrapped.readAndWrite(storage.compare(offsetHigh downto offsetLow), 0x08 + i * 2)
      }
    } else {
       for (i <- 0 to 1) {
        val offsetLow = i * 32
        val offsetHigh = offsetLow + 32 - 1
        busCtrlWrapped.read(io.counter(offsetHigh downto offsetLow), 0x00 + i * 4)
        busCtrlWrapped.readAndWrite(storage.compare(offsetHigh downto offsetLow), 0x08 + i * 4)
      }
    }
    /* Clear interrupt when writing to the lowest bits */
    io.clear := busCtrlWrapped.isWriting(0x08)

    io.storage := storage
  }

}
