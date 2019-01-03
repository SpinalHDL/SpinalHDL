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

package spinal.lib.peripheral.gpio.controller

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}
import spinal.lib.peripheral.gpio.{GpioParameter, GpioConfig}


case class GpioControllerStorage(gpioWidth: Int) extends Bundle {
  val write = Bits(gpioWidth bits)
  val writeEnable = Bits(gpioWidth bits)
}

class GpioController(parameter: GpioParameter) extends Component {
  val io = new Bundle {
    val storage = in(GpioControllerStorage(parameter.gpioWidth))
    val gpio = master(TriStateArray(parameter.gpioWidth bits))
  }

  io.gpio.writeEnable := io.storage.writeEnable
  io.gpio.write := io.storage.write

  def driveFrom(
    busCtrl: BusSlaveFactory,
    parameter: GpioParameter,
    config: GpioConfig,
    baseAddress: Int = 0
  ) = new Area {
    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val gpioCtrlStorage = Reg(io.storage)
    if (config.setDirection) {
      if (config.directionAsOut) {
        gpioCtrlStorage.writeEnable init(~0)
      } else {
        gpioCtrlStorage.writeEnable init(0)
      }
    }
    if (config.setOutput) {
      if (config.outputAsHigh) {
        gpioCtrlStorage.write init(~0)
      } else {
        gpioCtrlStorage.write init(0)
      }
    }

    busCtrlWrapped.read(io.gpio.read, 0x00)
    busCtrlWrapped.readAndWrite(gpioCtrlStorage.write, 0x04)
    busCtrlWrapped.readAndWrite(gpioCtrlStorage.writeEnable, 0x08)

    io.storage := gpioCtrlStorage
  }
}
