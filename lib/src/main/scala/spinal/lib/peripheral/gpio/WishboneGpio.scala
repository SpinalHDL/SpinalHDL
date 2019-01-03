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

package spinal.lib.peripheral.gpio

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}
import spinal.lib.bus.wishbone._
import spinal.lib.peripheral.gpio.controller.GpioController


class WishboneGpio(
  ctrlParameter: GpioParameter,
  ctrlConfig: GpioConfig,
  busConfig: WishboneConfig = WishboneConfig(12, 32)
) extends Component {
  val io = new Bundle{
    val wb = slave(Wishbone(busConfig))
    val gpio = master(TriStateArray(ctrlParameter.gpioWidth bits))
  }

  val gpioCtrl = new GpioController(ctrlParameter)
  gpioCtrl.io.gpio <> io.gpio

  val busCtrl = WishboneSlaveFactory(io.wb)
  val bridge = gpioCtrl.driveFrom(busCtrl, ctrlParameter, ctrlConfig)
}
