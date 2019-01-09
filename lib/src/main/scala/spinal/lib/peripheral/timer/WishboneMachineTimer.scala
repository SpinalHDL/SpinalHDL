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

package spinal.lib.peripheral.timer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.peripheral.timer.controller.MachineTimerController


class WishboneMachineTimer(
  ctrlParameter: MachineTimerParameter,
  ctrlConfig: MachineTimerConfig,
  busConfig: WishboneConfig = WishboneConfig(12, 32)
) extends Component {
  val io = new Bundle {
    val apb = slave(Wishbone(busConfig))
    val interrupt = out Bool
  }

  val timerCtrl = MachineTimerController(ctrlParameter)
  io.interrupt := timerCtrl.io.interrupt

  val busCtrl = WishboneSlaveFactory(io.apb)
  timerCtrl.driveFrom(busCtrl, ctrlParameter, ctrlConfig)
}
