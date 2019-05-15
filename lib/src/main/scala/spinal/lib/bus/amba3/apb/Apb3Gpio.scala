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
package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}


object Apb3Gpio{

  def getApb3Config() = Apb3Config(addressWidth = 4,dataWidth = 32)
}


/*
 * gpioRead  -> 0x00 Read only register to read the physical pin values
 * gpioWrite -> 0x04 Read-Write register to access the output values
 * gpioDirection -> 0x08 Read-Write register to set the GPIO pin directions. When set, the corresponding pin is set as output.
 **/

case class Apb3Gpio(gpioWidth: Int, withReadSync : Boolean) extends Component {

  val io = new Bundle {
    val apb  = slave(Apb3(Apb3Gpio.getApb3Config()))
    val gpio = master(TriStateArray(gpioWidth bits))
    val value = out Bits(gpioWidth bits)
  }

  io.value := (if(withReadSync) BufferCC(io.gpio.read) else io.gpio.read)

  val ctrl = Apb3SlaveFactory(io.apb)
  ctrl.read(io.value, 0)
  ctrl.driveAndRead(io.gpio.write, 4)
  ctrl.driveAndRead(io.gpio.writeEnable, 8)
  io.gpio.writeEnable.getDrivingReg init(0)
}
