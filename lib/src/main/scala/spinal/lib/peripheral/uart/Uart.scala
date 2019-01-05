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

package spinal.lib.peripheral.uart

import spinal.core._
import spinal.lib._
import spinal.lib.FragmentToBitsStates._


case class Uart() extends Bundle with IMasterSlave {
  val txd = Bool
  val rxd = Bool

  override def asMaster(): Unit = {
    out(txd)
    in(rxd)
  }
}

object UartParityType extends SpinalEnum(binarySequential) {
  val NONE, EVEN, ODD = newElement()
}

object UartStopType extends SpinalEnum(binarySequential) {
  val ONE, TWO = newElement()
  def toBitCount(that : C) : UInt = (that === ONE) ? U"0" | U"1"
}

case class UartParameter(
  busCanWriteFrameConfig: Boolean,
  dataWidthMax: Int,
  busCanWriteClockDividerConfig: Boolean,
  clockDividerWidth: Int,
  preSamplingSize: Int,
  samplingSize: Int,
  postSamplingSize: Int,
  txFifoDepth: Int,
  rxFifoDepth: Int
) {

  val rxSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")

  def getClockDivider(baudrate: Int) : Int = {
    return (ClockDomain.current.frequency.getValue / baudrate / rxSamplePerBit).toInt - 1
  }

  require(busCanWriteFrameConfig)
  require(dataWidthMax > 4 && dataWidthMax < 10)
  require(busCanWriteClockDividerConfig)
  require(clockDividerWidth > 1)
  require(rxSamplePerBit > 0)
  require(txFifoDepth > 0 && txFifoDepth < 256)
  require(rxFifoDepth > 0 && rxFifoDepth < 256)
}

object UartParameter {
  def default(): UartParameter = UartParameter(
    busCanWriteFrameConfig = true,
    dataWidthMax = 8,
    busCanWriteClockDividerConfig = true,
    clockDividerWidth = 20,
    preSamplingSize = 1,
    samplingSize = 5,
    postSamplingSize = 2,
    txFifoDepth = 16,
    rxFifoDepth = 16
  )
  def locked(): UartParameter = UartParameter(
    busCanWriteFrameConfig = false,
    dataWidthMax = 8,
    busCanWriteClockDividerConfig = false,
    clockDividerWidth = 20,
    preSamplingSize = 1,
    samplingSize = 5,
    postSamplingSize = 2,
    txFifoDepth = 16,
    rxFifoDepth = 16
  )
}

case class UartConfig(
  baudrate: Int,
  dataLength: Int,
  parity: UartParityType.E,
  stop: UartStopType.E
) { }

object UartConfig {
  def default() : UartConfig = default(115200)

  def default(baudrate: Int): UartConfig = UartConfig(
    baudrate = baudrate,
    dataLength = 7,
    parity = UartParityType.NONE,
    stop = UartStopType.ONE
  )
}
