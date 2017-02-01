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
package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.io._


/**
  * I2C interface definition
  */
case class I2C() extends Bundle with IMasterSlave {

  val sda   = ReadableOpenDrain(Bool)
  val scl   = ReadableOpenDrain(Bool)

  override def asMaster(): Unit = {
    master(scl)
    master(sda)
  }

  override def asSlave(): Unit = {
    master(scl)
    master(sda)
  }
}


/**
  * Counter of bit write/read.
  * MSB is send first on the I2C bus so the counter goes from dataWdith-1 to 0
  */
class I2CBitCounter(sclFallingEdge: Bool, dataWidth: BitCount) extends Area {

  val index  = Reg(UInt(log2Up(dataWidth.value) bits)) randBoot()
  val isDone = False

  def clear() : Unit = index := dataWidth.value - 1

  when(sclFallingEdge) {
    index  := index - 1
    isDone := index === 0
  }
}


/**
  * Detect the rising and falling Edge of the SCL signals
  */
class SCLEdgeDetector(scl: Bool) extends Area{
  val scl_prev = RegNext(scl) init(True)

  val rising  =  scl && !scl_prev
  val falling = !scl && scl_prev
}


/**
  * Filter the SCL and SDA input signals
  */
class I2CFilterInput(i2c_sda: Bool, i2c_scl: Bool, clockDivider: UInt, samplingSize: Int, clockDividerWidth: BitCount) extends Area{

  // Clock divider for sampling the input signals
  val samplingClockDivider = new Area{
    val counter = Reg(UInt(clockDividerWidth)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){ counter := clockDivider }
  }

  // Input sampling
  val samplingInput = new Area {
    val cc_scl = BufferCC(i2c_scl)
    val cc_sda = BufferCC(i2c_sda)

    val sdaSamples = History(that = cc_sda, length = samplingSize, when = samplingClockDivider.tick, init = True)
    val sclSamples = History(that = cc_scl, length = samplingSize, when = samplingClockDivider.tick, init = True)

    val sdaMajority = MajorityVote(sdaSamples)
    val sclMajority = MajorityVote(sclSamples)
  }

  val sda = RegNext(samplingInput.sdaMajority)
  val scl = RegNext(samplingInput.sclMajority)
}

