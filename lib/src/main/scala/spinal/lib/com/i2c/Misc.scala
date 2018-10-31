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
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.io.ReadableOpenDrain


/**
  * I2C interface definition
  */
case class I2c() extends Bundle with IMasterSlave {

  val sda   = ReadableOpenDrain(Bool)
  val scl   = ReadableOpenDrain(Bool)

  override def asMaster(): Unit = {
    master(scl)
    master(sda)
  }

  override def asSlave(): Unit = {
    slave(scl)
    slave(sda)
  }
}



/**
  * Filter the SCL and SDA input signals
  */
class I2cIoFilter(i2c: I2c, clockDivider: UInt, samplingSize: Int, clockDividerWidth: BitCount) extends Area{

  // Clock divider for sampling the input signals
  val timer = new Area{
    val counter = Reg(UInt(clockDividerWidth)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){ 
      counter := clockDivider 
    }
  }

  // Input sampling
  val sampler = new Area {
    val sclSync = BufferCC(i2c.scl.read, True)
    val sdaSync = BufferCC(i2c.sda.read, True)

    val sclSamples = History(that = sclSync, range = 0 until samplingSize, when = timer.tick, init = True)
    val sdaSamples = History(that = sdaSync, range = 0 until samplingSize, when = timer.tick, init = True)
  }

  val sda, scl = RegInit(True)
  when(timer.tick){
    when(sampler.sdaSamples.map(_ =/= sda).andR){
      sda := sampler.sdaSamples.last
    }
    when(sampler.sclSamples.map(_ =/= scl).andR){
      scl := sampler.sclSamples.last
    }
  }
}
