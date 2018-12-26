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
package spinal.lib.bus.bram


import spinal.core._
import spinal.lib._


/**
  * Bus BRAM configuration
  * @param dataWidth    : data width
  * @param addressWidth : address width
  */
case class BRAMConfig(dataWidth: Int, addressWidth: Int)


/**
  * Bus BRAM definition
  */
case class BRAM(config: BRAMConfig) extends Bundle with IMasterSlave {

  assert(config.dataWidth > 0 & config.dataWidth  % 8 == 0, "BRAM : dataWidth must be a multiple of 8")

  val en     = Bool
  val we     = Bits(config.dataWidth / 8 bits) // write byte enable
  val addr   = UInt(config.addressWidth bits)
  val wrdata = Bits(config.dataWidth bits)
  val rddata = Bits(config.dataWidth bits)


  /** Set the direction of the bus when it is used as master */
  override def asMaster(): Unit = {
    out(en, we, addr, wrdata)
    in(rddata)
  }

  /**
    * Connect two BRAM bus together Master >> Slave
    */
  def >> (sink: BRAM): Unit = {
    assert(this.config.addressWidth >= sink.config.addressWidth, "BRAM mismatch width address (slave address is bigger than master address )")
    assert(this.config.dataWidth == sink.config.dataWidth, "BRAM mismatch width data (slave and master doesn't have the same data width)")

    this.rddata := sink.rddata

    sink.addr   := this.addr.resized
    sink.we     := this.we
    sink.wrdata := this.wrdata
    sink.en     := this.en
  }

  /** Slave << Master */
  def << (sink: BRAM): Unit = sink >> this
}
