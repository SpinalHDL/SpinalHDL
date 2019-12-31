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


case class Apb3Config(
  addressWidth  : Int,
  dataWidth     : Int,
  selWidth      : Int = 1,
  useSlaveError : Boolean = true
)


object Apb3{

  def apply(addressWidth: Int, dataWidth: Int) = new Apb3(Apb3Config(addressWidth = addressWidth, dataWidth = dataWidth))
}


/**
  * AMBA 3 APB Protocol interface
  */
case class Apb3(config: Apb3Config) extends Bundle with IMasterSlave {

  val PADDR      = UInt(config.addressWidth bits)
  val PSEL       = Bits(config.selWidth bits)
  val PENABLE    = Bool
  val PREADY     = Bool
  val PWRITE     = Bool 
  val PWDATA     = Bits(config.dataWidth bits)
  val PRDATA     = Bits(config.dataWidth bits)
  val PSLVERROR  = if(config.useSlaveError) Bool else null

  override def asMaster(): Unit = {
    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
    in(PREADY, PRDATA)
    if(config.useSlaveError) in(PSLVERROR)
  }

  def << (sink: Apb3): Unit = sink >> this

  def >> (sink: Apb3): Unit = {
//    assert(this.config.addressWidth >= sink.config.addressWidth, "APB3 mismatch width address")
    assert(this.config.selWidth == sink.config.selWidth, "APB3 mismatch sel width")

    sink.PADDR   := this.PADDR.resized
    sink.PSEL    := this.PSEL
    sink.PENABLE := this.PENABLE
    this.PREADY  := sink.PREADY
    sink.PWRITE  := this.PWRITE
    sink.PWDATA  := this.PWDATA
    this.PRDATA  := sink.PRDATA

    if(PSLVERROR != null) {
      this.PSLVERROR := (if (sink.PSLVERROR != null) sink.PSLVERROR else False)
    }
  }

  // !! no tested !!
  def m2sPipe(): Apb3 ={
    val that = Apb3(config)
    that.PADDR    := RegNext(this.PADDR  )
    that.PWRITE   := RegNext(this.PWRITE )
    that.PWDATA   := RegNext(this.PWDATA )
    that.PSEL     := RegNext(this.PREADY ? B(0) | this.PSEL)
    that.PENABLE  := RegNext(this.PENABLE && !this.PREADY)
    this.PRDATA   := that.PRDATA
    this.PREADY   := that.PREADY && that.PENABLE
    if(PSLVERROR != null) { this.PSLVERROR := that.PSLVERROR }
    that
  }

  def crossClockDomainToggle(inClk: ClockDomain, outClk: ClockDomain): Apb3 = {
    val cc = new Apb3CCToggle(this.config, inClk, outClk)
    cc.io.input <> this
    return cc.io.output
  }
}