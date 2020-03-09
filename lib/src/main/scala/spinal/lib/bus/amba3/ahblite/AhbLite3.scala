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
package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._

object AhbLite3 {
  /** Transfer type constant */
  def IDLE   = B"00"
  def BUSY   = B"01"
  def NONSEQ = B"10"
  def SEQ    = B"11"
}


case class AhbLite3Config(addressWidth: Int, dataWidth: Int){

  def addressType  = UInt(addressWidth bits)
  def dataType     = Bits(dataWidth bits)
  def bytePerWord  = dataWidth / 8
  def symboleRange = log2Up(bytePerWord) - 1 downto 0
  def wordRange    = addressWidth - 1 downto log2Up(bytePerWord)
}

/**
  * AHB lite Master interface
  */
case class AhbLite3Master(config: AhbLite3Config) extends Bundle with IMasterSlave {

  //  Address and control
  val HADDR     = UInt(config.addressWidth bits)
  val HWRITE    = Bool
  val HSIZE     = Bits(3 bits)
  val HBURST    = Bits(3 bits)
  val HPROT     = Bits(4 bits)
  val HTRANS    = Bits(2 bits)
  val HMASTLOCK = Bool

  //  Data
  val HWDATA    = Bits(config.dataWidth bits)
  val HRDATA    = Bits(config.dataWidth bits)

  //  Transfer response
  val HREADY    = Bool
  val HRESP     = Bool

  override def asMaster(): Unit = {
    out(HADDR, HWRITE, HSIZE, HBURST, HPROT, HTRANS, HMASTLOCK, HWDATA)
    in(HREADY, HRESP, HRDATA)
  }

  def isIdle = HTRANS === AhbLite3.IDLE


  def toAhbLite3(): AhbLite3 = {
    val slave = AhbLite3(config)

    slave.HADDR     := this.HADDR
    slave.HWRITE    := this.HWRITE
    slave.HSIZE     := this.HSIZE
    slave.HBURST    := this.HBURST
    slave.HPROT     := this.HPROT
    slave.HTRANS    := this.HTRANS
    slave.HMASTLOCK := this.HMASTLOCK
    slave.HWDATA    := this.HWDATA
    slave.HREADY    := slave.HREADYOUT
    slave.HSEL      := True

    this.HRDATA     := slave.HRDATA
    this.HRESP      := slave.HRESP
    this.HREADY     := slave.HREADYOUT

    slave
  }
}


/**
  * AHB lite interface
  */
case class AhbLite3(config: AhbLite3Config) extends Bundle with IMasterSlave {

  //  Address and control
  val HADDR     = UInt(config.addressWidth bits)
  val HSEL      = Bool
  val HREADY    = Bool
  val HWRITE    = Bool
  val HSIZE     = Bits(3 bits)
  val HBURST    = Bits(3 bits)
  val HPROT     = Bits(4 bits)
  val HTRANS    = Bits(2 bits)
  val HMASTLOCK = Bool

  //  Data
  val HWDATA = Bits(config.dataWidth bits)
  val HRDATA = Bits(config.dataWidth bits)

  //  Transfer response
  val HREADYOUT = Bool
  val HRESP     = Bool

  def setOKEY  = HRESP := False
  def setERROR = HRESP := True

  override def asMaster(): Unit = {
    out(HADDR, HWRITE, HSIZE, HBURST, HPROT, HTRANS, HMASTLOCK, HWDATA, HREADY, HSEL)
    in(HREADYOUT, HRESP, HRDATA)
  }

  def OKEY   = !HRESP
  def ERROR  = HRESP
  def isIdle = HTRANS === AhbLite3.IDLE

  //return true when the current transaction is the last one of the current burst
  def last(): Bool = {
    val beatCounter        = Reg(UInt(4 bits)) init(0)
    val isLast             = Vec(U"0000", U"0011", U"0111", U"1111")(U(HBURST >> 1)) === beatCounter || (HREADY && ERROR)

    when(HSEL && HREADY && HTRANS(1)){
      beatCounter := beatCounter + 1
      when(isLast){
        beatCounter := 0
      }
    }

    isLast
  }

  def isLast(): Bool = last && HSEL

  def fire(): Bool = HSEL && HREADYOUT

  def writeMask(): Bits = {
    val lowMask, highMask = Bits(config.bytePerWord bits)
    val low  =  HADDR(config.symboleRange)
    val high = HADDR(config.symboleRange) + Vec((0 to config.bytePerWord).map(idx => idx === HSIZE)).asBits.asUInt

    for(idx <- lowMask.range){
      lowMask(idx)  := (if(idx != low.maxValue) low <= idx else True)
      highMask(idx) := high > idx
    }

    lowMask & highMask
  }

  /** Connect two AhbLite3 bus together with the resized of the address */
  def <<(that: AhbLite3): Unit = {

    assert(that.config.addressWidth >= this.config.addressWidth, "AhbLite3 << : mismatch width address (use remap())")
    assert(this.config.dataWidth == that.config.dataWidth, "AhbLite3 << : mismatch data width")

    that.HREADYOUT := this.HREADYOUT
    that.HRDATA    := this.HRDATA
    that.HRESP     := this.HRESP

    this.HSEL      := that.HSEL
    this.HREADY    := that.HREADY
    this.HSIZE     := that.HSIZE
    this.HWDATA    := that.HWDATA
    this.HWRITE    := that.HWRITE
    this.HBURST    := that.HBURST
    this.HPROT     := that.HPROT
    this.HMASTLOCK := that.HMASTLOCK
    this.HTRANS    := that.HTRANS
    this.HADDR     := that.HADDR.resized
  }

  def >>(that: AhbLite3): Unit = that << this


  /**
    * Remap a bus
    *
    * @example {{{
    *
    *     val ahb_0 = AhbLite3(AhbLite3Config(32, 32))
    *     val ahb_1 = AhbLite3(AhbLite3Config(24, 32))
    *
    *     ahb_0 <> ahb_1.remapAddress(addr => U(0x40, 8 bits) @@ addr)
    *         }}}
    */
  def remapAddress(remapping: UInt => UInt): AhbLite3 = {
    val address = remapping(this.HADDR)

    val busRemap = AhbLite3(AhbLite3Config(dataWidth = this.config.dataWidth, addressWidth = address.getBitsWidth))

    this.HREADYOUT := busRemap.HREADYOUT
    this.HRDATA    := busRemap.HRDATA
    this.HRESP     := busRemap.HRESP

    busRemap.HSEL      := this.HSEL
    busRemap.HREADY    := this.HREADY
    busRemap.HSIZE     := this.HSIZE
    busRemap.HWDATA    := this.HWDATA
    busRemap.HWRITE    := this.HWRITE
    busRemap.HBURST    := this.HBURST
    busRemap.HPROT     := this.HPROT
    busRemap.HMASTLOCK := this.HMASTLOCK
    busRemap.HTRANS    := this.HTRANS
    busRemap.HADDR     := address

    busRemap
  }
}
