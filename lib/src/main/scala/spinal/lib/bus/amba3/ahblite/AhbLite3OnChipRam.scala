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


/**
  * AHB on chip RAM
  *
  * @param AhbLite3Config : Ahb bus configuration
  * @param byteCount      : Size of the RAM
  */
case class AhbLite3OnChipRam(AhbLite3Config: AhbLite3Config, byteCount: BigInt) extends Component {

  val io = new Bundle {
    val ahb = slave(AhbLite3(AhbLite3Config))
  }

  val wordCount = byteCount / AhbLite3Config.bytePerWord
  val ram       = Mem(AhbLite3Config.dataType, wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(AhbLite3Config.bytePerWord) - 1 downto log2Up(AhbLite3Config.bytePerWord)

  // Address/control phase to write data phase
  val pendingWrite = Reg(new Bundle{
    val valid   = Bool
    val address = ram.addressType()
    val mask    = Bits(AhbLite3Config.bytePerWord bits)
  })
  
  pendingWrite.valid init(False)
  pendingWrite.valid := False

  when(io.ahb.HREADY){
    pendingWrite.valid   := io.ahb.HSEL && io.ahb.HTRANS(1) && io.ahb.HWRITE
    pendingWrite.address := io.ahb.HADDR(wordRange)
    pendingWrite.mask    := io.ahb.writeMask
  }

  io.ahb.setOKEY

  // Avoid write to read hazards
  io.ahb.HREADYOUT := !(io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && pendingWrite.valid && io.ahb.HADDR(wordRange) === pendingWrite.address)
  io.ahb.HRDATA    := ram.readSync(
    address = io.ahb.HADDR(wordRange),
    enable  = io.ahb.HSEL && io.ahb.HTRANS(1) && !io.ahb.HWRITE && io.ahb.HREADY
  )

  ram.write(
    enable  = pendingWrite.valid,
    address = pendingWrite.address,
    mask    = pendingWrite.mask,
    data    = io.ahb.HWDATA
  )
}


case class AhbLite3OnChipRamMultiPort(portCount : Int, AhbLite3Config: AhbLite3Config, byteCount: BigInt) extends Component {

  val io = new Bundle {
    val ahbs = Vec(slave(AhbLite3(AhbLite3Config)), portCount)
  }

  val wordCount = byteCount / AhbLite3Config.bytePerWord
  val ram       = Mem(AhbLite3Config.dataType, wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(AhbLite3Config.bytePerWord) - 1 downto log2Up(AhbLite3Config.bytePerWord)


  for(ahb <- io.ahbs) {
    // Address/control phase to write data phase
    val pending = Reg(new Bundle {
      val valid = Bool
      val write = Bool
      val readInvalid = Bool
      val address = ram.addressType()
      val mask = Bits(AhbLite3Config.bytePerWord bits)
    })

    val addressPhaseCanRead = ahb.HREADY && !(pending.valid && (pending.write || pending.readInvalid))
    val ramAddress = Mux(addressPhaseCanRead, ahb.HADDR(wordRange), pending.address)

    pending.valid init (False)

    when(ahb.HREADY) {
      pending.valid := ahb.HSEL && ahb.HTRANS(1) && ahb.HWRITE
      pending.write := ahb.HWRITE
      pending.readInvalid := !addressPhaseCanRead
      pending.address := ahb.HADDR(wordRange)
      pending.mask := ahb.writeMask
    }

    ahb.setOKEY

    // Avoid write to read hazards
    ahb.HREADYOUT := !(ahb.HSEL && ahb.HTRANS(1) && !ahb.HWRITE && pending.valid && pending.write && ahb.HADDR(wordRange) === pending.address) && !(pending.valid && pending.readInvalid)
    ahb.HRDATA := ram.readSync(
      address = ramAddress
//      enable = (ahb.HSEL && ahb.HTRANS(1) && !ahb.HWRITE && ahb.HREADY) || (pending.valid && pending.readInvalid)
    )

    ram.write(
      enable = pending.valid,
      address = ramAddress,
      mask = pending.mask,
      data = ahb.HWDATA
    )

    when(pending.valid && pending.readInvalid){
      pending.readInvalid := False
    }
  }
}