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
import spinal.lib.bus.amba3.apb._


object AhbLite3ToApb3BridgePhase extends SpinalEnum{
  val IDLE, SETUP, ACCESS, ERROR = newElement
}

case class AhbLite3ToApb3Bridge(ahbConfig: AhbLite3Config, apbConfig: Apb3Config) extends Component {

  assert(ahbConfig.addressWidth >= apbConfig.addressWidth, "APB size address is bigger than the AHB size address")
  assert(ahbConfig.dataWidth == apbConfig.dataWidth, "AHB data width is not equal to APB data width")
  assert(apbConfig.selWidth == 1, "HSEL width must be equal to 1")

  import AhbLite3ToApb3BridgePhase._

  val io = new Bundle{
    val ahb = slave(AhbLite3(ahbConfig))
    val apb = master(Apb3(apbConfig))
  }

  val phase      = RegInit(IDLE)
  val write      = Reg(Bool)
  val address    = Reg(ahbConfig.addressType)
  val readedData = Reg(ahbConfig.dataType)


  io.apb.PADDR  := address.resized
  io.ahb.HRDATA := readedData
  io.apb.PWDATA := io.ahb.HWDATA
  io.apb.PWRITE := write
  io.ahb.HRESP  := io.apb.PSLVERROR


  switch(phase){
    is(IDLE){
      io.apb.PSEL      := B"0"
      io.apb.PENABLE   := False
      io.ahb.HREADYOUT := True

      when(io.ahb.HSEL && io.ahb.HTRANS(1) && io.ahb.HREADY){
        phase   := SETUP
        address := io.ahb.HADDR
        write   := io.ahb.HWRITE
      }
    }
    is(SETUP){
      io.apb.PSEL      := B"1"
      io.apb.PENABLE   := False
      io.ahb.HREADYOUT := False
      phase            := ACCESS
    }
    is(ACCESS){
      io.apb.PSEL      := B"1"
      io.apb.PENABLE   := True
      io.ahb.HREADYOUT := False

      when(io.apb.PREADY){
        readedData := io.apb.PRDATA
        phase      := io.apb.PSLVERROR ? ERROR | IDLE
      }
    }
    default{ // ERROR
      io.apb.PENABLE   := False
      io.apb.PSEL      := B"0"
      io.ahb.HREADYOUT := True
      io.ahb.HRESP     := True
      phase            := IDLE
    }
  }
}