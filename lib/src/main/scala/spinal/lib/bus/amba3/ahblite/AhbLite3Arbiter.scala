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
  * AHB Lite arbiter
  *
  * @param ahbLite3Config : Ahb bus configuration
  * @param inputsCount    : Number of inputs for the arbiter
  */
case class AhbLite3Arbiter(ahbLite3Config: AhbLite3Config, inputsCount: Int, roundRobinArbiter : Boolean = true) extends Component {

  val io = new Bundle {
    val inputs = Vec(slave(AhbLite3(ahbLite3Config)), inputsCount)
    val output = master(AhbLite3(ahbLite3Config))
  }

  val logic = if(inputsCount == 1) new Area {

    io.output << io.inputs.head

  }else new Area{

    val dataPhaseActive = RegNextWhen(io.output.HTRANS(1), io.output.HREADY) init(False)
    val locked          = RegInit(False)
    val maskProposal    = Bits(inputsCount bits)
    val maskLocked      = Reg(Bits(inputsCount bits)) init(BigInt(1) << (inputsCount - 1))
    val maskRouted      = Mux(locked || dataPhaseActive, maskLocked, maskProposal)

    when(io.output.HSEL) { //valid
      maskLocked := maskRouted
      locked     := True

      when(io.output.HREADY){ //fire
        when(io.output.last && !io.output.HMASTLOCK) { //End of burst and no lock
          locked := False
        }
      }
    }

    val requests = io.inputs.map(_.HSEL).asBits
    if(roundRobinArbiter)
      maskProposal := OHMasking.roundRobin(requests, maskLocked(maskLocked.high - 1 downto 0) ## maskLocked.msb)
    else
      maskProposal := OHMasking.first(requests)

    val requestIndex     = OHToUInt(maskRouted)
    io.output.HSEL      := (io.inputs, maskRouted.asBools).zipped.map(_.HSEL & _).reduce(_ | _)
    io.output.HADDR     := io.inputs(requestIndex).HADDR
    io.output.HREADY    := io.inputs(requestIndex).HREADY
    io.output.HWRITE    := io.inputs(requestIndex).HWRITE
    io.output.HSIZE     := io.inputs(requestIndex).HSIZE
    io.output.HBURST    := io.inputs(requestIndex).HBURST
    io.output.HPROT     := io.inputs(requestIndex).HPROT
    io.output.HTRANS    := io.output.HSEL ? io.inputs(requestIndex).HTRANS | B"00"
    io.output.HMASTLOCK := io.inputs(requestIndex).HMASTLOCK

    val dataIndex        = RegNextWhen(requestIndex, io.output.HSEL && io.output.HREADY)
    io.output.HWDATA    := io.inputs(dataIndex).HWDATA

    for((input,requestRouted) <- (io.inputs,maskRouted.asBools).zipped){
      input.HRDATA    := io.output.HRDATA
      input.HRESP     := io.output.HRESP
      input.HREADYOUT := (!requestRouted && !input.HSEL) || (requestRouted && io.output.HREADYOUT)
    }

  }
}