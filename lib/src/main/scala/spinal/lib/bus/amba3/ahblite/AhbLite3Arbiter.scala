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

case class AhbLite3AddrPhase(config: AhbLite3Config) extends Bundle{
  val HADDR     = UInt(config.addressWidth bits)
  val HWRITE    = Bool
  val HSIZE     = Bits(3 bits)
  val HBURST    = Bits(3 bits)
  val HPROT     = Bits(4 bits)
  val HTRANS    = Bits(2 bits)
  val HMASTLOCK = Bool

  def assignFromBus(bus: AhbLite3): Unit={
    HADDR     := bus.HADDR
    HWRITE    := bus.HWRITE
    HSIZE     := bus.HSIZE
    HBURST    := bus.HBURST
    HPROT     := bus.HPROT
    HTRANS    := bus.HTRANS
    HMASTLOCK := bus.HMASTLOCK
   }
}

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
    val requestIndex    = OHToUInt(maskRouted)

    /** Backup address phase */
    val addressPhaseData  = Vec(Reg(AhbLite3AddrPhase(ahbLite3Config)), io.inputs.length)
    val addressPhaseValid = Vec(RegInit(False), io.inputs.length)

    for((input, index) <- io.inputs.zipWithIndex){
      when(input.HSEL & input.HTRANS === 2 & input.HREADYOUT){
        addressPhaseData(index).assignFromBus(io.inputs(index))
        addressPhaseValid(index) := True
      }
    }

    val transactionOnHold = addressPhaseValid.reduce(_ || _)


    when(io.output.HSEL & io.output.HTRANS(1)) { // valid transaction
      maskLocked := maskRouted
      locked     := True
      addressPhaseValid(requestIndex) := False

      when(io.output.HREADY & io.output.last && !io.output.HMASTLOCK) { // End of burst and no lock
        locked := False
      }
    }

    /** Arbiter logic */
    val requests = Mux(transactionOnHold, addressPhaseValid.asBits, io.inputs.map(bus => bus.HSEL & bus.HTRANS(1)).asBits)

    if(roundRobinArbiter) {
      maskProposal := OHMasking.roundRobin(requests, maskLocked(maskLocked.high - 1 downto 0) ## maskLocked.msb)
    }else{
      maskProposal := OHMasking.first(requests)
    }

    /** Multiplexer */
    val bufferAddrEnable = addressPhaseValid(requestIndex)

    io.output.HSEL      := !io.output.isIdle ? (addressPhaseValid(requestIndex) || io.inputs(requestIndex).HSEL) | False
    io.output.HADDR     := bufferAddrEnable  ? addressPhaseData(requestIndex).HADDR     | io.inputs(requestIndex).HADDR
    io.output.HREADY    := bufferAddrEnable  ? True | io.inputs(requestIndex).HREADY
    io.output.HWRITE    := bufferAddrEnable  ? addressPhaseData(requestIndex).HWRITE    | io.inputs(requestIndex).HWRITE
    io.output.HSIZE     := bufferAddrEnable  ? addressPhaseData(requestIndex).HSIZE     | io.inputs(requestIndex).HSIZE
    io.output.HBURST    := bufferAddrEnable  ? addressPhaseData(requestIndex).HBURST    | io.inputs(requestIndex).HBURST
    io.output.HPROT     := bufferAddrEnable  ? addressPhaseData(requestIndex).HPROT     | io.inputs(requestIndex).HPROT
    io.output.HTRANS    := bufferAddrEnable  ? addressPhaseData(requestIndex).HTRANS    | io.inputs(requestIndex).HTRANS
    io.output.HMASTLOCK := bufferAddrEnable  ? addressPhaseData(requestIndex).HMASTLOCK | io.inputs(requestIndex).HMASTLOCK

    val dataIndex        = RegNextWhen(requestIndex, io.output.HSEL && io.output.HREADY)
    io.output.HWDATA    := io.inputs(dataIndex).HWDATA

    /** Drive input response signals  */
    for((input, requestRouted, onHold) <- (io.inputs, maskRouted.asBools, addressPhaseValid).zipped){

      val hreadyOut  = RegInit(True)

      when(!requestRouted & input.HSEL & input.HTRANS(1)){
        hreadyOut := False
      } elsewhen (requestRouted || !onHold){
        hreadyOut := True
      }

      input.HRDATA    := io.output.HRDATA
      input.HRESP     := io.output.HRESP & requestRouted
      input.HREADYOUT := (hreadyOut && io.output.HREADYOUT) || (hreadyOut && !requestRouted)
    }

  }
}