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
import spinal.lib.fsm._



case class Apb3CC(config : Apb3Config,
                  inputClock : ClockDomain,
                  outputClock : ClockDomain) extends Component{
  assert(config.selWidth == 1)
  val io = new Bundle {
    val input = slave(Apb3(config))
    val output = master(Apb3(config))
  }

  case class Cmd() extends Bundle{
    val PADDR      = UInt(config.addressWidth bits)
    val PWRITE     = Bool()
    val PWDATA     = Bits(config.dataWidth bits)
  }
  case class Rsp() extends Bundle{
    val PRDATA     = Bits(config.dataWidth bits)
    val PSLVERROR  = if(config.useSlaveError) Bool() else null
  }

  val inputLogic = new ClockingArea(inputClock){
    val inputCmd = Flow(Cmd())
    val inputRsp = Flow(Rsp())
    val state = RegInit(False) setWhen(inputCmd.fire) clearWhen(inputRsp.valid)
    inputCmd.valid := io.input.PSEL.lsb && io.input.PENABLE && !state
    inputCmd.PADDR := io.input.PADDR
    inputCmd.PWRITE := io.input.PWRITE
    inputCmd.PWDATA := io.input.PWDATA

    io.input.PREADY := inputRsp.valid
    io.input.PRDATA := inputRsp.PRDATA
    if(config.useSlaveError) io.input.PSLVERROR := inputRsp.PSLVERROR
  }

  val outputLogic = new ClockingArea(outputClock){
    val outputCmdFlow = FlowCCByToggle(inputLogic.inputCmd, inputClock, outputClock, withOutputM2sPipe = false)
    val outputCmd = outputCmdFlow.toStream.m2sPipe(crossClockData = true, holdPayload = true)
    val state = RegInit(False)

    io.output.PENABLE := False
    io.output.PSEL := 0
    io.output.PADDR := outputCmd.PADDR
    io.output.PWDATA := outputCmd.PWDATA
    io.output.PWRITE := outputCmd.PWRITE
    outputCmd.ready := False

    when(outputCmd.valid) {
      io.output.PSEL := 1
      when(!state) {
        io.output.PENABLE := False
        state := True
      } otherwise {
        io.output.PENABLE := True
        when(io.output.PREADY){
          outputCmd.ready := True
          state := False
        }
      }
    }


    val outputRsp = Flow(Rsp())
    outputRsp.valid := outputCmd.fire
    outputRsp.PRDATA := io.output.PRDATA
    if(config.useSlaveError) outputRsp.PSLVERROR := io.output.PSLVERROR

    inputLogic.inputRsp := FlowCCByToggle(outputRsp, outputClock, inputClock)
  }
}



/**
  * Apb3 Cross Clock Domain by toggling
  *
  * @param busConfig  Apb3 configuration
  * @param inClk      Input Clock Domain
  * @param outClk     Output Clock Domain
  * @param selIds     Sequence indicating to which PSEL(x) signals the component replies to.
  */
class Apb3CCToggle(busConfig: Apb3Config, inClk: ClockDomain, outClk: ClockDomain, selIds: Seq[Int] = Seq(0)) extends Component {

  val io = new Bundle{
    val input  = slave(Apb3(busConfig))
    val output = master(Apb3(busConfig))
  }

  assert(selIds == Seq(0))
  val cc = Apb3CC(busConfig, inClk, outClk)
  cc.io.input <> io.input
  cc.io.output <> io.output
}



