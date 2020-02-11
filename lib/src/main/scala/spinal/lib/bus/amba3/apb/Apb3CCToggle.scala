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


/**
  * Apb3 Cross Clock Domain by toggling
  *
  * @param busConfig  Apb3 configuration
  * @param inClk      Input Clock Domain
  * @param outClk     Output Clock Domain
  */
class Apb3CCToggle(busConfig: Apb3Config, inClk: ClockDomain, outClk: ClockDomain) extends Component {

  val io = new Bundle{
    val input  = slave(Apb3(busConfig))
    val output = master(Apb3(busConfig))
  }

  val outAreaHit       = Bool
  val outAreaPRDATA    = cloneOf(io.output.PRDATA)
  val outAreaPSLVERROR = if(busConfig.useSlaveError) cloneOf(io.output.PSLVERROR) else null

  /**
    * Input clock domain
    */
  val inClkArea = new ClockingArea(inClk){

    val hit    = BufferCC(outAreaHit, False)
    val target = RegInit(False)

    val pReady = False

    io.input.PRDATA := BufferCC(outAreaPRDATA)
    io.input.PREADY := RegNext(pReady, False)
    if(busConfig.useSlaveError) io.input.PSLVERROR := BufferCC(outAreaPSLVERROR)

    /** State machine */
    val sm = new StateMachine{

      val sIdle: State = new State with EntryPoint{
        whenIsActive{
          when(io.input.PENABLE && hit === target){
            target := !target
            goto(sAccess)
          }
        }
      }

      val sAccess: State = new State{
        whenIsActive{
          when(hit === target){
            pReady := True
            goto(sWait)
          }
        }
      }

      val sWait: State = new State{
        whenIsActive{
          goto(sIdle)
        }
      }
    }
  }


  /**
    * Output clock domain
    */
  val outClkArea = new ClockingArea(outClk){

    val target = BufferCC(inClkArea.target, False)
    val hit    = RegInit(False)

    val pENABLE   = RegInit(False)
    val pRDATA    = Reg(cloneOf(io.output.PRDATA))
    val pSLVERROR = if(busConfig.useSlaveError) Reg(cloneOf(io.output.PSLVERROR)) else null

    io.output.PENABLE := pENABLE
    io.output.PADDR   := BufferCC(io.input.PADDR)
    io.output.PSEL    := BufferCC(io.input.PSEL)
    io.output.PWRITE  := BufferCC(io.input.PWRITE)
    io.output.PWDATA  := BufferCC(io.input.PWDATA)

    when(target =/= hit & !pENABLE){
      pENABLE := True
    }

    when(pENABLE & io.output.PREADY){
      hit     := !hit
      pENABLE := False
      pRDATA  := io.output.PRDATA
      if(busConfig.useSlaveError) pSLVERROR := io.output.PSLVERROR
    }

    outAreaHit    := hit
    outAreaPRDATA := pRDATA
    if(busConfig.useSlaveError) outAreaPSLVERROR := pSLVERROR
  }

}




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
    val PWRITE     = Bool
    val PWDATA     = Bits(config.dataWidth bits)
  }
  case class Rsp() extends Bundle{
    val PRDATA     = Bits(config.dataWidth bits)
    val PSLVERROR  = if(config.useSlaveError) Bool else null
  }

  val inputLogic = new ClockingArea(inputClock){
    val inputCmd = Stream(Cmd())
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
    val outputCmd = StreamCCByToggle(inputLogic.inputCmd, inputClock, outputClock)
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

