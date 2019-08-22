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
