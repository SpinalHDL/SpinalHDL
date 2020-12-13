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
package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bram._


/**
  * State of the state machine of the wrapper
  */
object Axi4ToBRAMPhase extends SpinalEnum{
  val SETUP, ACCESS, READ, RESPONSE = newElement
}


object Axi4SharedToBram {

  /**
    * Return the axi and bram configuration
    */
  def getConfigs(addressAxiWidth: Int, addressBRAMWidth: Int, dataWidth: Int, idWidth: Int): (Axi4Config, BRAMConfig) =
    (
      Axi4Config(
        addressWidth = addressAxiWidth,
        dataWidth    = dataWidth,
        idWidth      = idWidth,
        useLock      = false,
        useRegion    = false,
        useCache     = false,
        useProt      = false,
        useQos       = false
      ),
      BRAMConfig(
        dataWidth    = dataWidth,
        addressWidth = addressBRAMWidth
      )
    )
}


/**
  * Axi4 <-> BRAM bus with burst
  * WARNING do not support byte mask !!
  */
class Axi4SharedToBram(addressAxiWidth: Int, addressBRAMWidth: Int, dataWidth: Int, idWidth: Int) extends Component {
  import Axi4ToBRAMPhase._

  assert(addressAxiWidth >= addressBRAMWidth, "Address of the BRAM bus can be bigger than the Axi address")

  val (axiConfig, bramConfig) = Axi4SharedToBram.getConfigs(addressAxiWidth, addressBRAMWidth, dataWidth, idWidth)

  val io = new Bundle{
    val axi  = slave (Axi4Shared(axiConfig))
    val bram = master(BRAM(bramConfig))
  }

  val phase      = RegInit(SETUP)
  val lenBurst   = Reg(cloneOf(io.axi.arw.len))
  val arw        = Reg(cloneOf(io.axi.arw.payload))
  val readData   = Reg(cloneOf(io.axi.r.data))

  def isEndBurst = lenBurst === 0

  io.axi.arw.ready  := False
  io.axi.w.ready    := False
  io.axi.b.valid    := False
  io.axi.b.resp     := Axi4.resp.OKAY
  io.axi.b.id       := arw.id
  io.axi.r.valid    := False
  io.axi.r.resp     := Axi4.resp.OKAY
  io.axi.r.id       := arw.id
  io.axi.r.data     := readData
  io.axi.r.last     := isEndBurst && !arw.write

  io.bram.en        := False
  io.bram.addr      := arw.addr.resized
  io.bram.wrdata    := io.axi.w.data
  io.bram.we        := arw.write ? B(io.bram.we.getWidth bits, default -> true) | B(0)

  /**
    * Main state machine
    */
  val sm = new Area {
    switch(phase){
      is(SETUP){
        arw       := io.axi.arw
        lenBurst  := io.axi.arw.len

        when(io.axi.arw.valid){
          io.axi.arw.ready := True
          phase            := ACCESS
        }
      }
      is(ACCESS){

        when(io.axi.w.valid || !arw.write){
          io.bram.en := True
          arw.addr   := Axi4.incr(arw.addr , arw.burst, arw.len, arw.size, 4)
        }

        io.axi.w.ready   := io.axi.w.valid & arw.write

        when(arw.write && (io.axi.w.last || arw.len === 0)){
          phase := RESPONSE
        }

        when(!arw.write){
          phase := READ
        }
      }
      is(READ){
        readData := io.bram.rddata
        phase    := RESPONSE
      }
      default{ // RESPONSE
        when(arw.write){
          io.axi.b.valid := True
          when(io.axi.b.ready){
            phase := SETUP
          }
        }.otherwise {
          io.axi.r.valid   := True
          when(io.axi.r.ready){
            when(isEndBurst){
              phase    := SETUP
            }otherwise{
              lenBurst := lenBurst - 1
              phase    := ACCESS
            }
          }
        }
      }
    }
  }
}