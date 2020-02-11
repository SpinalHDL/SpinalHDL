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
import spinal.lib.bus.misc.SizeMapping


/**
  * Default Slave
  * Return an error when an operation occurs
  */
class DefaultAhbLite3Slave(config: AhbLite3Config) extends Component{

  val io = slave(AhbLite3(config))

  object Phase extends SpinalEnum{
    val IDLE, ACCESS, RESPONSE, ERROR = newElement()
  }

  import Phase._

  val state = RegInit(IDLE)

  io.HREADYOUT := True
  io.HRESP     := False
  io.HRDATA    := 0

  switch(state){
    is(IDLE){
      when(io.HSEL & !io.isIdle){
        state := ACCESS
      }
    }
    is(ACCESS){
      io.HREADYOUT := False
      state        := RESPONSE
    }
    is(RESPONSE){
      io.HREADYOUT := False
      io.HRESP     := True
      state        := ERROR
    }
    default{
      io.HRESP := True
      state    := IDLE
    }
  }

}



object AhbLite3Decoder{

  def apply(ahbLite3Config: AhbLite3Config, decodings: Seq[SizeMapping]): AhbLite3Decoder = {
    new AhbLite3Decoder(ahbLite3Config, decodings)
  }

  /**
    * @example {{{
    *
    *     val decoder = AhbLite3Decoder(
    *                      master = io.master_ahb,
    *                      slaves = List(
    *                           io.slave_ahb_1 -> (0x00000000, 1kB),
    *                           io.slave_ahb_2 -> (0x10000000, 1kB),
    *                           io.slave_ahb_3 -> (0x20000000, 1kB)
    *                      ))
    *         }}}
    *
    * A default slave can be added as follow
    *
    * @example {{{
    *
    *     val decoder = AhbLite3Decoder(
    *                      master = io.master_ahb,
    *                      slaves = List(
    *                           io.slave_ahb_1 -> (0x00000000, 1kB),
    *                           io.slave_ahb_2 -> (0x10000000, 1kB),
    *                           io.slave_ahb_3 -> (0x20000000, 1kB)
    *                      ),
    *                      defaultSlave = myDefautlSlave.io.ahb
    *                      )
    *         }}}
    *
    */
  def apply(master: AhbLite3, slaves: Seq[(AhbLite3, SizeMapping)], defaultSlave: AhbLite3 = null): AhbLite3Decoder = {

    val decoder = new AhbLite3Decoder(master.config, slaves.map(_._2), defaultSlave != null)

    decoder.io.input << master
    (slaves.map(_._1), decoder.io.outputs).zipped.map(_ << _)

    if(defaultSlave != null) defaultSlave << decoder.io.defaultSlave

    decoder
  }

}



/**
  * AHB lite decoder
  *
  * @param ahbLite3Config : AHB bus configuration
  * @param decodings      : Mapping list for all outputs
  */
class AhbLite3Decoder(ahbLite3Config: AhbLite3Config, decodings: Seq[SizeMapping], addDefaultSlaveInterface: Boolean = false) extends Component {

  assert(!SizeMapping.verifyOverlapping(decodings), "AhbLite3Decoder : overlapping found")

  val io = new Bundle {
    val input        = slave(AhbLite3(ahbLite3Config))
    val outputs      = Vec(master(AhbLite3(ahbLite3Config)), decodings.size)
    val defaultSlave = if(addDefaultSlaveInterface) master(AhbLite3(ahbLite3Config)).setPartialName("defaultSlave") else null
  }


  val defaultSlave = if(addDefaultSlaveInterface) null else new DefaultAhbLite3Slave(ahbLite3Config)

  // add the default slave to the output list
  def outputs : List[AhbLite3] = io.outputs.toList ++ List(if(addDefaultSlaveInterface) io.defaultSlave else defaultSlave.io)

  val isIdle  = io.input.isIdle
  val wasIdle = RegNextWhen(isIdle, io.input.HREADY) init(True)

  val slaveReadyOutReduction = outputs.map(_.HREADYOUT).reduce(_ & _)

  val decodesSlaves       = decodings.map(_.hit(io.input.HADDR) && !isIdle && io.input.HSEL).asBits
  val decodeDefaultSlave  = decodesSlaves === 0 & !isIdle && io.input.HSEL

  val decodedSels  = decodeDefaultSlave ## decodesSlaves // !! reverse order compare to def outputs
  val applyedSels  = Bits(decodings.size + 1 bits)
  val previousSels = Reg(Bits(decodings.size + 1 bits)) init(0)


  val noneIdleSwitchDetected = previousSels =/= 0 && decodedSels =/= 0 && previousSels =/= decodedSels
  applyedSels      := !noneIdleSwitchDetected ? decodedSels     | 0
  val applyedHTRANS = !noneIdleSwitchDetected ? io.input.HTRANS | 0
  val applyedSlaveHREADY = noneIdleSwitchDetected ? slaveReadyOutReduction | io.input.HREADY

  when(applyedSlaveHREADY) {
    previousSels := applyedSels
  }

  for((output, sel) <- (outputs, applyedSels.asBools).zipped){
    output.HREADY    := applyedSlaveHREADY
    output.HSEL      := sel
    output.HADDR     := io.input.HADDR
    output.HWRITE    := io.input.HWRITE
    output.HSIZE     := io.input.HSIZE
    output.HBURST    := io.input.HBURST
    output.HPROT     := io.input.HPROT
    output.HTRANS    := applyedHTRANS
    output.HMASTLOCK := io.input.HMASTLOCK
    output.HWDATA    := io.input.HWDATA
  }

  val requestIndex = OHToUInt(outputs.map(_.HSEL))
  val dataIndex    = RegNextWhen(requestIndex, io.input.HREADYOUT)
  val slaveHRDATA  = outputs(dataIndex).HRDATA
  val slaveHRESP   = outputs(dataIndex).HRESP

  val switchBufferValid  = RegNextWhen(noneIdleSwitchDetected, applyedSlaveHREADY) init(False)
  val switchBufferHRDATA = RegNextWhen(slaveHRDATA, applyedSlaveHREADY)
  val switchBufferHRESP  = RegNextWhen(slaveHRESP, applyedSlaveHREADY)

  io.input.HRDATA    := switchBufferValid ? switchBufferHRDATA | slaveHRDATA
  io.input.HRESP     := switchBufferValid ? switchBufferHRESP  | slaveHRESP
  io.input.HREADYOUT := slaveReadyOutReduction && !noneIdleSwitchDetected

  when(wasIdle){
    io.input.HRESP := False
  }
}