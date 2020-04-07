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
import spinal.lib.bus.misc.SizeMapping


object Apb3Decoder{

  def getOutputConfig(inputConfig: Apb3Config, decodings: Seq[SizeMapping]) = inputConfig.copy(selWidth = decodings.size)

  def apply(inputConfig: Apb3Config, decodings: Seq[SizeMapping]): Apb3Decoder = new Apb3Decoder(inputConfig, decodings)

  def apply(master: Apb3, slaves: Seq[(Apb3, SizeMapping)]): Apb3Decoder = {

    val decoder = new Apb3Decoder(master.config, slaves.map(_._2))
    val router  = new Apb3Router(decoder.io.output.config)

    decoder.io.input <> master
    router.io.input  <> decoder.io.output

    (slaves.map(_._1), router.io.outputs).zipped.map(_ << _)

    decoder.setPartialName(master, "decoder")
  }
}



class Apb3Decoder(inputConfig: Apb3Config, decodings: Seq[SizeMapping]) extends Component {

  assert(inputConfig.selWidth == 1, "Apb3Decoder: input sel width must be equal to 1")
  assert(!SizeMapping.verifyOverlapping(decodings), "Apb3Decoder: overlapping found")

  for(mapping <- decodings) {
    assert(mapping.base % mapping.size == 0, f"Mapping at 0x${mapping.base}%x is not aligned to its size (0x${mapping.size}%x bytes)")
  }

  val io = new Bundle {
    val input  = slave(Apb3(inputConfig))
    val output = master(Apb3(Apb3Decoder.getOutputConfig(inputConfig,decodings)))
  }

  io.output.PADDR   := io.input.PADDR
  io.output.PENABLE := io.input.PENABLE
  io.output.PWRITE  := io.input.PWRITE
  io.output.PWDATA  := io.input.PWDATA

  for((decoding,psel) <- (decodings, io.output.PSEL.asBools).zipped){
    psel := decoding.hit(io.input.PADDR) && io.input.PSEL.lsb
  }

  io.input.PREADY := io.output.PREADY
  io.input.PRDATA := io.output.PRDATA

  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.output.PSLVERROR

  when(io.input.PSEL.lsb && io.output.PSEL === 0){
    io.input.PREADY := True
    if(inputConfig.useSlaveError) io.input.PSLVERROR := True
  }
}

