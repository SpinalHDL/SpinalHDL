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
package spinal.lib.bus.bram

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping


object BRAMDecoder{

  /**
    * Map all slave bram bus on a master bram bus
    */
  def apply(master: BRAM, slaves: Seq[(BRAM, SizeMapping)]): BRAMDecoder = {

    val decoder = new BRAMDecoder(master.config, slaves.map(_._2))

    // connect the master bus to the decoder
    decoder.io.input <> master

    // connect all slave to the decoder
    (slaves.map(_._1), decoder.io.outputs).zipped.map(_ << _)

    decoder
  }
}


/**
  * BRAM decoder
  *
  *         /|
  *        | | -- bram bus
  * BRAM - | | ...
  *        | | -- bram bus
  *         \|
  */
class BRAMDecoder(inputConfig: BRAMConfig, decodings: Seq[SizeMapping]) extends Component {

  val io = new Bundle {
    val input   = slave(BRAM(inputConfig))
    val outputs = Vec(master(BRAM(inputConfig)), decodings.size)
  }

  val sel = Bits(decodings.size bits)

  for((output, index) <- io.outputs.zipWithIndex){
    output.addr   := io.input.addr
    output.wrdata := io.input.wrdata
    output.we     := io.input.we
    output.en     := io.input.en && sel(index)
  }

  for((decoding, psel) <- (decodings, sel.asBools).zipped){
    psel := decoding.hit(io.input.addr) & io.input.en
  }

  val selIndex = RegNext(OHToUInt(sel))
  io.input.rddata := io.outputs(selIndex).rddata
}
