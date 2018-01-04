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


object Apb3Router{
  def getOutputConfig(inputConfig: Apb3Config) = inputConfig.copy(selWidth = 1)
}


class Apb3Router(inputConfig: Apb3Config) extends Component {

  import Apb3Router._

  val io = new Bundle{
    val input   = slave(Apb3(inputConfig))
    val outputs = Vec(master(Apb3(getOutputConfig(inputConfig))), inputConfig.selWidth)
  }

  for((output,index) <- io.outputs.zipWithIndex){
    output.PADDR   := io.input.PADDR
    output.PENABLE := io.input.PENABLE
    output.PSEL(0) := io.input.PSEL(index)
    output.PWRITE  := io.input.PWRITE
    output.PWDATA  := io.input.PWDATA
  }


  val selIndex = RegNext(OHToUInt(io.input.PSEL))
  io.input.PREADY := io.outputs(selIndex).PREADY
  io.input.PRDATA := io.outputs(selIndex).PRDATA

  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.outputs(selIndex).PSLVERROR
}
