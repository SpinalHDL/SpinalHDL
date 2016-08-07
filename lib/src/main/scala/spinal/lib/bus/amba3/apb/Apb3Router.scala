package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._


object Apb3Router{
  def getOutputConfig(inputConfig : Apb3Config) = inputConfig.copy(selWidth = 1)
}

class Apb3Router(inputConfig: Apb3Config) extends Component{
  import Apb3Router._
  val io = new Bundle{
    val input = slave(Apb3(inputConfig))
    val outputs = Vec(master(Apb3(getOutputConfig(inputConfig))),inputConfig.selWidth)
  }

  for((output,index) <- io.outputs.zipWithIndex){
    output.PADDR := io.input.PADDR
    output.PENABLE := io.input.PENABLE
    output.PSEL(0) := io.input.PSEL(index)
    output.PWRITE := io.input.PWRITE
    output.PWDATA := io.input.PWDATA
    output.PADDR := io.input.PADDR
  }

  val selIndex = OHToUInt(io.input.PSEL)
  io.input.PREADY := io.outputs(selIndex).PREADY
  io.input.PRDATA := io.outputs(selIndex).PRDATA
  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.outputs(selIndex).PSLVERROR
}
