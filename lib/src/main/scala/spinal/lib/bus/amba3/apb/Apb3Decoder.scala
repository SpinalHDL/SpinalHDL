package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping


object Apb3Decoder{
  def getOutputConfig(inputConfig: Apb3Config,decodings : Seq[SizeMapping]) = inputConfig.copy(selWidth = decodings.length)
}


class Apb3Decoder(inputConfig: Apb3Config,decodings : Seq[SizeMapping]) extends Component {
  import Apb3Router._
  assert(inputConfig.selWidth == 1)

  val io = new Bundle {
    val input = slave(Apb3(inputConfig))
    val output = master(Apb3(getOutputConfig(inputConfig)))
  }

  io.output.PADDR := io.input.PADDR
  io.output.PENABLE := io.input.PENABLE
  io.output.PWRITE := io.input.PWRITE
  io.output.PWDATA := io.input.PWDATA
  io.output.PADDR := io.input.PADDR

  for((decoding,psel) <- (decodings,io.output.PSEL.asBools).zipped){
    psel := decoding.hit(io.input.PADDR)
  }

  io.input.PREADY := io.output.PREADY
  io.input.PRDATA := io.output.PRDATA
  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.output.PSLVERROR
}