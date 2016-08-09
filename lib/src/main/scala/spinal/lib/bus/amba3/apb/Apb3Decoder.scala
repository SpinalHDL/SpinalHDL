package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping


object Apb3Decoder{
  def getOutputConfig(inputConfig: Apb3Config,decodings : Iterable[SizeMapping]) = inputConfig.copy(selWidth = decodings.size)
  def apply(inputConfig: Apb3Config,decodings : Iterable[SizeMapping]) : Apb3Decoder = new Apb3Decoder(inputConfig,decodings)
  def apply(apb : Apb3,mappings : Iterable[(Apb3,SizeMapping)]): Apb3Decoder ={
    val decoder = new Apb3Decoder(apb.config,mappings.map(_._2))
    decoder.io.input <> apb
    for((slave,idx) <- mappings.map(_._1).zipWithIndex){
      slave.PSEL  := decoder.io.output.PSEL(idx).asBits
      slave.PENABLE := decoder.io.output.PENABLE
      slave.PWRITE := decoder.io.output.PWRITE
      slave.PADDR := decoder.io.output.PADDR
      slave.PWDATA := decoder.io.output.PWDATA

      decoder.io.output.PREADY := slave.PREADY
      decoder.io.output.PRDATA := slave.PRDATA
      if(apb.config.useSlaveError) decoder.io.output.PSLVERROR := slave.PSLVERROR
    }
    decoder.setPartialName(apb,"decoder")
  }
}


class Apb3Decoder(inputConfig: Apb3Config,decodings : Iterable[SizeMapping]) extends Component {
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

  for((decoding,psel) <- (decodings,io.output.PSEL.asBools).zipped){
    psel := decoding.hit(io.input.PADDR)
  }

  io.input.PREADY := io.output.PREADY
  io.input.PRDATA := io.output.PRDATA
  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.output.PSLVERROR
}