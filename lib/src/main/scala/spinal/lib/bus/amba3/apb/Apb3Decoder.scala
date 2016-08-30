package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping


object Apb3Decoder{
  def getOutputConfig(inputConfig: Apb3Config,decodings : Seq[SizeMapping]) = inputConfig.copy(selWidth = decodings.size)
  def apply(inputConfig: Apb3Config,decodings : Seq[SizeMapping]) : Apb3Decoder = new Apb3Decoder(inputConfig,decodings)

  def apply(master : Apb3,slaves : Seq[(Apb3,SizeMapping)]): Apb3Decoder ={
    val decoder = new Apb3Decoder(master.config,slaves.map(_._2))
    val router = new Apb3Router(decoder.io.output.config)
    decoder.io.input <> master
    router.io.input <> decoder.io.output
    (slaves.map(_._1),router.io.outputs).zipped.map(_ << _)
    decoder.setPartialName(master,"decoder")
  }
}


class Apb3Decoder(inputConfig: Apb3Config,decodings : Seq[SizeMapping]) extends Component {
  assert(inputConfig.selWidth == 1)

  val io = new Bundle {
    val input = slave(Apb3(inputConfig))
    val output = master(Apb3(Apb3Decoder.getOutputConfig(inputConfig,decodings)))
  }

  io.output.PADDR := io.input.PADDR
  io.output.PENABLE := io.input.PENABLE
  io.output.PWRITE := io.input.PWRITE
  io.output.PWDATA := io.input.PWDATA

  for((decoding,psel) <- (decodings,io.output.PSEL.asBools).zipped){
    psel := decoding.hit(io.input.PADDR) && io.input.PSEL.lsb
  }

  io.input.PREADY := io.output.PREADY
  io.input.PRDATA := io.output.PRDATA
  if(inputConfig.useSlaveError) io.input.PSLVERROR := io.output.PSLVERROR
}

