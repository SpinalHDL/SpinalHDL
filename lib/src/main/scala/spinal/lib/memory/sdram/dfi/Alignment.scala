package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class Alignment(config: DfiConfig) extends Component {

  val io = new Bundle {
    val input = slave(IDFI(config))
    val output = master(Dfi(config))
  }

  val caAlignment = CAAlignment(config)
  caAlignment.io.cke.setAll()
  caAlignment.io.cmd <> io.input.cmd
  caAlignment.io.address <> io.input.address
  caAlignment.io.output <> io.output.control

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.input <> io.input.wrData
  if (config.useWrdataCsN) wrAlignment.io.inputCs <> io.input.wrCs
  wrAlignment.io.output <> io.output.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.input <> io.input.rdData
  if (config.useRddataCsN) rdAlignment.io.inputCs <> io.input.rdCs
  rdAlignment.io.inputEn <> io.input.rdEn
  rdAlignment.io.output <> io.output.read
  rdAlignment.io.phaseClear := False
}
