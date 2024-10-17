package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.function._
import spinal.lib.memory.sdram.dfi.interface._

case class Alignment(config: DfiConfig) extends Component {

  val io = new Bundle {
    val inIdfiport = slave(IDFI(config))
    val outDfiport = master(Dfi(config))
  }

  val caAlignment = CAAlignment(config)
  caAlignment.io.cke.setAll()
  caAlignment.io.cmd <> io.inIdfiport.cmd
  caAlignment.io.address <> io.inIdfiport.address
  caAlignment.io.output <> io.outDfiport.control

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.idfiWrData <> io.inIdfiport.wrData
  if (config.useWrdataCsN) wrAlignment.io.idfiWrCs <> io.inIdfiport.wrCs
  wrAlignment.io.dfiWr <> io.outDfiport.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.idfiRd <> io.inIdfiport.rdData
  if (config.useRddataCsN) rdAlignment.io.idfiRdCs <> io.inIdfiport.rdCs
  rdAlignment.io.dfiRd <> io.outDfiport.read.rd
  if (config.useRddataCsN) rdAlignment.io.dfiRdCs <> io.outDfiport.read.rdCs
  io.inIdfiport.rdEn <> io.outDfiport.read.rden
  rdAlignment.io.phaseClear := False
}
