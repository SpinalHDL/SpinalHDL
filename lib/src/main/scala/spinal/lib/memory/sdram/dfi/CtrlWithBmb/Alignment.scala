package spinal.lib.memory.sdram.dfi.CtrlWithBmb

import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.dfi._
import spinal.lib.memory.sdram.dfi.Interface.{Dfi, DfiConfig, IDFI}

case class Alignment(config : DfiConfig) extends Component{

  val io = new Bundle{
    val inIdfiport = slave(IDFI(config))
    val outDfiport = master(Dfi(config))
  }

  val caAlignment = CAAlignment(config)
    caAlignment.io.cke.setAll()
    caAlignment.io.cmd <> io.inIdfiport.cmd
    caAlignment.io.address <> io.inIdfiport.address
    caAlignment.io.output <> io.outDfiport.control

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.wrdata <> io.inIdfiport.wrData
  if(config.useWrdataCsN)wrAlignment.io.wrcs <> io.inIdfiport.wrCs
  wrAlignment.io.output <> io.outDfiport.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.rddata <> io.inIdfiport.rdData
  if(config.useRddataCsN)rdAlignment.io.rdcs <> io.inIdfiport.rdCs
  rdAlignment.io.rd <> io.outDfiport.read.rd
  if(config.useRddataCsN)rdAlignment.io.rdCs <> io.outDfiport.read.rdCs
  io.inIdfiport.rdEn <> io.outDfiport.read.rden
  rdAlignment.io.phaseclear := False
}
