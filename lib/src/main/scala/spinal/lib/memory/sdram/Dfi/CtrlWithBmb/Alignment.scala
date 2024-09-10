package spinal.lib.memory.sdram.Dfi.CtrlWithBmb

import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.Dfi._
import spinal.lib.memory.sdram.Dfi.Interface.{Dfi, DfiConfig, IDFI}

case class Alignment(config : DfiConfig) extends Component{

  val io = new Bundle{
    val inIdfiport = slave(IDFI(config))
    val outDfiport = master(Dfi(config))
  }

  val caAlignment = CAAlignment(config)
    caAlignment.io.ckeN.clearAll()
    caAlignment.io.cmd <> io.inIdfiport.cmd
    caAlignment.io.address <> io.inIdfiport.address
    caAlignment.io.output <> io.outDfiport.control

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.wrdata <> io.inIdfiport.wrdata
  if(config.useWrdataCsN)wrAlignment.io.wrcs <> io.inIdfiport.wrcs
  wrAlignment.io.output <> io.outDfiport.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.rddata <> io.inIdfiport.rddata
  if(config.useRddataCsN)rdAlignment.io.rdcs <> io.inIdfiport.rdcs
  rdAlignment.io.rd <> io.outDfiport.read.rd
  if(config.useRddataCsN)rdAlignment.io.rdCs <> io.outDfiport.read.rdCs
  io.inIdfiport.rden <> io.outDfiport.read.rden
  rdAlignment.io.phaseclear := False
}
