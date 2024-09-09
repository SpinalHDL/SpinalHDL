package spinal.lib.memory.sdram.Dfi.Alignment

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.Interface.{DfiAddr, DfiCmd, DfiConfig, DfiControlInterface, Dfiodt, IDFI, InitBus}

case class CAAlignment(config: DfiConfig) extends Component{
  import config._
  val io = new Bundle{
    val cmd     = Vec(slave(Flow(DfiCmd(config))),config.frequencyRatio)
    val address = Vec(slave(Flow(DfiAddr(config))),config.frequencyRatio)
    val ckeN    = in Vec(Bits(config.chipSelectNumber bits),config.frequencyRatio)
    val output  = master(DfiControlInterface(config))
  }
  //cke,reserN,odt
  //Most DRAMs define CKE as low at reset; some devices, such as LPDDR1, LPDDR2 and LPDDR3, define CKE as high at
  // reset. The default value should adhere to the DRAM definition.
  io.output.cke := io.ckeN.asBits
  //In general, the dfi_reset_n signal is defined as low at reset; however, in some cases it may be necessary
  // to hold dfi_reset_n high during initialization.
  if(useResetN)io.output.resetN.setAll()
  //The MC generates dfi_odt (dfi_odt_pN in frequency ratio systems) based on the DRAM burst length including 
  //CRC data. With CRC enabled, the MC may need to extend ODT.
  if(useOdt)io.output.odt.clearAll()

  //cmd
  if(useAckN)io.output.actN := Bits(widthOf(io.output.actN) bits).setAll()
  io.output.csN  := Bits(widthOf(io.output.csN) bits).setAll()
  io.output.rasN := Bits(widthOf(io.output.rasN) bits).setAll()
  io.output.casN := Bits(widthOf(io.output.casN) bits).setAll()
  io.output.weN  := Bits(widthOf(io.output.weN) bits).setAll()
  if(useCid)io.output.cid  := Bits(widthOf(io.output.cid) bits).clearAll()

  //address
  if(config.useBg)io.output.bg      := Bits(widthOf(io.output.bg) bits).assignDontCare()
  io.output.bank    := Bits(widthOf(io.output.bank) bits).clearAll()
  io.output.address := Bits(widthOf(io.output.address) bits).clearAll()

  for(i <- 0 until(frequencyRatio)){
    when(io.cmd(i).valid){
      if(useAckN)io.output.actN(i) := io.cmd(i).actN
      io.output.csN(i*chipSelectNumber,chipSelectNumber bits)  := io.cmd(i).csN
      io.output.rasN(i) := io.cmd(i).rasN
      io.output.casN(i) := io.cmd(i).casN
      io.output.weN(i)  := io.cmd(i).weN
      if(useCid)io.output.cid(i*chipIdWidth,chipIdWidth bits) := io.cmd(i).cid
    }
  }

  for(i <- 0 until(frequencyRatio)){
    when(io.address(i).valid){
      if(config.useBg)io.output.bg(i*bankGroupWidth,bankGroupWidth bits) := io.address(i).bg
      io.output.bank(i*bankWidth,bankWidth bits)         := io.address(i).bank
      io.output.address(i*addressWidth,addressWidth bits) := io.address(i).address
    }
  }

}
