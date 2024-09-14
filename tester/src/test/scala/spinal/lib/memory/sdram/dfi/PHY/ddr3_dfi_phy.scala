package spinal.lib.memory.sdram.dfi.PHY

import spinal.core.BlackBox
import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.dfi.Interface.TaskParameterAggregate

class dfi_IO extends Bundle with IMasterSlave {
  val address_i =  Bits(15 bits)
  val bank_i =  Bits(3 bits)
  val cas_n_i =  Bool()
  val cke_i =  Bool()
  val cs_n_i =  Bool()
  val odt_i =  Bool()
  val ras_n_i =  Bool()
  val reset_n_i =  Bool()
  val we_n_i =  Bool()
  val wrdata_i =  Bits(32 bits)
  val wrdata_en_i =  Bool()
  val wrdata_mask_i =  Bits(4 bits)
  val rddata_en_i =  Bool()

  val rddata_o =  Bits(32 bits)
  val rddata_valid_o =  Bool()
  val rddata_dnv_o =  Bits(2 bits)

  override def asMaster(): Unit = {
    in(address_i,bank_i,cas_n_i,cke_i,cs_n_i,odt_i,ras_n_i,reset_n_i,we_n_i,
      wrdata_i,wrdata_en_i,wrdata_mask_i,rddata_en_i)
    out(rddata_o,rddata_valid_o,rddata_dnv_o)
  }
}

class ddr3_IO extends Bundle{
  val ck_p_o = out Bool()
  val ck_n_o = out Bool()
  val cke_o = out Bool()
  val reset_n_o = out Bool()
  val ras_n_o = out Bool()
  val cas_n_o = out Bool()
  val we_n_o = out Bool()
  val cs_n_o = out Bool()
  val ba_o = out Bits(3 bits)
  val addr_o = out Bits(15 bits)
  val odt_o = out Bool()
  val dm_o = out Bits(2 bits)
  val dqs_p_io = inout (Analog(Bits(2 bits)))
  val dqs_n_io = inout (Analog(Bits(2 bits)))
  val dq_io = inout (Analog(Bits(16 bits)))
}

case class ddr3_dfi_phy() extends BlackBox{
  val io = new Bundle{
    val clk = new Bundle{
      val i = in Bool()
      val ddr_i = in Bool()
      val ddr90_i = in Bool()
      val ref_i = in Bool()
    }
    val rst_i = in Bool()
    val cfg = new Bundle{
      val valid_i = in Bool()
      val i = in Bits(32 bits)
    }
    val dfi = master(new dfi_IO)
    val ddr3 = new ddr3_IO
  }
  addGeneric("REFCLK_FREQUENCY",200)
  addGeneric("DQS_TAP_DELAY_INIT",27)
  addGeneric("DQ_TAP_DELAY_INIT",0)
  addGeneric("TPHY_RDLAT",3)
  addGeneric("TPHY_WRLAT",3)
  addGeneric("TPHY_WRDATA",0)
  noIoPrefix()

  addRTLPath("tester/src/test/scala/spinal/lib/memory/sdram/Dfi/PHY/ddr3_dfi_phy.v")
}

case class dfi_phy_ddr3(tpa:TaskParameterAggregate) extends Component{
  val io = new Bundle {
    val clk = new Bundle {
      val i = in Bool()
      val ddr_i = in Bool()
      val ddr90_i = in Bool()
      val ref_i = in Bool()
    }
    val rst_i = in Bool()
    val initDone = out Bool()
    val dfi = master(new dfi_IO)
    val ddr3 = new ddr3_IO
  }
  val init = Initialize(tpa)
  init.io.initDone <> io.initDone

  val initDfi = slave(new dfi_IO)
  initDfi.address_i <> init.io.address.address
  initDfi.bank_i <> init.io.address.bank
  initDfi.cas_n_i <> init.io.cmd.casN
  initDfi.cke_i <> init.io.cke
  initDfi.cs_n_i <> init.io.cmd.csN
  initDfi.odt_i <> False
  initDfi.ras_n_i <> init.io.cmd.rasN
  initDfi.reset_n_i <> True
  initDfi.we_n_i <> init.io.cmd.weN
  initDfi.wrdata_i <> B(0,32 bits)
  initDfi.wrdata_en_i <> False
  initDfi.wrdata_mask_i <> B(0,4 bits)
  initDfi.rddata_en_i <> False


  val ddr3Phy = ddr3_dfi_phy()
  ddr3Phy.io.clk <> io.clk
  ddr3Phy.io.rst_i <> io.rst_i
  ddr3Phy.io.ddr3 <> io.ddr3
  ddr3Phy.io.cfg.valid_i.clear()
  ddr3Phy.io.cfg.i.clearAll()
  ddr3Phy.io.dfi <> Mux(init.io.initDone,io.dfi,initDfi)
  io.dfi.rddata_valid_o <> ddr3Phy.io.dfi.rddata_valid_o
  io.dfi.rddata_o <> ddr3Phy.io.dfi.rddata_o

}