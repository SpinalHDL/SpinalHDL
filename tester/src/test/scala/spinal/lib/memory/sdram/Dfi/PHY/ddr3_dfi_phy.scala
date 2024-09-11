package spinal.lib.memory.sdram.Dfi.PHY

import spinal.core.BlackBox
import spinal.lib._
import spinal.core._

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
    val dfi = new Bundle{
      val address_i = in Bits(15 bits)
      val bank_i = in Bits(3 bits)
      val cas_n_i = in Bool()
      val cke_i = in Bool()
      val cs_n_i = in Bool()
      val odt_i = in Bool()
      val ras_n_i = in Bool()
      val reset_n_i = in Bool()
      val we_n_i = in Bool()
      val wrdata_i = in Bits(32 bits)
      val wrdata_en_i = in Bool()
      val wrdata_mask_i = in Bits(4 bits)
      val rddata_en_i = in Bool()

      val rddata_o = out Bits(32 bits)
      val rddata_valid_o = out Bool()
      val rddata_dnv_o = out Bits(2 bits)
    }
    val ddr3 = new Bundle{
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
