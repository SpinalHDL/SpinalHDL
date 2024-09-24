package spinal.demo.phy

import spinal.core.{BlackBox, _}
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface.{Dfi, DfiConfig, TaskParameterAggregate}

class ddr3_IO extends Bundle {
  val ck_p_o = out Bool ()
  val ck_n_o = out Bool ()
  val cke_o = out Bool ()
  val reset_n_o = out Bool ()
  val ras_n_o = out Bool ()
  val cas_n_o = out Bool ()
  val we_n_o = out Bool ()
  val cs_n_o = out Bool ()
  val ba_o = out Bits (3 bits)
  val addr_o = out Bits (15 bits)
  val odt_o = out Bool ()
  val dm_o = out Bits (2 bits)
  val dqs_p_io = inout(Analog(Bits(2 bits)))
  val dqs_n_io = inout(Analog(Bits(2 bits)))
  val dq_io = inout(Analog(Bits(16 bits)))
}

case class ddr3_dfi_phy(config: DfiConfig) extends BlackBox {
  val io = new Bundle {
    val clk = new Bundle {
      val i = in Bool ()
      val ddr_i = in Bool ()
      val ddr90_i = in Bool ()
      val ref_i = in Bool ()
    }
    val rst_i = in Bool ()
    val cfg = new Bundle {
      val valid_i = in Bool ()
      val i = in Bits (32 bits)
    }
    val dfi = slave(Dfi(config))
    val ddr3 = new ddr3_IO
  }
  addGeneric("REFCLK_FREQUENCY", 200)
  addGeneric("DQS_TAP_DELAY_INIT", 27)
  addGeneric("DQ_TAP_DELAY_INIT", 0)
  addGeneric("TPHY_RDLAT", 3)
  addGeneric("TPHY_WRLAT", 3)
  addGeneric("TPHY_WRDATA", 0)
  noIoPrefix()
  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      if (bt.getName().contains("dfi")) {
        if (bt.getName().contains("control")) bt.setName(bt.getName().replace("control_", "").replace("N", "_n") + "_i")
        if (bt.getName().contains("write"))
          bt.setName(bt.getName().replace("write_wr_0_", "").replace("En", "_en").replace("Mask", "_mask") + "_i")
        if (bt.getName().contains("rddata"))
          bt.setName(bt.getName().replace("read_", "").replace("Valid", "_valid").replace("Dnv", "_dnv") + "_o")
        if (bt.getName().contains("rden"))
          bt.setName(bt.getName().replace("read_", "").replace("rden_", "rddata_en_").replace("_0", "") + "_i")
      }
    })
  }
  addPrePopTask(() => renameIO())
  addRTLPath("tester/src/test/scala/spinal/demo/phy/ddr3_dfi_phy.v")
}

case class dfi_phy_ddr3(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val clk = new Bundle {
      val i = in Bool ()
      val ddr_i = in Bool ()
      val ddr90_i = in Bool ()
      val ref_i = in Bool ()
    }
    val rst_i = in Bool ()
    val initDone = out Bool ()
    val dfi = slave(Dfi(config))
    val ddr3 = new ddr3_IO
  }
  val init = Initialize(tpa)
  init.io.initDone <> io.initDone

  val initDfi = cloneOf(io.dfi)
  initDfi.control <> init.io.control
  initDfi.write.wr.clearAll()
  initDfi.read.rden.clearAll()

  val ddr3Phy = ddr3_dfi_phy(tpa.config)
  ddr3Phy.io.clk <> io.clk
  ddr3Phy.io.rst_i <> io.rst_i
  ddr3Phy.io.ddr3 <> io.ddr3
  ddr3Phy.io.cfg.valid_i.clear()
  ddr3Phy.io.cfg.i.clearAll()
  ddr3Phy.io.dfi <> Mux(init.io.initDone, io.dfi, initDfi)
  io.dfi.read.rd <> ddr3Phy.io.dfi.read.rd

}
