package spinal.demo.phy

import spinal.core.{BlackBox, _}
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface.{Dfi, DfiConfig, TaskConfig}

case class Ddr3DfiPhy(dc: DfiConfig) extends BlackBox {
  val io = new Bundle {
    val clk = new Bundle {
      val work = in Bool ()
      val ddr = in Bool ()
      val ddr90 = in Bool ()
      val ref = in Bool ()
    }
    val rst = in Bool ()
    val cfg = new Bundle {
      val valid = in Bool ()
      val value = in Bits (32 bits)
    }
    val dfi = slave(Dfi(dc))
    val ddr3 = new DDR3IO(dc)
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
      if (bt.getName().contains("clk")) bt.setName(bt.getName().replace("_work", "") + "_i")
      if (bt.getName().contains("rst")) bt.setName(bt.getName() + "_i")
      if (bt.getName().contains("ddr3")) {
        bt.setName(bt.getName().replace("P", "_p").replace("N", "_n") + "_i")
        if (bt.getName().contains("dq")) bt.setName(bt.getName().replace("_i", "_io"))
      }
      if (bt.getName().contains("dfi")) {
        if (bt.getName().contains("control")) bt.setName(bt.getName().replace("control_", "").replace("N", "_n") + "_i")
        if (bt.getName().contains("write"))
          bt.setName(bt.getName().replace("write_wr_0_", "").replace("En", "_en").replace("Mask", "_mask") + "_i")
        if (bt.getName().contains("rddata"))
          bt.setName(bt.getName().replace("_read_rd_0", "").replace("Valid", "_valid").replace("Dnv", "_dnv") + "_o")
        if (bt.getName().contains("rden"))
          bt.setName(bt.getName().replace("_read_rden_0", "_rddata_en_i"))
      }
      if (bt.getName().contains("cfg")) bt.setName(bt.getName().replace("_value", "") + "_i")
    })
  }
  addPrePopTask(() => renameIO())
  addRTLPath("tester/src/test/scala/spinal/demo/phy/ddr3_dfi_phy.v")
}

case class DfiPhyDdr3(tc: TaskConfig, dc: DfiConfig) extends Component {
  val io = new Bundle {
    val clk = new Bundle {
      val work = in Bool ()
      val ddr = in Bool ()
      val ddr90 = in Bool ()
      val ref = in Bool ()
    }
    val rst = in Bool ()
    val initDone = out Bool ()
    val dfi = slave(Dfi(dc))
    val ddr3 = new DDR3IO(dc)
  }
  val init = Initialize(tc, dc)
  init.io.initDone <> io.initDone

  val initDfi = cloneOf(io.dfi)
  initDfi.control <> init.io.control
  initDfi.write.wr.clearAll()
  initDfi.read.rden.clearAll()

  val ddr3Phy = Ddr3DfiPhy(dc)
  ddr3Phy.io.clk <> io.clk
  ddr3Phy.io.rst <> io.rst
  ddr3Phy.io.ddr3 <> io.ddr3
  ddr3Phy.io.cfg.valid.clear()
  ddr3Phy.io.cfg.value.clearAll()
  ddr3Phy.io.dfi <> Mux(init.io.initDone, io.dfi, initDfi)
  io.dfi.read.rd <> ddr3Phy.io.dfi.read.rd
}
