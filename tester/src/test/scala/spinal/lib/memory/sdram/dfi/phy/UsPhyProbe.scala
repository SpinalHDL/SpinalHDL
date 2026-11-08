package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._

object UsPhyProbe {
  def main(args: Array[String]): Unit = {
    val sdramtime = SdramTiming(
      generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15,
      WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35
    )
    val sdram = SdramConfig(
      SdramGeneration.DDR4,
      bgWidth = 1, cidWidth = 1, bankWidth = 4,
      columnWidth = 10, rowWidth = 13,
      dataWidth = 16, ddrMHZ = 200, ddrWrLat = 4, ddrRdLat = 4,
      sdramtime = sdramtime
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2, cmdPhase = 0,
      tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 2,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 4, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val signalConfig = new DDR4SignalConfig(DfiFunctionConfig(), false) {}
    val dfiConfig = DfiConfig(
      chipSelectNumber = 2, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(),
      initSteps = None, bootTraining = false
    )

    SpinalVerilog(new Component {
      val dfi = slave(Dfi(dfiConfig))
      // Tie off PHY-side (slave) outputs: read data
      dfi.read.rd.foreach { r =>
        r.rddataValid := False
        r.rddata := 0
        r.rddataDbiN := 0
      }

      val pads = new Bundle {
        val ck_t = out Bool(); val ck_c = out Bool()
        val cke = out Bool()
        val cs_n = out(Vec(Bool(), dfiConfig.chipSelectNumber))
        val ca = out(Vec(Bool(), dfiConfig.addressWidth))
        val act_n = out Bool()
        val bg = out(Vec(Bool(), dfiConfig.sdram.bgWidth))
        val cid = out(Vec(Bool(), dfiConfig.sdram.cidWidth))
        val odt = out Bool(); val reset_n = out Bool(); val par = out Bool()
        val dq = out(Vec(Vec(Bool(), 8), dfiConfig.sdram.dataWidth / 8))
        val dqs_t = out(Vec(Bool(), dfiConfig.sdram.dataWidth / 8))
        val dqs_c = out(Vec(Bool(), dfiConfig.sdram.dataWidth / 8))
        val dm_dbi_n = out(Vec(Bool(), dfiConfig.sdram.dataWidth / 8))
      }
      pads.ck_t := True; pads.ck_c := False; pads.cke := True
      pads.cs_n.foreach(_ := True)
      pads.ca.foreach(_ := False)
      pads.act_n := True; pads.bg.foreach(_ := False); pads.cid.foreach(_ := False)
      pads.odt := False; pads.reset_n := True; pads.par := False
      for (l <- pads.dq) l.foreach(_ := False)
      pads.dqs_t.foreach(_ := False); pads.dqs_c.foreach(_ := False)
      pads.dm_dbi_n.foreach(_ := False)
    })
    println("PROBE_OK: elaboration succeeded")
    println("  addressWidth = " + dfiConfig.addressWidth)
    println("  dataWidth    = " + dfiConfig.dataWidth)
    println("  beatWidth    = " + dfiConfig.beatWidth)
    println("  chipSelNum   = " + dfiConfig.chipSelectNumber)
    println("  freqRatio    = " + dfiConfig.frequencyRatio)
  }
}
