package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi._

/** Generates the x32 (dataWidth=32, fr=2) MC+PHY netlists for the TC12 e2e xsim top.
  * Config mirrors UsPhyGen/E2eMcGen EXACTLY except sdram.dataWidth = 32, so the
  * DFI bundle widths on both sides match bit-for-bit:
  *   DFI per-phase data = dataRate*dataWidth = 64 bits
  *   BMB word (beatWidth) = frequencyRatio*dataRate*dataWidth = 128 bits
  *   bytePerBurst = dataWidth*burstLength/8 = 32 bytes -> 1 burst = 2 BMB fragments
  * Usage: runMain ...Tc12X32Gen phy|mc   (writes UsPhy.v / DfiController.v to CWD) */
object Tc12X32Gen {
  private def mkCfg() = {
    val sdramtime = SdramTiming(
      generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15,
      WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35
    )
    val sdram = SdramConfig(
      SdramGeneration.DDR4,
      bgWidth = 1, cidWidth = 1, bankWidth = 4,
      columnWidth = 10, rowWidth = 13,
      dataWidth = 32, ddrMHZ = 200, ddrWrLat = 4, ddrRdLat = 4,
      sdramtime = sdramtime
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2, cmdPhase = 0,
      tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 1, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val signalConfig = new DDR4SignalConfig(DfiFunctionConfig(useTrainingSignals = true, useStatusSignals = true), false) {}
    val cfg = DfiConfig(
      chipSelectNumber = 2, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(),
      initSteps = None, bootTraining = false
    )
    cfg
  }

  def main(args: Array[String]): Unit = {
    val cfg = mkCfg()
    if (args.nonEmpty && args(0) == "mc") {
      val task = TaskParameter(
        timingWidth = 5, refWidth = 23,
        cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024
      )
      val bmbp = BmbParameter(
        addressWidth = cfg.sdram.byteAddressWidth + log2Up(cfg.chipSelectNumber),
        dataWidth = cfg.beatWidth,
        sourceWidth = 0, contextWidth = 2, lengthWidth = 10,
        alignment = BmbParameter.BurstAlignement.WORD
      )
      SpinalVerilog(DfiController(bmbp, task, cfg, RowBankColumn))
      println("GEN_MC_X32_DONE")
    } else {
      SpinalVerilog(UsPhy(cfg))
      println("GEN_PHY_X32_DONE")
    }
  }
}
