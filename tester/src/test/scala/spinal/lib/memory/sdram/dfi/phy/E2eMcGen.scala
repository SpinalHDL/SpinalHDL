package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi._

/** Generates the MC side (DfiController) for the TC10 e2e xsim top.
  * Config mirrors UsPhyGen EXACTLY (same DfiConfig) so the DFI bundle widths
  * on both sides match bit-for-bit. */
object E2eMcGen {
  def main(args: Array[String]): Unit = {
    // Optional arg: mr5 hex override (e.g. "0x1800" = write+read DBI enabled).
    val mr5: Int = if (args.length >= 1) java.lang.Integer.decode(args(0)).toInt else 0
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
      // tPhyWrData = 0: UsPhy serializes dfi_wrdata on every refclk edge inside the
      // wrdataEn window (OSERDESE3 frame-load == wren edge), i.e. it implements the
      // zero-extra-data-delay DFI contract. A nonzero value activates WrAlignment's
      // delay chain, pushing beat k's data out of the wren window (stale-duplicate bug).
      tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 1, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val signalConfig = new DDR4SignalConfig(DfiFunctionConfig(useTrainingSignals = true, useStatusSignals = true), false) {}
    val cfg = DfiConfig(
      chipSelectNumber = 2, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(mr5 = mr5),
      initSteps = None, bootTraining = false
    )
    val task = TaskParameter(
      timingWidth = 5, refWidth = 23,
      cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024
    )
    val bmbp = BmbParameter(
      addressWidth = sdram.byteAddressWidth + log2Up(cfg.chipSelectNumber),
      dataWidth = cfg.beatWidth,
      sourceWidth = 0, contextWidth = 2, lengthWidth = 10,
      alignment = BmbParameter.BurstAlignement.WORD
    )
    SpinalVerilog(DfiController(bmbp, task, cfg, RowBankColumn))
    println("GEN_MC_DONE")
  }
}
