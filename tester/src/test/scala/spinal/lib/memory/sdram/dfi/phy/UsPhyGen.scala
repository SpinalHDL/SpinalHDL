package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._

object UsPhyGen {
  // Optional arg: mr5 hex override (e.g. "0x1800" = write+read DBI enabled).
  def main(args: Array[String]): Unit = {
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
      // tPhyWrData = 0: UsPhy samples dfi_wrdata in lockstep with dfi_wrdata_en
      // (its write path implements only tPhyWrData == 0 — see the UsPhy require).
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
    SpinalVerilog(SpinalConfig(targetDirectory = "./simWorkspace", mode = Verilog))(UsPhy(cfg))
    println("GEN_DONE")
  }
}
