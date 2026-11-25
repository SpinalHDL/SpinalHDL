package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._

/** Elaboration gate for the new UsPhy (Steps 1-3 skeleton).
  * Constructs a legal DDR4 DfiConfig (verbatim from the verified probe),
  * elaborates UsPhy to Verilog, and prints PROBE_OK on success.
  * Run: sbt -batch "tester/Test/runMain spinal.lib.memory.sdram.dfi.phy.UsPhyElab"
  */
object UsPhyElab {
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
    val cfg = DfiConfig(
      chipSelectNumber = 2, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(),
      initSteps = None, bootTraining = false
    )

    SpinalVerilog(UsPhy(cfg))
    println("PROBE_OK: UsPhy elaboration succeeded")
    println("  addressWidth = " + cfg.addressWidth)
    println("  dataWidth    = " + cfg.dataWidth)
    println("  beatWidth    = " + cfg.beatWidth)
    println("  chipSelNum   = " + cfg.chipSelectNumber)
    println("  freqRatio    = " + cfg.frequencyRatio)
    println("  byteLanes    = " + (cfg.sdram.dataWidth / 8))
  }
}