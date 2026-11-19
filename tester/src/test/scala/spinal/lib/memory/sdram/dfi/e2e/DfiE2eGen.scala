package spinal.lib.memory.sdram.dfi.e2e

import spinal.core._
import spinal.lib.memory.sdram.dfi._

/**
 * Codegen: emit the DFI command encoder (DfiLegacySimPhy) as plain Verilog so it can be
 * co-simulated with the ORIGINAL Micron ddr3.v in a single Icarus-Verilog bench.
 * No Verilator, no model patching. The generated .v is our own RTL (regenerable).
 *
 * Usage: sbt 'tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.DfiE2eGen'
 * Output dir taken from $DFI_E2E_GEN_DIR (set by run.sh), else ./ ; emits DfiLegacySimPhy.v there.
 */
object DfiE2eGen extends App {
  def buildCfg() = {
    val st = SdramTiming(
      generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15,
      WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35
    )
    val sdram = SdramConfig(
      generation = SdramGeneration.DDR3,
      bgWidth = 0, cidWidth = 0, bankWidth = 3, columnWidth = 10, rowWidth = 13,
      dataWidth = 16, ddrMHZ = 400, ddrWrLat = 4, ddrRdLat = 4, sdramtime = st
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 4,
      tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0
    )
    val signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false)
    DfiConfig(
      chipSelectNumber = 1, dataSlice = 1, signalConfig = signalConfig,
      timeConfig = timeConfig, sdram = sdram
    )
  }
  val cfg = buildCfg()
  val dir = sys.env.getOrElse("DFI_E2E_GEN_DIR", ".")
  SpinalConfig(targetDirectory = dir, mode = Verilog)(new DfiLegacySimPhy(cfg))
}
