package spinal.lib.memory.sdram.dfi.e2e

import spinal.core._
import spinal.lib.memory.sdram.dfi._

/**
 * Codegen: emit the DFI command encoder (DfiLegacySimPhy) configured for DDR2 as
 * plain Verilog so it can be co-simulated with the ORIGINAL Micron ddr2.v in a
 * single Icarus-Verilog bench. Mirrors DfiE2eGen (DDR3) with a DDR2 cell:
 * 8 banks, 13-bit row/10-bit column (=> 13-bit chip address), RL=WL=5.
 *
 * Usage: sbt 'tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.Ddr2E2eGen'
 * Output dir taken from $DFI_E2E_GEN_DIR (set by run_ddr2.sh), else ./ ;
 * emits DfiLegacySimPhy.v there (SAME component name as the DDR3 gen — keep the
 * DDR2 build dir separate from the DDR3 one).
 */
object Ddr2E2eGen extends App {
  def buildCfg() = {
    val st = SdramTiming(
      generation = 2, RFC = 60, RAS = 14, RP = 4, RCD = 4,
      WTR = 3, WTP = 0, RTP = 3, RRD = 3, REF = 64000, FAW = 30
    )
    val sdram = SdramConfig(
      generation = SdramGeneration.DDR2,
      bgWidth = 0, cidWidth = 0, bankWidth = 3, columnWidth = 10, rowWidth = 13,
      dataWidth = 16, ddrMHZ = 333, ddrWrLat = 5, ddrRdLat = 5, sdramtime = st
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 5,
      tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0
    )
    val signalConfig = new DDR2SignalConfig(DfiFunctionConfig(), false)
    DfiConfig(
      chipSelectNumber = 1, dataSlice = 1, signalConfig = signalConfig,
      timeConfig = timeConfig, sdram = sdram
    )
  }
  val cfg = buildCfg()
  val dir = sys.env.getOrElse("DFI_E2E_GEN_DIR", ".")
  SpinalConfig(targetDirectory = dir, mode = Verilog)(new DfiLegacySimPhy(cfg))
}
