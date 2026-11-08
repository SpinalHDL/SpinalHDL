package spinal.lib.memory.sdram.dfi.e2e

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._

/** Simulation DFI -> DRAM-pin PHY (pure logic, NO SerDes black boxes).
  *
  * Purpose: e2e validation of the DFI command path against a behavioral DRAM model
  * (Micron ddr3.v / ddr2.v). Reuses UsPhy's proven command-path idiom (select the
  * packed DFI control bus by the DRAM-clock phase), but replaces UsPhy's UltraScale
  * OSERDESE3/ISERDESE3 data path with plain logic so it actually simulates.
  *
  * Simplification vs. the real PHY: the DRAM-pin outputs are driven COMBINATORIALY
  * from DFI phase slot 0 (not registered). The e2e testbench drives both DFI phase
  * slots identically, so the phase select collapses to slot 0 and no SerDes
  * registration is needed. Combinatorial outputs are reset-safe (no spurious
  * CKE-active-at-reset) and zero-latency, which the behavioral model's strict
  * init-sequence checks require.
  *
  * This is a VALIDATION vehicle, not a fabricatable PHY (no SerDes, no delay-line
  * training). The real 7-series deployment PHY (Kintex-7/Artix) is a separate
  * artifact (Phase 4).
  *
  * v1 supports fr = 2 (matches UsPhy's requirement and all in-repo DDR2/DDR3 cells).
  */
class DfiLegacySimPhy(cfg: DfiConfig) extends Component {
  val io = new Bundle {
    val mcClk   = in Bool()   // DFI / MC clock (kept for interface parity)
    val mcRst   = in Bool()   // reset (active high; kept for interface parity)
    val dramClk = in Bool()   // DRAM clock = fr * mcClk, phase-locked
    val control = in(new DfiControlInterface(cfg))

    // DRAM pins (subset matching Micron ddr3.v / ddr2.v):
    val ck     = out Bool()
    val ck_n   = out Bool()
    val cke    = out Bool()
    val cs_n   = out Bool()
    val ras_n  = out Bool()
    val cas_n  = out Bool()
    val we_n   = out Bool()
    val odt    = out Bool()
    val ba     = out Bits(cfg.dfiBankWidth bits)
    val addr   = out Bits(cfg.addressWidth bits)
  }

  val fr = cfg.frequencyRatio
  require(fr == 2, s"DfiLegacySimPhy v1 supports fr=2 (got $fr)")

  // Slot-0 (combinational) select of each packed DFI control field. The TB drives
  // both phase slots identically, so slot 0 carries the intended value.
  def slot0(field: Bits): Bits = field.take(field.getWidth / fr)

  io.ck     := io.dramClk
  io.ck_n   := !io.dramClk
  io.ras_n  := (if (cfg.useRasN) slot0(io.control.rasN)(0) else True)
  io.cas_n  := (if (cfg.useCasN) slot0(io.control.casN)(0) else True)
  io.we_n   := (if (cfg.useWeN)  slot0(io.control.weN)(0)  else True)
  io.cs_n   := slot0(io.control.csN)(0)
  io.cke    := slot0(io.control.cke)(0)
  io.odt    := (if (cfg.useOdt) slot0(io.control.odt)(0) else True)
  io.ba     := slot0(io.control.bank).take(cfg.dfiBankWidth)
  io.addr   := slot0(io.control.address).take(cfg.addressWidth)
}
