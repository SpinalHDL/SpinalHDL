package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi._
import java.io.File
import java.nio.file.Files

/** Phase-2 config-matrix elaboration smoke (ratified 2026-10-31, TASK.md plan).
  *
  * Proves the MC+PHY framework contract elaborates (SpinalVerilog) for
  * configurations beyond the TC10/TC11 golden point (x16/fr=2/nranks=2):
  *   - x32_fr2    : dataWidth=32, frequencyRatio=2, nranks=2
  *   - x16_fr2nr1 : dataWidth=16, frequencyRatio=2, nranks=1
  *
  * Elaboration FAILURE = framework bug (fix in scope). If a fix requires NEW
  * hardware semantics, stop and ask the user (hardware scope is frozen).
  *
  * Output goes to a throw-away temp dir — canonical netlists at the repo
  * root (UsPhy.v / DfiController.v) are never touched. No xsim: this is an
  * elaboration-level smoke only.
  */
object E2eSmokeGen {
  private def mkSdram(dataWidth: Int, ddrMHZ: Int = 200): SdramConfig = {
    val sdramtime = SdramTiming(
      generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15,
      WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35
    )
    SdramConfig(
      SdramGeneration.DDR4,
      bgWidth = 1, cidWidth = 1, bankWidth = 4,
      columnWidth = 10, rowWidth = 13,
      dataWidth = dataWidth, ddrMHZ = ddrMHZ, ddrWrLat = 4, ddrRdLat = 4,
      sdramtime = sdramtime
    )
  }

  private def mkCfg(dataWidth: Int, nranks: Int, ddrMHZ: Int = 200, mr5: Int = 0, useRddataDbiN: Boolean = false): DfiConfig = {
    val sdram = mkSdram(dataWidth, ddrMHZ)
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2, cmdPhase = 0,
      // tPhyWrData = 0: e2e-verified value (see E2eMcGen) — UsPhy implements
      // the zero-extra-data-delay DFI contract.
      tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 1, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val signalConfig = if (useRddataDbiN) new DDR4SignalConfig(DfiFunctionConfig(useTrainingSignals = true), false) {
      override val useRddataDbiN = true
    } else new DDR4SignalConfig(DfiFunctionConfig(useTrainingSignals = true), false) {}
    DfiConfig(
      chipSelectNumber = nranks, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(mr5 = mr5),
      initSteps = None, bootTraining = false
    )
  }

  private def mkCfgFr(dataWidth: Int, nranks: Int, fr: Int): DfiConfig = {
    val sdram = mkSdram(dataWidth)
    val timeConfig = DfiTimeConfig(
      frequencyRatio = fr, cmdPhase = 0,
      tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 1, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val signalConfig = new DDR4SignalConfig(DfiFunctionConfig(useTrainingSignals = true), false) {}
    DfiConfig(
      chipSelectNumber = nranks, dataSlice = 1,
      signalConfig = signalConfig, timeConfig = timeConfig,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(),
      initSteps = None, bootTraining = false
    )
  }

  private def rmTree(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles()).getOrElse(Array.empty).foreach(rmTree)
    f.delete()
  }

  def main(args: Array[String]): Unit = {
    val base = Files.createTempDirectory("e2e_smoke").toFile
    try {
      for ((tag, dw, nr) <- Seq(("x32_fr2", 32, 2), ("x16_fr2nr1", 16, 1))) {
        val c = mkCfg(dw, nr)
        // PHY side
        SpinalVerilog(SpinalConfig(targetDirectory = base.getAbsolutePath, mode = Verilog))(UsPhy(c))
        // MC side (mirrors E2eMcGen wiring)
        val task = TaskParameter(
          timingWidth = 5, refWidth = 23,
          cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024
        )
        val bmbp = BmbParameter(
          addressWidth = c.sdram.byteAddressWidth + log2Up(c.chipSelectNumber),
          dataWidth = c.beatWidth,
          sourceWidth = 0, contextWidth = 2, lengthWidth = 10,
          alignment = BmbParameter.BurstAlignement.WORD
        )
        SpinalVerilog(SpinalConfig(targetDirectory = base.getAbsolutePath, mode = Verilog))(DfiController(bmbp, task, c, RowBankColumn))
        println(s"SMOKE_${tag}_OK")
      }
      // Permanent negative proof: the documented fr>=2 boundary (UsPhy.scala
      // require) must still be ENFORCED — a silent acceptance would mean the
      // boundary quietly disappeared. Expect the elaboration to blow up.
      val bad = mkCfgFr(16, 2, 1)
      var negativeOk = false
      try {
        SpinalVerilog(SpinalConfig(targetDirectory = base.getAbsolutePath, mode = Verilog))(UsPhy(bad))
      } catch {
        case e: Throwable => negativeOk = e.toString.toLowerCase.contains("frequencyratio") || Option(e.getMessage).exists(_.toLowerCase.contains("frequencyratio"))
      }
      if (!negativeOk) throw new RuntimeException("SMOKE_NEGATIVE_FAIL: fr=1 elaboration unexpectedly succeeded (boundary require gone?)")
      println("SMOKE_negative_fr1_rejected_OK")
      // Conformance-audit negative proof (item 2): tWR must scale with speed.
      // At ddrMHZ=800 (DDR4-3200, 1.25ns nominal cycle) the JESD79-4B AC minimum
      // (15ns) demands >= 12 cycles; the old hardcoded 6-cycle constant violated
      // this and would be caught here.
      val fast = mkSdram(16, ddrMHZ = 800)
      if (fast.tWR < 12) throw new RuntimeException(s"SMOKE_TWRFLOOR_FAIL: tWR=${fast.tWR} < 12 cycles at ddrMHZ=800 (spec min 15ns)")
      println(s"SMOKE_twrfloor_ok (tWR@800MHz=${fast.tWR} cycles)")
      // Conformance-audit item 3 (DBI/DM wire-through, 2026-10-31): a DBI-enabled
      // config (mr5 A10+A11) must ELABORATE — the old loud-failure guard is retired
      // because UsPhy now serializes the DM/DBI pin in lockstep with DQ and reads
      // rddataDbiN back from the DRAM-returned DM pad.
      SpinalVerilog(SpinalConfig(targetDirectory = base.getAbsolutePath, mode = Verilog))(UsPhy(mkCfg(16, 2, mr5 = 0x1800, useRddataDbiN = true)))
      println("SMOKE_positive_dbielaborates_OK")
      println("SMOKE_ALL_OK")
    } finally {
      rmTree(base)
    }
  }
}
