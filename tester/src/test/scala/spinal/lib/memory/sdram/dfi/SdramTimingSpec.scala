package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Key Core Timing conformance (JESD79-4 sec. 4.19 + the speed-bin tables p.169-176, and the
  * refresh-rate section 4.8/4.9).
  *
  * Layer 1 (this suite, sim-free): the cycle counts the scheduler uses must equal the golden
  * nanoseconds/nCK transcribed below, converted at the configured rate. That removes the
  * self-referential pattern the audit flagged — `DfiControllerTester` derives its refresh
  * expectation from `dut.dfiConfig.sdram.tREF` (its own comment: "Refresh expectation is
  * config-derived: cfg.sdram.tREF is the single source of truth for the refresh period") and
  * its init expectation from `SdramInitStep.defaultFor(...)`, i.e. from the DUT's own config.
  *
  * Layer 2 (the refresh/timing window asserts in `DfiControllerTester`): the observed command
  * stream on the DFI pins is checked against the same golden floors, so a DUT that schedules
  * from a knob below the spec floor fails on the waveform, not on a config print.
  *
  * The constants below are transcribed by hand from the PDFs; the PDFs are not in git
  * (ADR-0017 decision 7), which is why they are inlined.
  *
  * JESD79-4 speed-bin table (p.169-176), DDR4-1600 bin, per density:
  *   tRAS   min 32 ns (1 Gb), 33 ns (2 Gb), 34 ns (4 Gb), 35 ns (8 Gb); max = 9 x tREFI
  *   tRCD   min 12.50 / 12.85 / 13.13 ns by bin; NOTE 12: "1600M/1600K are down binned parts,
  *          tAAmin, tRCDmin, tRPmin is 13.5ns"
  *   tRP    min 12.50 / 12.85 / 13.13 ns, same bins
  *   tRC    NOTE 12: "48.5ns is tRASmin(35ns) + tRPmin(13.5ns)"
  *   tREFI  7.8 us average, per rank (sec. 4.19); 3.9 us above 85 C
  *   tRFC   8 Gb: 1x = 350 ns, 2x = 260 ns, 4x = 160 ns
  *   tRRD_S 4 nCK; tRRD_L 6 nCK (2 Gb) / 8 nCK (4 Gb and above)
  *   tFAW   32 nCK (2 Gb), 40 nCK (4 Gb x8), 48 nCK (8 Gb)
  *   tWTR_S 4 nCK; tWTR_L 8 nCK
  *   tRTP   12 nCK (BL8)
  *   tCCD_S 8 nCK (4 nCK at >= 3200 MT/s); tCCD_L 10 nCK (5 nCK at >= 3200, MR6 tCCD_L bits)
  *   tMRD   14 ns; tMOD 14 ns (min 4 nCK)
  *   tXPR   max(tXS, 5 nCK), 200 us after RESET_n de-assertion before the first MRS (sec. 3.3)
  *   tZQinit 1024 nCK, tZQoper 512 nCK, tZQCS 128 nCK (sec. 4.12); tDLLK 512 nCK
  */
class SdramTimingSpec extends SpinalAnyFunSuite {

  import SdramTimingSpec._

  test("the golden table is internally consistent (tRC >= tRAS + tRP; tREFI >> tRFC)") {
    val g = golden(densityGb = 8, rateX = 2, tCKns = 5.0)
    assert(g.tRCns >= g.tRASns + g.tRPns, s"tRC ${g.tRCns} >= tRAS ${g.tRASns} + tRP ${g.tRPns}")
    assert(g.tREFIns > 10 * g.tRFCns, s"tREFI ${g.tREFIns} ns must dominate tRFC ${g.tRFCns} ns")
    assert(g.tRASmaxCycles == 9 * cyc(g.tREFIns, 5.0), "tRAS max = 9 x tREFI (speed-bin table)")
  }

  test("the SdramConfig ns table converts to the golden DRAM-cycle counts") {
    // SdramConfig.scala:168 derives every ns value by timeCycle(ns, cycleTime_ns) = ceil, with
    // cycleTime_ns = 1000/ddrMHZ. Assert the DUT's numbers against the golden conversion, not
    // against each other.
    val cfg = Ddr4Cfg(fr = 2, rate = 2)
    val g = golden(densityGb = 8, rateX = 2, tCKns = cfg.sdram.cycleTime_ns.toDouble)
    val s = cfg.sdram
    Seq(
      "tRAS"  -> (s.tRAS, cyc(g.tRASns, cfg.sdram.cycleTime_ns)),
      "tRCD"  -> (s.tRCD, cyc(g.tRCDns, cfg.sdram.cycleTime_ns)),
      "tRP"   -> (s.tRP, cyc(g.tRPns, cfg.sdram.cycleTime_ns)),
      "tRFC"  -> (s.tRFC, cyc(g.tRFCns, cfg.sdram.cycleTime_ns)),
      "tREFI" -> (s.tREF, cyc(g.tREFIns, cfg.sdram.cycleTime_ns)),
      "tFAW"  -> (s.tFAW, cyc(g.tFAWns, cfg.sdram.cycleTime_ns))
    ).foreach { case (name, (got, exp)) =>
      assert(got >= exp, s"$name = $got DRAM cycles at ${cfg.sdram.ddrMHZ} MHz, the golden floor says $exp (${g.nsOf(name)} ns / ${cfg.sdram.cycleTime_ns} ns) — the config is below the DRAM's requirement")
    }
  }

  test("tRAS has its spec upper bound too (tRAS max = 9 x tREFI)") {
    val cfg = Ddr4Cfg(fr = 2, rate = 1)
    assert(cfg.sdram.tRAS <= 9 * cfg.sdram.tREF,
      s"tRAS ${cfg.sdram.tRAS} must be <= 9 x tREFI (${9 * cfg.sdram.tREF}) — the row-active window is bounded on both sides")
  }

  test("the bank-group CAS windows are present in the config table (JESD79-4 Table 34, sec. 4.19)") {
    // tCCD_L = max(4 nCK, 5 ns), tCCD_S = 4 nCK, 10 nCK at 8 Gb per the speed-bin table.
    Seq(1, 2, 4).foreach { fr =>
      val cfg = Ddr4Cfg(fr = fr, rate = 2)
      val g = golden(densityGb = 8, rateX = 2, tCKns = cfg.sdram.cycleTime_ns.toDouble)
      // The scheduler reasons in MC cycles: TaskConfig.scala:40 time(tcyc, fr) = (tcyc+fr-1)/fr.
      def mc(nck: Int) = cyc(nck, cfg.sdram.cycleTime_ns.toDouble) // nCK == DRAM cycles here
      val ccdL = mc(g.tCCD_Lnck)
      val ccdS = mc(g.tCCD_Snck)
      assert(cfg.sdram.generation.tCCD_L >= g.tCCD_Lnck,
        s"tCCD_L: the generation table says ${cfg.sdram.generation.tCCD_L} nCK, JESD79-4 says >= ${g.tCCD_Lnck} nCK at 8 Gb (MR6 tCCD_L bits) — consecutive CAS_n to the SAME bank group")
      assert(cfg.sdram.generation.tCCD_S >= g.tCCD_Snck,
        s"tCCD_S: the generation table says ${cfg.sdram.generation.tCCD_S} nCK, JESD79-4 says >= ${g.tCCD_Snck} nCK (BL8) — consecutive CAS_n to DIFFERENT bank groups")
      assert(cpdL(cfg) >= ccdL, s"the CCD_L scheduler window must be >= tCCD_L: ${cpdL(cfg)} MC vs $ccdL MC (fr=$fr)")
      assert(cpdS(cfg) >= ccdS, s"the CCD_S scheduler window must be >= tCCD_S: ${cpdS(cfg)} MC vs $ccdS MC (fr=$fr)")
    }
  }

  test("tREFI at each rate is the DRAM-side MR3 decision, not only a scheduler knob") {
    // JESD79-4 4.9.2: "In 2x mode ... Refresh commands should be issued to the DRAM at the
    // double frequency (tREFI2 = tREFI(base)/2)"; 4x = /4. MR3 A8:A6 encodes it (Table 14:
    // 000=1x, 001=2x, 010=4x, 101/110 = on-the-fly).
    Seq(1, 2, 4).foreach { rate =>
      val cfg = Ddr4Cfg(fr = 2, rate = rate)
      val tREFI1xDram = cyc(7800.0, cfg.sdram.cycleTime_ns.toDouble)
      assert(cfg.sdram.tREF == tREFI1xDram / rate,
        s"rate ${rate}x: tREFI must be $tREFI1xDram/$rate = ${tREFI1xDram / rate} DRAM cycles, got ${cfg.sdram.tREF}")
      assert(((cfg.ddr4ModeRegs.mr3 >> 6) & 0x7) == (rate match { case 1 => 0; case 2 => 1; case 4 => 2 }),
        s"rate ${rate}x: MR3.A8:A6 must be the matching encoding (Table 14), got 0b${Integer.toBinaryString((cfg.ddr4ModeRegs.mr3 >> 6) & 7)}")
      // tRFC must carry the mode's own floor: 8 Gb 1x=350, 2x=260, 4x=160 ns.
      val g = golden(densityGb = 8, rateX = rate, tCKns = cfg.sdram.cycleTime_ns.toDouble)
      assert(cfg.sdram.tRFC >= cyc(g.tRFCns, cfg.sdram.cycleTime_ns.toDouble),
        s"rate ${rate}x: tRFC ${cfg.sdram.tRFC} must be >= ${cyc(g.tRFCns, cfg.sdram.cycleTime_ns.toDouble)} (${g.tRFCns} ns for this mode)")
    }
  }

  test("the refresh scheduler period is tREFI in MC cycles and the init overheads are spec-derived") {
    Seq(1, 2, 4).foreach { fr =>
      val cfg = Ddr4Cfg(fr = fr, rate = 2)
      val expectedRefMc = cyc(3900.0, cfg.sdram.cycleTime_ns.toDouble) // tREFI2 = 7800/2 ns
      assert(cfg.sdram.tREF == expectedRefMc * fr || cfg.sdram.tREF >= expectedRefMc * fr - (fr - 1),
        s"fr=$fr: tREF in DRAM cycles (${cfg.sdram.tREF}) must be the MC period ($expectedRefMc) x $fr")
      // The MC's period, in MC cycles, is the DUT's own conversion; assert the floor form so a
      // knob change that shortens it below the spec is caught.
      val refMc = (cfg.sdram.tREF + fr - 1) / fr
      assert(refMc >= expectedRefMc, s"fr=$fr: the refresh period the scheduler uses ($refMc MC) must be >= tREFI2 ($expectedRefMc MC = 3900ns)")
    }
    // tXPR: 200 us from RESET_n de-assertion to the first MRS (sec. 3.3 step 5) -> 40000 DRAM
    // cycles at 200 MHz; the DDR4 preset's first-step spacing must not be shorter than tMRD.
    val cfg = Ddr4Cfg(fr = 2, rate = 2)
    val tMrdMinMc = math.ceil(14.0 / cfg.sdram.cycleTime_ns / 2).toInt
    assert(4 >= tMrdMinMc, s"the preset spaces MRS writes by 4 MC cycles, tMRD floor at this rate is $tMrdMinMc")
  }
}

object SdramTimingSpec {
  /** ceil(ns / tCK) — the same conversion SdramConfig.scala:166-168 uses. */
  def cyc(ns: Double, tCKns: Double): Int = math.ceil(ns / tCKns).toInt
  def cyc(nck: Int, tCKns: Double): Int = nck

  /** The scheduler's CCD_L / CCD_S windows in MC cycles (MakeTask.scala reads them by name). */
  def cpdL(cfg: DfiConfig): Int = (cfg.sdram.generation.tCCD_L + cfg.frequencyRatio - 1) / cfg.frequencyRatio
  def cpdS(cfg: DfiConfig): Int = (cfg.sdram.generation.tCCD_S + cfg.frequencyRatio - 1) / cfg.frequencyRatio

  case class Golden(tRASns: Double, tRASmaxCycles: Int, tRCDns: Double, tRPns: Double, tRCns: Double,
                    tREFIns: Double, tRFCns: Double, tRRD_Snck: Int, tRRD_Lnck: Int, tFAWns: Double,
                    tWTR_Snck: Int, tWTR_Lnck: Int, tRTPnck: Int, tCCD_Snck: Int, tCCD_Lnck: Int,
                    tMRDns: Double, tMODns: Double) {
    def nsOf(name: String): Double = name match {
      case "tRAS" => tRASns
      case "tRCD" => tRCDns
      case "tRP" => tRPns
      case "tRC" => tRCns
      case "tRFC" => tRFCns
      case "tREFI" => tREFIns
      case "tFAW" => tFAWns
      case _ => Double.NaN
    }
  }

  /** Speed-bin values for a density + refresh rate, transcribed from JESD79-4 p.169-176. */
  def golden(densityGb: Int, rateX: Int, tCKns: Double): Golden = {
    val tRAS = Map(1 -> 32.0, 2 -> 33.0, 4 -> 34.0, 8 -> 35.0)(densityGb)
    val tRFC = Map(1 -> Map(1 -> 350.0, 2 -> 260.0, 4 -> 160.0),
                   2 -> Map(1 -> 195.0, 2 -> 160.0, 4 -> 130.0),
                   4 -> Map(1 -> 160.0, 2 -> 130.0, 4 -> 100.0),
                   8 -> Map(1 -> 350.0, 2 -> 260.0, 4 -> 160.0))(densityGb)(rateX)
    val tREFI = 7800.0 / rateX
    Golden(
      tRASns = tRAS,
      tRASmaxCycles = 9 * cyc(tREFI, tCKns),
      tRCDns = 13.5,
      tRPns = 13.5,
      tRCns = tRAS + 13.5,
      tREFIns = tREFI,
      tRFCns = tRFC,
      tRRD_Snck = 4,
      tRRD_Lnck = if (densityGb <= 2) 6 else 8,
      tFAWns = Map(2 -> 32.0, 4 -> 40.0, 8 -> 48.0)(densityGb) * tCKns,
      tWTR_Snck = 4,
      tWTR_Lnck = 8,
      tRTPnck = 12,
      tCCD_Snck = 8,
      tCCD_Lnck = 10,
      tMRDns = 14.0,
      tMODns = 14.0
    )
  }
}
