package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Mode-register conformance for the DDR4 presets (JESD79-4 Tables 11-15 + the CAS speed-bin
  * table; DFI 5.2 Table 28 for the write-leveling gate).
  *
  * The MC programs MR0..MR6 through the init micro-program
  * ([[Ddr4InitSequencer]]/[[SdramInitStep]] in `MakeTask.scala`), so a mode register is a spec
  * artifact: every bit position means something to the DRAM, and several MC behaviours are
  * *gated* on those bits (`DfiConfig.useRdDbi` reads MR5.A12, `useWrDbi` reads MR5.A11;
  * `SdramConfig.tREF` divides by `SdramTiming.REF_RATE`).
  *
  * The `Golden` object below is a test-side transcription of the bit fields; it is deliberately
  * not a re-export of anything in the DUT, so a preset that is spec-wrong cannot agree with its
  * own checker (the green-but-wrong pattern ADR-0017 exists to prevent). The PDFs are not in git
  * (ADR-0017 decision 7), so the transcription is inlined here.
  *
  * Transcription notes (JESD79-4):
  *  - MR0 (Table 11): A1:A0 burst length (00=BL8 fixed, 01=BC4/on-the-fly, 10=BC4 fixed,
  *    11=Reserved), A3 burst type (0=sequential), A8 DLL reset, A7 top-only temperature range,
  *    A6,A5,A4,A2 = CAS latency code, A5,A4 = additive latency (00=0), A17/A13/A12 RFU=0,
  *    BG1 RFU=0, BG0+BA1:BA0 = MR select.
  *  - CAS code A6,A5,A4,A2 -> CL: 0000=9, 0001=10, 0010=11, 0011=12, 0100=13, 0101=14,
  *    0110=15, 0111=16, 1000=18, 1001=20, 1010=22, 1011=24, 11xx=Reserved (speed-bin table).
  *  - MR1 (Table 12): A12 Qoff, A11 TDQS, A10:A8 RTT_NOM, A7 write leveling enable,
  *    A4:A3 additive latency (00=0).
  *  - MR2 (Table 13): A2 = CA parity enable.
  *  - MR3 (Table 14): A8:A6 = refresh rate 000=1x, 001=2x, 010=4x, 101=on-the-fly 2x,
  *    110=on-the-fly 4x; A12 reserved; A11 = MPRB; A10:A8 = MPR pull-up/pull-down/RTT.
  *  - MR4 (Table 15): A3 = TCR (temperature controlled refresh) enable.
  *  - MR5: A12 = DBI over read enable, A11 = DBI over write enable, A2:A0 = CA parity latency
  *    (tPARALY).
  *  - MR6: A12 = tCCD_L extension, A13 = DBI write capture, A10:A9 + A8:A0 = VrefDQ.
  */
class Ddr4ModeRegSpec extends SpinalAnyFunSuite {

  import Ddr4ModeRegGolden._

  test("MR0 default (0x130) decodes to the fields its own comment claims (Table 11)") {
    val mr0 = Ddr4ModeRegs().mr0
    assert(mr0 == 0x130, f"default MR0 changed: 0x$mr0%04X")
    assert(burstLength(mr0) == 8, f"MR0 A1:A0=00 => BL8 fixed (Table 11); got 0x$mr0%04X")
    assert(burstSequential(mr0), "MR0 A3=0 => sequential burst (Table 11)")
    assert(dllReset(mr0), "MR0 A8=1: the init DLL reset, required by JESD79-4 sec. 3.3 step 12")
    assert(casLatency(mr0) == Some(15), s"MR0 CAS code must decode to CL15, got ${casLatency(mr0)}")
    // AL lives in MR1 A5:A4 (Table 12), NOT in MR0 — MR0 A6:A4,A2 is the CAS Latency field
    // (Table 2). The scheduler's AL=0 assumption is a statement about MR1, which the MC writes 0.
    assert(additiveLatency(Ddr4ModeRegs().mr1) == 0, "MR1 A5:A4 = 00 => AL disabled (Table 12); the scheduler assumes AL=0")
    assert(encodeCl(15) == mr0, "the CL encoder (Table 2 field A6,A5,A4,A2, plus the A8 DLL reset it sets) must reproduce the default MR0")
  }

  test("no MR preset sets an RFU bit (Tables 11-15: 'must be programmed to 0 during MRS')") {
    // RFU on MR0/MR1: A17, A13, A12, plus BG1 in the MR-select field. JESD79-4 Table 11 marks
    // A17/A13/A12 RFU = 0; a set bit there is a programming violation the agent's MR map
    // tolerates because it only records what it sees.
    Ddr4ModeRegs().productIterator.collect { case v: Int => v }.foreach { op =>
      assert((op & 0x1E000) == 0, f"a preset OP code sets an RFU bit (A17,A13:A10 must be 0): 0x$op%04X")
    }
  }

  test("the CAS decoder covers the whole speed-bin map and rejects Reserved codes") {
    val golden = Seq(0x0 -> 9, 0x1 -> 10, 0x2 -> 11, 0x3 -> 12, 0x4 -> 13, 0x5 -> 14, 0x6 -> 15,
                     0x7 -> 16, 0x8 -> 18, 0x9 -> 20, 0xa -> 22, 0xb -> 24)
    // Table 2 keys the field as the 4-tuple (A6, A5, A4, A2) — not a contiguous slice.
    def clBits(code: Int) = ((code >> 3) & 1) << 6 | ((code >> 2) & 1) << 5 | ((code >> 1) & 1) << 4 | (code & 1) << 2
    golden.foreach { case (code, cl) => assert(casLatency(clBits(code) | (1 << 8)) == Some(cl), s"code $code => CL$cl, got ${casLatency(clBits(code) | (1 << 8))}") }
    assert(casLatency(clBits(0xc) | (1 << 8)).isEmpty, "11xx is Reserved in the speed-bin table")
  }

  test("MR1 default keeps write leveling disabled and Qoff off (Table 12)") {
    val mr1 = Ddr4ModeRegs().mr1
    assert(((mr1 >> 7) & 1) == 0, "MR1.A7 write leveling enable is 0 in the default preset")
    assert(((mr1 >> 12) & 1) == 0, "MR1.A12 Qoff=0 => output buffer enabled")
    assert(rttNom(mr1) == 0, "MR1.A10:A8 RTT_NOM=0 (RZQ/1 off) — the reference model's ODT monitor has no RTT term, so the preset must stay inert")
    // DFI 5.2 Table 28: "Write pulses sent by the PHY on receipt of a dfi_wrlvl_en command from
    // the MC (with DDR4 MR1.A7 enabled)". Nothing in this repo sets MR1.A7, so the legacy write
    // leveling handshake (io.legacyWrlvlTrigger) can only work for a user who injects MR1.A7
    // themselves. Assert the gate is at least observable, and record the gap.
    assert(writeLevelingEnable(mr1) == false,
      "MR1.A7 is not programmed by any preset: the DDR4 write-leveling precondition (DFI 5.2 " +
      "Table 28) is unmet by the built-in preset")
  }

  test("MR3 refresh-rate bits and REF_RATE must be the same decision (Table 14, sec. 4.9.2)") {
    // JESD79-4 Table 14: A8:A6 = 000 1x, 001 fixed 2x, 010 fixed 4x, 101 on-the-fly 2x,
    // 110 on-the-fly 4x; the scheduler divides tREFI by SdramTiming.REF_RATE (SdramConfig.tREF).
    assert(refreshRateOf(Ddr4ModeRegs().mr3) == 1, "default MR3 = normal 1x refresh")
    Seq(1 -> 0x000, 2 -> 0x040, 4 -> 0x080).foreach { case (rate, bits) =>
      assert(refreshRateOf(bits) == rate, f"MR3.A8:A6=0b${Integer.toBinaryString(bits >> 6)} => ${rate}x")
    }
    assert(refreshRateOf(0x140) == -1, "101 = on-the-fly 2x: the MC has no per-command rate path, so it must not be read as a fixed rate")
    // Cross-check: a config that shortens tREFI by REF_RATE=2 must also program MR3=001, or the
    // DRAM and the MC disagree about the refresh period (tREFI2 = tREFI/2, sec. 4.9.2).
    Seq(1, 2, 4).foreach { rate =>
      val cfg = Ddr4Cfg(fr = 2, rate = rate)
      assert(cfg.sdram.tREF == 1560 / rate, s"tREFI at ${rate}x must be ${1560 / rate} DRAM cycles (7800ns/5ns/${rate}), got ${cfg.sdram.tREF}")
      assert(refreshRateOf(cfg.ddr4ModeRegs.mr3) == rate,
        s"REF_RATE=$rate shortens the MC's tREFI to ${cfg.sdram.tREF} cycles, but MR3.A8:A6=" +
        f"0b${Integer.toBinaryString((cfg.ddr4ModeRegs.mr3 >> 6) & 7)} tells the DRAM ${refreshRateOf(cfg.ddr4ModeRegs.mr3)}x — non-conformant")
    }
  }

  test("MR4 TCR is disabled and the MC has no derating path (Table 15)") {
    val mr4 = Ddr4ModeRegs().mr4
    assert(((mr4 >> 3) & 1) == 0, "MR4.A3 TCR must stay 0: the scheduler has no 3.9us derating path")
    // JESD79-4 sec. 4.8: above 85 C the DRAM requires tREFI halved; TCR off means the *system*
    // must sense temperature and re-program. Nothing in the repo does, so the gate's refresh
    // conformance is scoped to the normal-temperature 7.8us figure.
    assert(tcrEnable(mr4) == false)
  }

  test("MR5 DBI bits are what gates the MC datapath (Table 15, sec. 4.11)") {
    Seq(0x0000, 0x1000, 0x0800, 0x1800).foreach { mr5 =>
      val cfg = Ddr4Cfg(fr = 2, mr5 = mr5)
      val rdBit = (mr5 >> 12) & 1
      val wrBit = (mr5 >> 11) & 1
      assert(cfg.useRdDbi == (rdBit == 1), s"MR5.A12=$rdBit must gate useRdDbi (${cfg.useRdDbi})")
      assert(cfg.useWrDbi == (wrBit == 1), s"MR5.A11=$wrBit must gate useWrDbi (${cfg.useWrDbi})")
      assert(dbiRead(mr5) == (rdBit == 1) && dbiWrite(mr5) == (wrBit == 1), "the golden decoder must agree with the gate")
    }
  }

  test("MR2/MR6 defaults keep CA parity and tCCD_L extension off (Table 13, Table 16)") {
    val regs = Ddr4ModeRegs()
    assert(caParityEnable(regs.mr2) == false, "MR2.A2 CA parity enable = 0 in the default preset")
    assert(((regs.mr6 >> 12) & 1) == 0, "MR6.A12 tCCD_L extension = 0 (tCCD_L = 4 nCK, JESD79-4 Table 34)")
    assert(((regs.mr6 >> 13) & 1) == 0, "MR6.A13 DBI write capture = 0")
    // The MC gates dfi_parity on SIGNAL PRESENCE only (DfiConfig: useCaParity =
    // generation.hasCaParity && signalConfig.useParity), NOT on MR2.A2 — asymmetric with the
    // MR-gated DBI path. A board that turns on dfi_parity without MR2.A2 gets parity the DRAM
    // ignores, and no test can see it. Recorded as ADR-0017 finding F7.
    val cfg = Ddr4Cfg(fr = 2, caParity = true)
    assert(cfg.useCaParity, "the signal-level gate is on")
    assert(caParityEnable(cfg.ddr4ModeRegs.mr2) == false,
      "MR2.A2 is not programmed by any preset: the parity gate is signal-presence only, " +
      "so MC-generated parity is not enabled on a default-configured DRAM (asymmetric with " +
      "the MR5-gated DBI path)")
  }
}

/**
  * Test-side golden decoder for the DDR4 mode registers (JESD79-4 Tables 11-16). Not derived
  * from the DUT. Bit positions are the A-bit numbers the tables use; the MR-select field
  * (BG0, BA1:BA0) is carried in the command's bank field, not in the OP code, so it is out of
  * these masks.
  */
object Ddr4ModeRegGolden {
  private val clMap = Map(0x0 -> 9, 0x1 -> 10, 0x2 -> 11, 0x3 -> 12, 0x4 -> 13, 0x5 -> 14,
                          0x6 -> 15, 0x7 -> 16, 0x8 -> 18, 0x9 -> 20, 0xa -> 22, 0xb -> 24)
  def casField(mr0: Int): Int = ((mr0 >> 6) & 1) << 3 | ((mr0 >> 5) & 1) << 2 | ((mr0 >> 4) & 1) << 1 | ((mr0 >> 2) & 1)
  def casLatency(mr0: Int): Option[Int] = clMap.get(casField(mr0))
  /** Build an MR0 OP code with only the CAS code and the DLL-reset bit set. */
  def encodeCl(cl: Int): Int = {
    val code = clMap.collectFirst { case (c, `cl`) => c }.get
    (code & 1) << 2 | (code >> 1 & 1) << 4 | (code >> 2 & 1) << 5 | (code >> 3 & 1) << 6 | (1 << 8)
  }
  def burstLength(mr0: Int): Int = (mr0 & 0x3) match { case 0 => 8; case 1 => -1; case 2 => 4; case _ => -1 }
  def burstSequential(mr0: Int): Boolean = ((mr0 >> 3) & 1) == 0
  def dllReset(mr0: Int): Boolean = ((mr0 >> 8) & 1) == 1
  def additiveLatency(mr0: Int): Int = (mr0 >> 4) & 0x3
  def writeLevelingEnable(mr1: Int): Boolean = ((mr1 >> 7) & 1) == 1
  def rttNom(mr1: Int): Int = (mr1 >> 8) & 0x7
  def caParityEnable(mr2: Int): Boolean = ((mr2 >> 2) & 1) == 1
  /** 1/2/4 for the fixed encodings; -1 for on-the-fly/reserved (no fixed scheduler meaning). */
  def refreshRateOf(mr3: Int): Int = (mr3 >> 6) & 0x7 match {
    case 0 => 1
    case 1 => 2
    case 2 => 4
    case _ => -1
  }
  def tcrEnable(mr4: Int): Boolean = ((mr4 >> 3) & 1) == 1
  def dbiRead(mr5: Int): Boolean = ((mr5 >> 12) & 1) == 1
  def dbiWrite(mr5: Int): Boolean = ((mr5 >> 11) & 1) == 1
  def caParityLatency(mr5: Int): Int = mr5 & 0x7
}

/**
  * DUT-side DDR4 config factory for the conformance suites, following the canonical recipe the
  * existing specs use (SdramCmdTruthTableSpec.scala:14-30), with the refresh-rate, DBI and CA
  * parity axes exposed.
  *
  * `rate` sets BOTH halves of the refresh-rate decision: `SdramTiming.REF_RATE` (the MC side,
  * divides tREFI) and MR3.A8:A6 (the DRAM side, JESD79-4 Table 14). tREFI is the absolute
  * `REF_NS` = 7800 ns, not the legacy per-row `REF` (us) derivation
  * (SdramConfig.scala:116-122: REF_NS=0 falls back to `REF*ddrMHZ/rowSize`).
  */
object Ddr4Cfg {
  def timing(rate: Int): SdramTiming = SdramTiming(
    generation = 4, RFC = rate match { case 1 => 350; case 2 => 260; case _ => 160 }, // JESD79-4 8 Gb tRFC: 1x=350, 2x=260, 4x=160 ns
    RAS = 35, RP = 14, RCD = 14, WTR = 8, WTP = 0, RTP = 12, RRD = 6,
    REF = 7800, FAW = 240,
    REF_NS = 7800, REF_RATE = rate)

  def apply(fr: Int,
            rate: Int = 1,
            ranks: Int = 1,
            ddrMHz: Int = 200,
            mr0: Int = 0x130,
            mr1: Int = 0x000,
            mr2: Int = 0x000,
            mr5: Int = 0x0000,
            caParity: Boolean = false,
            rowWidth: Int = 17,
            dataWidth: Int = 16,
            initSteps: Option[List[SdramInitStep]] = None): DfiConfig = {
    val bgWidth = if (fr >= 4) 2 else 1
    val cidWidth = if (ranks > 2) 2 else 1
    val sdram = SdramConfig(SdramGeneration.DDR4, bgWidth = bgWidth, cidWidth = cidWidth, bankWidth = 4,
      columnWidth = 10, rowWidth = rowWidth, dataWidth = dataWidth, ddrMHZ = ddrMHz,
      ddrWrLat = 12, ddrRdLat = 12, sdramtime = timing(rate))
    val timeConfig = DfiTimeConfig(
      frequencyRatio = fr, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsLat = 0, tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 4,
      tPhyRdCslat = 0, tPhyRdCsGap = 3)
    val mr3 = rate match { case 2 => 0x040; case 4 => 0x080; case _ => 0x000 }
    DfiConfig(
      chipSelectNumber = ranks, dataSlice = 1,
      signalConfig = new DDR4SignalConfig(DfiFunctionConfig(useStatusSignals = caParity), false) {
        override val useParityIn = caParity
        override val useAlertN = caParity
      },
      timeConfig = timeConfig, sdram = sdram,
      ddr4ModeRegs = Ddr4ModeRegs(mr0 = mr0, mr1 = mr1, mr2 = mr2, mr3 = mr3, mr5 = mr5),
      initSteps = initSteps)
  }
}
