package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Golden conformance for the LEGACY (DDR2 / DDR3) command encoding on the DFI control bus.
  *
  * Unlike the DDR4 [[SdramCmdTruthTable16Spec]] (JESD79-4B Table 16, actN-flagged), the legacy
  * command bus has NO actN pin: a command is the 3-bit (RAS_n, CAS_n, WE_n) level pattern, and
  * the chip is selected by the per-rank CS_n one-hot. Per ADR-0008 the encoding lives only in
  * the MC, so this suite drives [[CmdTxd]] directly and samples the raw DFI pins — no PHY, no
  * traffic, and deliberately no [[DfiMemoryAgent]] decoder in the loop (a shared decoder would
  * make encoder and checker agree on the same wrong assumption).
  *
  * Oracle: the deployment target is a real DDR2/DDR3 part on a 7-series FPGA. The authoritative
  * in-repo silicon reference is the vendor behavioural model the deployment is validated
  * against — tester/src/test/python/spinal/SdramXdr/Ddr3ModelTester/ddr3.v and
  * Ddr2ModelTester/ddr2.v. Both define the command as {cs_n, ras_n, cas_n, we_n} (all active-low,
  * 1 = pin HIGH) with the identical encoding:
  *
  *   parameter  // {cs, ras, cas, we}
  *     LOAD_MODE = 4'b0000,  // MRS / EMRS: bank = MR/EMRS number, address = OP code
  *     REFRESH   = 4'b0001,
  *     PRECHARGE = 4'b0010,
  *     ACTIVATE  = 4'b0011,
  *     WRITE     = 4'b0100,
  *     READ      = 4'b0101,
  *     [ZQ        = 4'b0110,]   // DDR3 only; DDR2 has no ZQ calibration command
  *     NOP       = 4'b0111,
  *
  * i.e. with the chip selected (cs_n = 0):
  *   ACT  (RAS_n,CAS_n,WE_n) = (0,1,1)
  *   WR   = (1,0,0)
  *   RD   = (1,0,1)
  *   PRE  = (0,1,0)   (A10/AP = L for single bank, H for precharge-all)
  *   REF  = (0,0,1)
  *   MRS  = (0,0,0)   (bank = MR/EMRS select, address = OP code)
  *   ZQCL = (1,1,0)   + A10 = H   (DDR3 only)
  *
  * The DfiMemoryAgent legacy decoder (DfiMemoryAgent.scala) recognises exactly these patterns;
  * this suite pins them independently against the model so the two cannot drift together.
  */
class SdramCmdLegacyTruthTableSpec extends SpinalAnyFunSuite {
  import spinal.core._
  import spinal.core.sim._

  /** Legacy command = the 3-bit (RAS_n, CAS_n, WE_n) level pattern, 1 = pin HIGH. */
  case class LegPin(ras: Int, cas: Int, we: Int)

  /** Micron ddr3.v / ddr2.v command encoding (deployment-target silicon reference). */
  val legacyTable: Map[String, LegPin] = Map(
    "ACT"  -> LegPin(0, 1, 1),
    "WR"   -> LegPin(1, 0, 0),
    "RD"   -> LegPin(1, 0, 1),
    "PRE"  -> LegPin(0, 1, 0),
    "PREA" -> LegPin(0, 1, 0),
    "REF"  -> LegPin(0, 0, 1),
    "MRS"  -> LegPin(0, 0, 0),
    "ZQCL" -> LegPin(1, 1, 0)
  )

  /** Commands the MC can emit on the legacy bus, keyed by the OpTasks flag that emits them. */
  val emittedOps: Set[String] = Set("ACT", "WR", "RD", "PRE", "PREA", "REF", "MRS", "ZQCL")

  private def build(generation: SdramGeneration, rowWidth: Int, ranks: Int) = new Bundle {
    val taskParam = TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024)
    val taskConfig = TaskConfig(taskParam, contextWidth = 2, writeTokenInterfaceWidth = 2, writeTokenBufferSize = 16, canRead = true, canWrite = true)
    val sdramtime = SdramTiming(generation = if (generation == SdramGeneration.DDR2) 2 else 3,
      RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
    // Legacy: no bank groups (bgWidth=0), flat bank field; bankWidth=3 (8 banks) like the legacy
    // DfiControllerTester cells.
    val sdram = SdramConfig(generation, bgWidth = 0, cidWidth = 0, bankWidth = 3, columnWidth = 10,
      rowWidth = rowWidth, dataWidth = 16, ddrMHZ = 200, ddrWrLat = 4, ddrRdLat = 4, sdramtime = sdramtime)
    val timeConfig = DfiTimeConfig(frequencyRatio = 2, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 4, tPhyRdCsGap = 3,
      tPhyRdCslat = 0, tPhyWrCsLat = 0, tWrlvlResp = 16)
    val signalConfig = if (generation == SdramGeneration.DDR2)
      new DDR2SignalConfig(DfiFunctionConfig(), false)
    else
      new DDR3SignalConfig(DfiFunctionConfig(), false)
    val dfiConfig = DfiConfig(chipSelectNumber = ranks, dataSlice = 1, signalConfig = signalConfig,
      timeConfig = timeConfig, sdram = sdram)
  }

  private case class Sample(ras: Int, cas: Int, we: Int, a10: Int, address: BigInt, bank: BigInt, csN: BigInt, validPhases: Set[Int])

  private def runEncodingCase(generation: SdramGeneration): Unit = {
    val fr = 2
    val cmdPhase = 0
    val b = build(generation, rowWidth = 15, ranks = 1)
    val genStr = if (generation == SdramGeneration.DDR2) "DDR2" else "DDR3"
    SimConfig.compile(CmdTxd(b.taskConfig, b.dfiConfig)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val cfg = b.dfiConfig

      def clrFlags(): Unit = Seq(dut.io.task.active, dut.io.task.write, dut.io.task.read, dut.io.task.precharge,
        dut.io.task.prechargeAll, dut.io.task.refresh, dut.io.task.mrs, dut.io.task.zq).foreach(_ #= false)

      clrFlags()
      dut.io.task.address.byte #= 0
      dut.io.task.address.column #= 0
      dut.io.task.address.bank #= 0
      dut.io.task.address.row #= 0
      dut.io.task.address.cs #= 0
      dut.io.task.context #= 0
      dut.io.task.initCsN #= (1 << cfg.chipSelectNumber) - 1
      dut.clockDomain.waitRisingEdge()

      def sample(): Sample = {
        val c = dut.io.cmd(cmdPhase).payload
        val a = dut.io.address(cmdPhase).payload
        assert(dut.io.cmd(cmdPhase).valid.toBoolean, s"$genStr: expected a valid command on the command phase")
        val valids = (0 until fr).filter(p => dut.io.cmd(p).valid.toBoolean).toSet
        assert(valids == Set(cmdPhase), s"$genStr: exactly one phase must carry a command, got $valids")
        assert(c.csN.toBigInt == 0, s"$genStr: csN must select rank 0, got 0x${c.csN.toBigInt.toString(16)}")
        // Negative conformance: the deselect/NOP pattern (RAS=CAS=WE=H) must never be presented
        // with the chip selected.
        assert(!(c.rasN.toBoolean && c.casN.toBoolean && c.weN.toBoolean),
          s"$genStr: encoding collapsed to NOP/DESEL (RAS=CAS=WE=H) with chip selected")
        Sample(if (c.rasN.toBoolean) 1 else 0, if (c.casN.toBoolean) 1 else 0, if (c.weN.toBoolean) 1 else 0,
          ((a.address.toBigInt >> 10) & 1).toInt, a.address.toBigInt, a.bank.toBigInt, c.csN.toBigInt, valids)
      }

      def fire(drive: => Unit): Sample = {
        clrFlags()
        drive
        dut.clockDomain.waitRisingEdge()
        val out = sample()
        clrFlags()
        dut.clockDomain.waitRisingEdge()
        out
      }

      def col(c: Int): Unit = dut.io.task.address.column #= c
      def bank(bk: Int): Unit = dut.io.task.address.bank #= bk
      def row(r: Int): Unit = dut.io.task.address.row #= r

      // ---- every emittable command: (RAS_n, CAS_n, WE_n) exactly as the model encodes it -----
      val drivers: Map[String, () => Sample] = Map(
        "ACT"  -> (() => fire { dut.io.task.active #= true; row(0x4000); bank(5) }),
        "WR"   -> (() => fire { dut.io.task.write #= true; col(3); bank(6) }),
        "RD"   -> (() => fire { dut.io.task.read #= true; col(7); bank(4) }),
        "PRE"  -> (() => fire { dut.io.task.precharge #= true; bank(2) }),
        "PREA" -> (() => fire { dut.io.task.prechargeAll #= true }),
        "REF"  -> (() => fire { dut.io.task.refresh #= true }),
        "MRS"  -> (() => fire { dut.io.task.mrs #= true; bank(1); row(0x00B0) }),
        "ZQCL" -> (() => fire { dut.io.task.zq #= true })
      )

      emittedOps.foreach { op =>
        val exp = legacyTable(op)
        val s = drivers(op)()
        assert(s.ras == exp.ras && s.cas == exp.cas && s.we == exp.we,
          s"$genStr $op: (RAS_n,CAS_n,WE_n)=(${s.ras},${s.cas},${s.we}), Micron model says (${exp.ras},${exp.cas},${exp.we})")
      }

      // ---- address / bank placement --------------------------------------------------------
      // ACT: full row on the address bus, bank field carries the flat bank index.
      val act = fire { dut.io.task.active #= true; row(0x4000); bank(5) }
      assert(act.address == 0x4000, s"$genStr ACT: address bus=0x${act.address.toString(16)}, expected the full row 0x4000")
      assert(act.bank == 5, s"$genStr ACT: bank=0x${act.bank.toString(16)}, expected 5")

      // WR / RD: column on A9:A0, A10 (auto-precharge) = L.
      val wr = fire { dut.io.task.write #= true; col(0x155); bank(6) }
      assert((wr.address & 0x3FF) == 0x155, s"$genStr WR: column must sit on A9:A0, got 0x${(wr.address & 0x3FF).toString(16)}")
      assert(wr.a10 == 0, s"$genStr WR: A10/auto-precharge must be L, got ${wr.a10}")
      assert(wr.bank == 6, s"$genStr WR: bank=0x${wr.bank.toString(16)}, expected 6")
      val rd = fire { dut.io.task.read #= true; col(0x2AA); bank(4) }
      assert((rd.address & 0x3FF) == 0x2AA, s"$genStr RD: column must sit on A9:A0, got 0x${(rd.address & 0x3FF).toString(16)}")
      assert(rd.a10 == 0, s"$genStr RD: A10/auto-precharge must be L, got ${rd.a10}")

      // PRE: single bank (A10=L); PREA: all banks (A10=H).
      val pre = fire { dut.io.task.precharge #= true; bank(3) }
      assert(pre.a10 == 0, s"$genStr PRE: single-bank precharge requires A10=L, got ${pre.a10}")
      assert(pre.bank == 3, s"$genStr PRE: bank=0x${pre.bank.toString(16)}, expected 3")
      val prea = fire { dut.io.task.prechargeAll #= true }
      assert(prea.a10 == 1, s"$genStr PREA: precharge-all requires A10=H, got ${prea.a10}")

      // REF: no column, no bank coded.
      val ref = fire { dut.io.task.refresh #= true }
      assert((ref.address & 0x3FF) == 0, s"$genStr REF: must not carry a column address, got 0x${(ref.address & 0x3FF).toString(16)}")

      // MRS: bank = MR/EMRS select, OP code on the address bus.
      Seq(0, 1, 2, 3, 4, 5, 6, 7).foreach { mr =>
        val op = 0x1000 + mr
        val s = fire { dut.io.task.mrs #= true; bank(mr); row(op) }
        assert(s.bank == mr, s"$genStr MRS MR$mr: bank must carry the MR/EMRS select=$mr, got ${s.bank}")
        assert((s.address & 0x3FF) == (op & 0x3FF), s"$genStr MRS MR$mr: OP code must be on A9:A0, got 0x${(s.address & 0x3FF).toString(16)}")
      }

      // ZQCL: A10=H (long / all-bank calibration). DDR2 presets never emit ZQ, but the encoder
      // still forms the pattern if asked; assert the encoding is correct regardless.
      val zq = fire { dut.io.task.zq #= true }
      assert(zq.a10 == 1, s"$genStr ZQCL: A10 must be H (long calibration), got ${zq.a10}")

      // ---- NOP when the MC has nothing to send ---------------------------------------------
      (0 until 8).foreach { _ =>
        clrFlags()
        dut.clockDomain.waitRisingEdge()
        val v = (0 until fr).filter(p => dut.io.cmd(p).valid.toBoolean).toSet
        assert(v.isEmpty, s"$genStr: idle must not present a command on any phase, got valid phases $v")
      }
    }
  }

  test("DDR3 legacy command encoding matches the Micron ddr3.v model") {
    runEncodingCase(SdramGeneration.DDR3)
  }

  test("DDR2 legacy command encoding matches the Micron ddr2.v model") {
    runEncodingCase(SdramGeneration.DDR2)
  }

  test("DES: unselected ranks are deselected on every phase (CS_n HIGH)") {
    val fr = 2
    val ranks = 2
    val b = build(SdramGeneration.DDR3, rowWidth = 15, ranks = ranks)
    SimConfig.compile(CmdTxd(b.taskConfig, b.dfiConfig)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      Seq(dut.io.task.active, dut.io.task.write, dut.io.task.read, dut.io.task.precharge,
        dut.io.task.prechargeAll, dut.io.task.refresh, dut.io.task.mrs, dut.io.task.zq).foreach(_ #= false)
      dut.io.task.address.byte #= 0
      dut.io.task.address.column #= 0
      dut.io.task.address.bank #= 0
      dut.io.task.address.row #= 0
      dut.io.task.context #= 0
      dut.io.task.initCsN #= (1 << ranks) - 1
      dut.clockDomain.waitRisingEdge()

      Seq(0, 1).foreach { cs =>
        dut.io.task.address.cs #= cs
        dut.io.task.precharge #= true
        dut.clockDomain.waitRisingEdge()
        val c = dut.io.cmd(0).payload
        assert(dut.io.cmd(0).valid.toBoolean, s"PRE cs=$cs: expected a valid command")
        // DfiCmd.csN is Bits(chipSelectNumber) per phase; the DES contract at this boundary is
        // that the command phase csN is one-hot active-low for the selected rank.
        val ph0 = c.csN.toBigInt & ((1 << ranks) - 1)
        assert(ph0 == (((1 << ranks) - 1) & ~(1 << cs)), s"PRE cs=$cs: csN=0x${ph0.toString(16)}, expected one-hot active-low for rank $cs")
        (1 until fr).foreach { p =>
          assert(!dut.io.cmd(p).valid.toBoolean, s"PRE cs=$cs: phase $p must be invalid (NOP/DES), got valid")
        }
        dut.io.task.precharge #= false
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
