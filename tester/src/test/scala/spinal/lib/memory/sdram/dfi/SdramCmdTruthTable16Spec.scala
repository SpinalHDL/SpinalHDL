package spinal.lib.memory.sdram.dfi

import spinal.tester.code.SpinalAnyFunSuite

/**
  * Full golden conformance for the DDR4 command encoding on the DFI control bus:
  * **every row of JESD79-4 Table 16**, plus the address/bank placement the same table
  * specifies, plus the negative rows (RFU must never be emitted, DES via CS, NOP when idle),
  * plus a feature-coverage ledger of which rows the MC is able to emit at all.
  *
  * Transcription (JESD79-4, p.24, Table 16 — "Command Truth Table"), verbatim column order
  * `CS_n ACT_n RAS_n/A16 CAS_n/A15 WE_n/A14 ... A10/AP`, 1 = pin HIGH, `V` = defined H or L,
  * `X` = don't care:
  *
  *   MRS    H L H L L L   BG BA  OP Code                              (NOTE 12)
  *   REF    H L H L L H   V  V  V V V V V
  *   SRE    H L H L L H   V  V  V V V V V                             (7,9)   CKE H->L
  *   SRX    L H H L L H   V  V  V V V V V                             (7,8,9,10) CKE L->H
  *   PRE    H L H L H L   BG BA V V V L V
  *   PREA   H L H L H L   V  V  V V V H V
  *   RFU    H L H L H H                              <- must never be emitted
  *   ACT    H L L Row Address(RA) ... BG BA V Row Address(RA)
  *   WR     H L H H L L   BG BA V V V L CA
  *   WRS4   H L H H L L   BG BA V L V L CA     (BC4 on the fly)
  *   WRS8   H L H H L L   BG BA V H V L CA     (BL8 on the fly)
  *   WRA    H L H H L L   BG BA V V V H CA
  *   WRAS4  H L H H L L   BG BA V L V H CA
  *   WRAS8  H L H H L L   BG BA V H V H CA
  *   RD     H L H H L H   BG BA V V V L CA
  *   RDS4   H L H H L H   BG BA V L V L CA
  *   RDS8   H L H H L H   BG BA V H V L CA
  *   RDA    H L H H L H   BG BA V V V H CA
  *   RDAS4  H L H H L H   BG BA V L V H CA
  *   RDAS8  H L H H L H   BG BA V H V H CA
  *   NOP    H L H H H H   V  V  V V V V V                             (10)
  *   DES    H H X X X X   X  X  X X X X X
  *   PDE    H L H X X X X X X X X X X                                 (6)   CKE H->L
  *   PDX    L H H X X X X X X X X X X                                 (6)   CKE L->H
  *   ZQCL   H L H H H L   V  V  V V V H V
  *   ZQCS   H L H H H L   V  V  V V V L V
  *
  * NOTE 1 (verbatim): "All DDR4 SDRAM commands are defined by states of CS_n, ACT_n,
  * RAS_n/A16, CAS_n/A15, WE_n/A14 and CKE at the rising edge of the clock. ... When ACT_n = H;
  * pins RAS_n/A16, CAS_n/A15, and WE_n/A14 are used as command pins RAS_n, CAS_n, and WE_n
  * respectively. When ACT_n = L; pins RAS_n/A16, CAS_n/A15, and WE_n/A14 are used as address
  * pins A16, A15, and A14 respectively."
  *
  * Per ADR-0008 the command encoding lives only in the MC, so this suite drives [[CmdTxd]]
  * directly and samples the raw DFI pins — no PHY, no traffic, and deliberately no
  * [[DfiMemoryAgent]] decoder in the loop (a shared decoder would make encoder and checker
  * agree on the same wrong assumption).
  *
  * The expected constants are transcribed by hand from the PDF; the PDF itself is NOT in git
  * (ADR-0017 decision 7), which is why the transcription above is part of this file.
  */
class SdramCmdTruthTable16Spec extends SpinalAnyFunSuite {
  import spinal.core._
  import spinal.core.sim._

  case class Pin(actN: Int, ras: Int, cas: Int, we: Int)

  /** One Table 16 row. `-1` = V / X (don't care). */
  case class Row(function: String,
                 abbrev: String,
                 pins: Pin,
                 a10: Int,
                 ckePrev: Int,
                 ckeCur: Int,
                 carriesColumn: Boolean,
                 carriesBank: Boolean,
                 carriesRow: Boolean,
                 opCode: Boolean,
                 /** The OpTasks flag that emits this row, if the MC can emit it at all. */
                 mcOp: Option[String],
                 notes: String)

  private val X = -1

  /** JESD79-4 Table 16, all rows, in table order. */
  val table16: Seq[Row] = Seq(
    Row("Mode Register Set",        "MRS",   Pin(1, 0, 0, 0), X, 1, 1, carriesColumn = false, carriesBank = true,  carriesRow = false, opCode = true,  Some("mrs"),          "12"),
    Row("Refresh",                  "REF",   Pin(1, 0, 0, 1), X, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, Some("refresh"),      ""),
    Row("Self Refresh Entry",       "SRE",   Pin(1, 0, 1, 1), X, 1, 0, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "7,9 CKE H->L"),
    Row("Self Refresh Exit",        "SRX",   Pin(1, 0, 1, 1), X, 0, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "7,8,9,10 CKE L->H"),
    Row("Single Bank Precharge",    "PRE",   Pin(1, 0, 1, 0), 0, 1, 1, carriesColumn = false, carriesBank = true,  carriesRow = false, opCode = false, Some("precharge"),    ""),
    Row("Precharge all Banks",      "PREA",  Pin(1, 0, 1, 0), 1, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, Some("prechargeAll"), ""),
    Row("RFU",                      "RFU",   Pin(1, 0, 1, 1), X, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "reserved: must never be emitted"),
    Row("Bank Activate",            "ACT",   Pin(0, X, X, X), X, 1, 1, carriesColumn = false, carriesBank = true,  carriesRow = true,  opCode = false, Some("active"),       "1"),
    Row("Write (Fixed BL8 or BC4)", "WR",    Pin(1, 1, 0, 0), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, Some("write"),        ""),
    Row("Write (BC4, on the Fly)",  "WRS4",  Pin(1, 1, 0, 0), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 "A11/A12/A13 BC_n = L"),
    Row("Write (BL8, on the Fly)",  "WRS8",  Pin(1, 1, 0, 0), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 "A11/A12/A13 BC_n = H"),
    Row("Write with Auto Precharge","WRA",   Pin(1, 1, 0, 0), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 "A10/AP = H"),
    Row("Write AP (BC4, on fly)",   "WRAS4", Pin(1, 1, 0, 0), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("Write AP (BL8, on fly)",   "WRAS8", Pin(1, 1, 0, 0), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("Read (Fixed BL8 or BC4)",  "RD",    Pin(1, 1, 0, 1), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, Some("read"),         ""),
    Row("Read (BC4, on the Fly)",   "RDS4",  Pin(1, 1, 0, 1), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("Read (BL8, on the Fly)",   "RDS8",  Pin(1, 1, 0, 1), 0, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("Read with Auto Precharge", "RDA",   Pin(1, 1, 0, 1), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 "A10/AP = H"),
    Row("Read AP (BC4, on fly)",    "RDAS4", Pin(1, 1, 0, 1), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("Read AP (BL8, on fly)",    "RDAS8", Pin(1, 1, 0, 1), 1, 1, 1, carriesColumn = true,  carriesBank = true,  carriesRow = false, opCode = false, None,                 ""),
    Row("No Operation",             "NOP",   Pin(1, 1, 1, 1), X, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "10; MC expresses NOP as an invalid phase"),
    Row("Device Deselected",        "DES",   Pin(1, X, X, X), X, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "CS_n HIGH; expressed by the per-rank CS one-hot"),
    Row("Power Down Entry",         "PDE",   Pin(1, X, X, X), X, 1, 0, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "6 CKE H->L"),
    Row("Power Down Exit",          "PDX",   Pin(1, X, X, X), X, 0, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "6 CKE L->H"),
    Row("ZQ calibration Long",      "ZQCL",  Pin(1, 1, 1, 0), 1, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, Some("zq"),           ""),
    Row("ZQ calibration Short",     "ZQCS",  Pin(1, 1, 1, 0), 0, 1, 1, carriesColumn = false, carriesBank = false, carriesRow = false, opCode = false, None,                 "same pins as ZQCL, A10/AP = L")
  )

  /** Rows the MC can emit, keyed by the OpTasks flag that emits them. */
  val emitted: Seq[Row] = table16.filter(_.mcOp.isDefined)

  private def build(rowWidth: Int, ranks: Int) = new Bundle {
    val taskParam = TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024)
    val taskConfig = TaskConfig(taskParam, contextWidth = 2, writeTokenInterfaceWidth = 2, writeTokenBufferSize = 16, canRead = true, canWrite = true)
    val sdramtime = SdramTiming(generation = 4, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
    val sdram = SdramConfig(SdramGeneration.DDR4, bgWidth = 1, cidWidth = 1, bankWidth = 4, columnWidth = 10,
                            rowWidth = rowWidth, dataWidth = 16, ddrMHZ = 200, ddrWrLat = 4, ddrRdLat = 4, sdramtime = sdramtime)
    val timeConfig = DfiTimeConfig(frequencyRatio = 2, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
                                   tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 4, tPhyRdCsGap = 3,
                                   tPhyRdCslat = 0, tPhyWrCsLat = 0, tWrlvlResp = 16)
    val dfiConfig = DfiConfig(chipSelectNumber = ranks, dataSlice = 1, signalConfig = new DDR4SignalConfig(DfiFunctionConfig(), false),
                              timeConfig = timeConfig, sdram = sdram)
    val dut = CmdTxd(taskConfig, dfiConfig)
  }

  /** Drive one command, return the sampled pins + A10 + full address bus + bank/bg fields. */
  private case class Sample(pins: Pin, a10: Int, address: BigInt, bank: Int, bg: Int, validPhases: Set[Int], csN: BigInt)

  test("JESD79-4 Table 16 is fully transcribed and the implemented set is the documented one") {
    // The ledger: a row that gains an OpTasks flag (or loses one) must be a deliberate change.
    val implemented = emitted.map(_.abbrev).toSet
    val documented = Set("MRS", "REF", "PRE", "PREA", "ACT", "WR", "RD", "ZQCL")
    assert(implemented == documented,
      s"MC-emittable Table 16 rows changed: got $implemented, documented $documented. " +
      "Not emitted today (no OpTasks flag, see ADR-0017): SRE SRX PDE PDX (no CKE path — " +
      "Alignment.scala:18 ties cke high), WRA/RDA (Task2IDFI.scala:98,111 clears AUTO_PRECHARGE_BIT), " +
      "WRS4/WRS8/RDS4/RDS8/RDAS4/RDAS8 (no on-the-fly burst), ZQCS, REFsb (no per-bank refresh), NOP (implicit), DES (implicit via CS).")
    assert(table16.size == 26, s"Table 16 transcription has ${table16.size} rows, expected 26")
    // The RFU row is the only row that must never be emitted, and it is uniquely identified by
    // (RAS_n, CAS_n, WE_n) = (L, H, H) with ACT_n = H.
    val rfu = table16.find(_.abbrev == "RFU").get.pins
    assert(rfu == Pin(1, 0, 1, 1), "RFU row transcription check")
  }

  private def runEncodingCase(rowWidth: Int): Unit = {
    val fr = 2
    val cmdPhase = 0
    val b = build(rowWidth, ranks = 1)
    SimConfig.compile(b.dut).doSim { dut =>
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
        assert(dut.io.cmd(cmdPhase).valid.toBoolean, "expected a valid command on the command phase")
        val valids = (0 until fr).filter(p => dut.io.cmd(p).valid.toBoolean).toSet
        assert(valids == Set(cmdPhase), s"exactly one phase must carry a command, got $valids")
        assert(c.csN.toBigInt == 0, s"csN must select rank 0, got 0x${c.csN.toBigInt.toString(16)}")
        assert(!(c.rasN.toBoolean && c.casN.toBoolean && c.weN.toBoolean), "encoding collapsed to NOP (RAS=CAS=WE=H)")
        // Negative conformance: the RFU row (RAS=L, CAS=H, WE=H, ACT_n=H) must never appear.
        val pins = Pin(if (c.actN.toBoolean) 1 else 0, if (c.rasN.toBoolean) 1 else 0, if (c.casN.toBoolean) 1 else 0, if (c.weN.toBoolean) 1 else 0)
        if (pins == Pin(1, 0, 1, 1)) throw new AssertionError(s"RFU row emitted (Table 16): pins=$pins address=0x${a.address.toBigInt.toString(16)}")
        Sample(pins, ((a.address.toBigInt >> 10) & 1).toInt, a.address.toBigInt,
          c.bank.toBigInt.toInt, c.bg.toBigInt.toInt, valids, c.csN.toBigInt)
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

      // ---- every emittable row: pins + A10 exactly as transcribed -------------------------
      val drivers: Map[String, () => Sample] = Map(
        "MRS"   -> (() => fire { dut.io.task.mrs #= true; bank(1); row(0x00B0) }),
        "REF"   -> (() => fire { dut.io.task.refresh #= true }),
        "PRE"   -> (() => fire { dut.io.task.precharge #= true; bank(2) }),
        "PREA"  -> (() => fire { dut.io.task.prechargeAll #= true }),
        "ACT"   -> (() => fire { dut.io.task.active #= true; row(0x14000 & ((1 << rowWidth) - 1)); bank(5) }),
        "WR"    -> (() => fire { dut.io.task.write #= true; col(3); bank(6) }),
        "RD"    -> (() => fire { dut.io.task.read #= true; col(7); bank(9) }),
        "ZQCL"  -> (() => fire { dut.io.task.zq #= true })
      )

      emitted.foreach { r =>
        val s = drivers(r.abbrev)()
        if (r.pins.actN >= 0) assert(s.pins.actN == r.pins.actN, s"${r.abbrev}: ACT_n=${s.pins.actN}, Table 16 says ${r.pins.actN}")
        if (r.pins.ras >= 0) assert(s.pins.ras == r.pins.ras, s"${r.abbrev}: RAS_n/A16=${s.pins.ras}, Table 16 says ${r.pins.ras}")
        if (r.pins.cas >= 0) assert(s.pins.cas == r.pins.cas, s"${r.abbrev}: CAS_n/A15=${s.pins.cas}, Table 16 says ${r.pins.cas}")
        if (r.pins.we >= 0) assert(s.pins.we == r.pins.we, s"${r.abbrev}: WE_n/A14=${s.pins.we}, Table 16 says ${r.pins.we}")
        if (r.a10 >= 0) assert(s.a10 == r.a10, s"${r.abbrev}: A10/AP=${s.a10}, Table 16 says ${r.a10} (${r.notes})")
      }

      // ---- ACT: row pin multiplexing per NOTE 1, full row on the address bus ---------------
      def actExp(r: BigInt): Pin = Pin(0,
        if (rowWidth > 16) ((r >> 16) & 1).toInt else 0,
        if (rowWidth > 15) ((r >> 15) & 1).toInt else 0,
        if (rowWidth > 14) ((r >> 14) & 1).toInt else 0)
      Seq(0x14000, 0x8000, 0x4000, 0x00000).foreach { raw =>
        val r = raw & ((1 << rowWidth) - 1)
        val s = fire { dut.io.task.active #= true; row(r); bank(5) }
        assert(s.pins == actExp(r), s"ACT row=0x${Integer.toHexString(r)} (rowWidth=$rowWidth): pins=${s.pins}, expected ${actExp(r)} (RA16/RA15/RA14 on RAS/CAS/WE, NOTE 1)")
        assert(s.address == r, s"ACT row=0x${Integer.toHexString(r)}: address bus=0x${Integer.toHexString(s.address)}, expected the full row")
      }

      // ---- address placement (Table 16 columns CA / BG / BA / OP Code) ---------------------
      // Column on A0..A9 (JESD79-4 2.7), A10 = AP = L for the fixed-BL variants.
      val wr = fire { dut.io.task.write #= true; col(0x155); bank(6) }
      assert((wr.address & 0x3FF) == 0x155, s"WR: column must sit on A9:A0, got A9:A0=0x${(wr.address & 0x3FF).toString(16)}")
      assert(wr.a10 == 0, s"WR (fixed BL8): A10/AP must be L (Table 16), got ${wr.a10}")
      val rd = fire { dut.io.task.read #= true; col(0x2AA); bank(9) }
      assert((rd.address & 0x3FF) == 0x2AA, s"RD: column must sit on A9:A0, got 0x${(rd.address & 0x3FF).toString(16)}")
      assert(rd.a10 == 0, s"RD (fixed BL8): A10/AP must be L (Table 16), got ${rd.a10}")

      // BA = bank[1:0] (low bits), BG = bank[2] (high bits) — JESD79-4 2.7 splits a flat bank
      // index as BG-major, and Task2IDFI.scala:64-65 does the same; asserted here from the table.
      Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13).foreach { bk =>
        val s = fire { dut.io.task.precharge #= true; bank(bk) }
        val dfiBankWidth = 4 - cfg.sdram.bgWidth
        assert((s.bank & ((1 << dfiBankWidth) - 1)) == (bk & ((1 << dfiBankWidth) - 1)),
          s"PRE bank=$bk: BA must carry the low $dfiBankWidth bits, got 0x${Integer.toHexString(s.bank)}")
        assert((s.bg & 1) == ((bk >> dfiBankWidth) & 1), s"PRE bank=$bk: BG must carry bit $dfiBankWidth, got 0x${Integer.toHexString(s.bg)}")
        assert(s.a10 == 0, s"PRE bank=$bk: single-bank precharge requires A10/AP=L (Table 16)")
      }
      val prea = fire { dut.io.task.prechargeAll #= true }
      assert(prea.a10 == 1, s"PREA: A10/AP must be H (Table 16), got ${prea.a10}")

      // MRS: MR select = {BG0, BA1:BA0} and the OP code on A12:A0 (Table 16 "OP Code", NOTE 12).
      Seq(0, 1, 2, 3, 4, 5, 6).foreach { mr =>
        val op = 0x1000 + mr
        val s = fire { dut.io.task.mrs #= true; bank(mr); row(op) }
        val sel = ((s.bg & 1) << 2) | (s.bank & 3)
        assert(sel == mr, s"MRS MR$mr: select {BG0,BA1:BA0}=$sel (NOTE 3: 'For MRS commands the BG and BA selects the specific Mode Register location')")
        assert((s.address & 0x1FFF) == (op & 0x1FFF), s"MRS MR$mr: OP code must be on A12:A0, got 0x${(s.address & 0x1FFF).toString(16)}")
      }

      // ---- NOP when the MC has nothing to send (Table 16 NOP row) --------------------------
      // The MC expresses NOP by not asserting any phase; the pins must not present a command.
      (0 until 8).foreach { _ =>
        clrFlags()
        dut.clockDomain.waitRisingEdge()
        val v = (0 until fr).filter(p => dut.io.cmd(p).valid.toBoolean).toSet
        assert(v.isEmpty, s"idle must not present a command on any phase, got valid phases $v")
      }
    }
  }

  test("every MC-emittable Table 16 row matches the transcription (rowWidth=17)") { runEncodingCase(17) }
  test("every MC-emittable Table 16 row matches the transcription (rowWidth=16)") { runEncodingCase(16) }

  test("DES: unselected ranks are deselected on every phase (Table 16 DES row = CS_n HIGH)") {
    val fr = 2
    val ranks = 2
    val b = build(17, ranks)
    SimConfig.compile(b.dut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.task.active #= false
      dut.io.task.write #= false
      dut.io.task.read #= false
      dut.io.task.precharge #= true
      dut.io.task.prechargeAll #= false
      dut.io.task.refresh #= false
      dut.io.task.mrs #= false
      dut.io.task.zq #= false
      dut.io.task.address.byte #= 0
      dut.io.task.address.column #= 0
      dut.io.task.address.bank #= 0
      dut.io.task.address.row #= 0
      dut.io.task.context #= 0
      dut.io.task.initCsN #= (1 << ranks) - 1
      dut.clockDomain.waitRisingEdge()

      Seq(0, 1).foreach { cs =>
        dut.io.task.address.cs #= cs
        dut.clockDomain.waitRisingEdge()
        val c = dut.io.cmd(0).payload
        assert(dut.io.cmd(0).valid.toBoolean, s"PRE cs=$cs: expected a valid command")
        // csN is packed per phase: phase p owns bits [p*ranks +: ranks]. Active-low one-hot.
        val ph0 = (c.csN.toBigInt >> 0) & ((1 << ranks) - 1)
        assert(ph0 == (((1 << ranks) - 1) & ~(1 << cs)), s"PRE cs=$cs: phase0 csN=0x${ph0.toString(16)}, expected one-hot active-low for rank $cs")
        (1 until fr).foreach { p =>
          val slice = (c.csN.toBigInt >> (p * ranks)) & ((1 << ranks) - 1)
          assert(slice == (1 << ranks) - 1, s"phase $p must carry DES (all CS_n HIGH) while rank $cs is commanded, got 0x${slice.toString(16)}")
        }
        dut.io.task.precharge #= false
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
