package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi.phy.UsPhy
import spinal.tester.SpinalAnyFunSuite

/**
  * T1 netlist lint (ADR-0017): machine-check what no simulation in this repo reaches.
  *
  * Every signal in the [[Dfi]] bundle is a protocol obligation with a direction and a
  * per-generation Required/Optional column (DFI 5.2 Table 54). The functional simulations
  * exercise the command and data groups; the control-channel groups (update, low power,
  * training, PHY-requested leveling, status) have no stimulus at all, so an undriven wire --
  * or a wire quietly tied to a constant that a protocol needs toggled -- is invisible to
  * them. This suite elaborates the MC and the PHY with every capability wired, derives the
  * signal list from the bundle itself (so it cannot drift from the interface), and reads the
  * generated Verilog.
  *
  * A signal tied to a constant is legal ONLY with a reason recorded in [[ledger]] citing a
  * spec clause, an ADR, or the "not implemented, interface reserved" rule. Undocumented
  * tie-offs FAIL; undocumented absence FAILS. Two control tests pin the matcher so the
  * suite cannot pass vacuously.
  *
  * Elaboration only -- no simulation, no file I/O beyond the generated RTL.
  */
object DfiNetlistLint {

  sealed trait Expect
  case object Driven extends Expect                                 // must be a real, non-constant driver
  case class Tied(reason: String) extends Expect                    // must be a constant (any value)
  case class Const(expected: String, reason: String) extends Expect // must be exactly this literal
  case class Zero(reason: String) extends Expect                    // must be constant zero

  /**
    * MC-output signals the simulations never exercise. The name is a leaf path of
    * [[DfiController.io.dfi]] (bundle-derived, Verilog spelling) and must appear here.
    */
  val ledger: Seq[(String, Expect)] = Seq(
    "io_dfi_update_ctrlupdReq" -> Zero("no MC-initiated update sequence: the MC never requests an update (DFI 5.2 sec. 4.9.1); the PHY keeps phyupd_req low, so neither side starts the handshake)"),
    "io_dfi_update_phyupdAck" -> Zero("the MC never accepts a PHY-initiated update, so it never acknowledges (DFI 5.2 sec. 4.9.2)"),
    "io_dfi_lowPowerControl_lpCtrlReq" -> Zero("no DFI low-power handshake: DRAM power-down / self-refresh entry is not implemented (no CKE path) (DFI 5.2 sec. 3.6, sec. 4.13)"),
    "io_dfi_lowPowerControl_lpDataReq" -> Zero("no DFI data-channel low-power request (DFI 5.2 sec. 3.6, sec. 4.13)"),
    "io_dfi_lowPowerControl_lpWakeUp" -> Zero("no DRAM-side wake indication to report (DFI 5.2 sec. 3.6)"),
    "io_dfi_status_initStart" -> Driven,
    "io_dfi_status_freqRatio" -> Const("<ratio code>", "dfi_cmd_freq_ratio / dfi_data_freq_ratio convey the ratio to the PHY (DFI 5.2 sec. 3.5.1, sec. 3.5.3): 1:1=00, 1:2=01, 1:4=10, 1:8=11. Constant per build, and it must be the code, not the ratio"),
    "io_dfi_status_parityIn" -> Zero("idle level: the MC has no parity to report into the PHY; its own CA-parity check is dfi_control_parity (JESD79-4B sec. 4.17, DFI 5.2 sec. 4.12)"),
    "io_dfi_status_dataByteDisable" -> Tied("DFI 5.2 Table 25 removed dfi_data_byte_disable 'for lack of usefulness'; sec. 3.5.2 carries the mask as the dfidata_bit_enable parameter, so the wire stays inactive"),
    "io_dfi_status_dramClkDisable" -> Tied("DFI 5.2 sec. 3.5.3 / sec. 4.4 host-clock disabling is a low-power feature with no path in this MC"),
    "io_dfi_control_cke" -> Tied("CKE held high in operation: JESD79-4B Table 17 / sec. 3.3 step 3 keep CKE registered high through init and normal operation; self-refresh and power-down entry need a CKE path first (not implemented)"),
    "io_dfi_control_resetN" -> Tied("JESD79-4B Table 16 NOTE 2: RESET_n is the asynchronous reset and 'must be maintained HIGH during any function'; the open gap is the inability to assert it during init (DFI 5.2 sec. 4.1/4.2), not the idle level"),
    "io_dfi_rdTraining_rdlvlEn" -> Tied("DFI 5.2 sec. 4.10.3 read-leveling mode is a stub (ADR-0002: 'read training STUB (UNVERIFIED_ON_BOARD)')"),
    "io_dfi_rdTraining_rdlvlReq" -> Tied("DFI 5.2 sec. 4.10.3: no read-leveling request is issued by this MC"),
    "io_dfi_rdTraining_rdlvlGateEn" -> Tied("DFI 5.2 sec. 4.10.3: the read-gate training mode of the stub is never entered"),
    "io_dfi_rdTraining_rdlvlGateReq" -> Tied("DFI 5.2 sec. 4.10.3: no read-gate training request is issued"),
    "io_dfi_wrTraining_wrlvlReq" -> Tied("the implemented write leveling is the legacy dfi_wrlvl_en window (ADR-0002 tier 2), not the DFI 5.2 wrlvl request"),
    "io_dfi_levelingTraining_lvlPattern" -> Tied("DFI 5.2 sec. 4.10.4.4 leveling pattern generator not implemented"),
    "io_dfi_phyRequestedTraining_phylvlReqCsN" -> Tied("DFI 5.2 sec. 4.10.4.5 PHY-requested periodic leveling is not implemented"),
    "io_dfi_control_cid" -> Zero("3DS logical ranks are NOT_IMPLEMENTED: DFI 5.2 sec. 4.5.2 / Table 46 map logical ranks onto dfi_cid[2:0]; this MC selects physical ranks with dfi_cs only, so cid must stay constant 0")
  )

  // ------------------------------------------------------------------ netlist helpers

  /** Leaf paths of a bundle, spelled the way SpinalHDL spells Verilog ports. */
  def leaves(d: Data, prefix: String): Seq[String] = d match {
    case m: MultiData if m.elements != null && m.elements.nonEmpty =>
      m.elements.flatMap { case (n, e) => leaves(e, s"${prefix}_$n") }.toSeq
    case _ => Seq(prefix)
  }

  private def q(s: String) = java.util.regex.Pattern.quote(s)

  /** Port declaration in the module header. */
  private val declRe = (port: String) => raw"(?m)^\s*(output|input)\s+(wire|reg)?\s*(\[[0-9:]+\])?\s*${q(port)}\s*[,;]".r
  def declared(v: String, port: String): Boolean = declRe(port).findFirstIn(v).isDefined

  /**
    * SpinalHDL prunes a top-level port whose driver is a constant: it disappears from the
    * netlist (see the `Pruned signal detected` report). So "absent from the netlist" has two
    * meanings -- constant-folded (documented tie-off) or genuinely not wired (a hole). The
    * pruned set is the discriminator, which is why the ledger is checked against both.
    */
  def pruned(r: SpinalReport[_], port: String): Boolean =
    r.prunedSignals.exists(bt => bt.toString.contains(port))

  /**
    * Continuous assign, bit-slice assign, and register (always-block) assign, in every
    * SpinalHDL spelling: `assign p = x;`, `assign p (3) = x;`, `p[0 : 0] = x;`, `p[1] = x;`.
    */
  private val drvRe = (port: String) => raw"(?m)^\s*(assign\s+)?${q(port)}\s*((\[[0-9]+(\s*:\s*[0-9]+)?\])\s*)?=\s*(.*);".r
  def driversOf(v: String, port: String): Seq[String] =
    drvRe(port).findAllMatchIn(v).map(m => m.group(5).trim).toSeq

  private val identRe = raw"^[A-Za-z_][A-Za-z0-9_]*$$".r

  private val regDeclRe = (name: String) => raw"(?m)^\s*(output\s+)?reg\s+([0-9a-zA-Z_\s]*\[[0-9:]+\])?\s*${q(name)}\s*[,;]".r
  def isReg(v: String, name: String): Boolean = regDeclRe(name).findFirstIn(v).isDefined

  // SpinalHDL also drives top-level outputs through the instantiation port map
  // (`.io_dfi_status_dramClkDisable (alignment_1_io_output_status_dramClkDisable)`).
  private val portMapRe = (net: String) => raw"(?m)\.${q(net)}\s*\(\s*([^(),]+)\s*\)".r
  def portMapOf(v: String, port: String): Seq[String] =
    portMapRe(port).findAllMatchIn(v).map(_.group(1).trim).toSeq

  /**
    * Follow single-identifier routings to their source. SpinalHDL names an instance's net
    * `<inst>_<localPort>`, so the source of `alignment_1_io_output_control_cke` is found by
    * stripping leading tokens. A `reg` terminal means real sequential logic, not a tie-off.
    */
  def terminalRhs(v: String, port: String, depth: Int = 6): Seq[String] = {
    val ds = driversOf(v, port)
    if (ds.isEmpty) Nil
    else ds.flatMap { rhs =>
      if (identRe.findFirstIn(rhs.trim).isEmpty) Seq(rhs)
      else {
        val id = rhs.trim
        if (id == port) Seq(rhs)
        else if (isReg(v, id)) Seq(s"reg:$id")
        else {
          val parts = id.split("_")
          val src = (1 until parts.length).toSeq
            .map(i => parts.drop(i).mkString("_"))
            .flatMap(c => driversOf(v, c))
          if (src.isEmpty) Seq(rhs)
          else if (depth > 0) src.flatMap(s => terminalRhs(v, s, depth - 1).headOption).toSeq
          else src
        }
      }
    }.distinct
  }

  private val litRe = raw"^(\d+)?'([bho])([0-9a-fA-Fx]+)$$".r
  def literalOf(rhs: String): Option[String] = litRe.findFirstIn(rhs.trim) match {
    case Some(_) => Some(rhs.trim)
    case None => None
  }
  def isZeroLiteral(rhs: String): Boolean = literalOf(rhs) match {
    case Some(l) => val d = l.split("'")(1).drop(1); d.nonEmpty && d.forall(_ == '0')
    case None => false
  }
  def isConstantLiteral(rhs: String): Boolean = literalOf(rhs) match {
    case Some(l) => val d = l.split("'")(1).drop(1); d.nonEmpty && d.forall(c => c == '0' || c == '1')
    case None => false
  }

  def moduleBody(v: String, module: String): Option[String] = {
    val m = raw"(?m)^module\s+${q(module)}\s*\(".r.findFirstIn(v)
    if (m.isEmpty) None
    else {
      val from = v.indexOf(m.get)
      val next = v.indexOf("\nmodule ", from + 1)
      Some(if (next < 0) v.substring(from) else v.substring(from, next))
    }
  }
  /** Any module in `v` whose body mentions `needle` (used to find the module that owns a signal). */
  def moduleWith(v: String, needle: String): Option[String] = {
    val re = raw"(?m)^module\s+(\w+)\s*\(".r
    val names = re.findAllMatchIn(v).map(_.group(1)).toSeq
    names.map(n => moduleBody(v, n)).flatten.find(b => b.contains(needle))
  }

  def classify(v: String, port: String): (Expect, String) = {
    val t = terminalRhs(v, port)
    if (t.isEmpty) (if (declared(v, port)) (Driven, "no assign text: driven by an instance port map") else (Driven, "absent"))
    else {
      val lits = t.flatMap(x => literalOf(x))
      val exprs = t.filter(x => literalOf(x).isEmpty)
      if (exprs.nonEmpty) (Driven, exprs.mkString(" | "))
      else if (lits.forall(isZeroLiteral)) (Zero("undocumented"), lits.mkString(" | "))
      else if (lits.nonEmpty) (Tied("undocumented"), lits.mkString(" | "))
      else (Driven, t.mkString(" | "))
    }
  }

  // ------------------------------------------------------------------ configs

  val allSignalsDdr4: DfiSignalConfig = new DDR4SignalConfig(
    DfiFunctionConfig(
      useCtrlSignals = true, useWrDataSignals = true, useRdDataSignals = true,
      useUpdateSignals = true, useStatusSignals = true, useTrainingSignals = true,
      useLowPowerSignals = true, useErrorSignals = true
    ),
    useCrc = false
  ) {
    override val useCtrlMsg = true
  }

  val minimalDdr4: DfiSignalConfig = new DDR4SignalConfig(DfiFunctionConfig(), useCrc = false)

  def lintConfig(fr: Int, signalConfig: DfiSignalConfig, ranks: Int = 1, cidWidth: Int = 1,
                 mr2: Int = 0x000, mr5: Int = 0x0000): DfiConfig = {
    val timing = SdramTiming(generation = 4, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
    val sdram = SdramConfig(SdramGeneration.DDR4, bgWidth = 1, cidWidth = cidWidth, bankWidth = 4, columnWidth = 10,
      rowWidth = 16, dataWidth = 16, ddrMHZ = 200, ddrWrLat = 4, ddrRdLat = 4, sdramtime = timing)
    val time = DfiTimeConfig(frequencyRatio = fr, cmdPhase = 0, tPhyWrLat = sdram.tPhyWrlat, tPhyWrData = 0,
      tPhyWrCsGap = 3, tRddataEn = sdram.tRddataEn, tPhyRdlat = 4, tPhyRdCsGap = 3, tPhyRdCslat = 0, tPhyWrCsLat = 0)
    DfiConfig(chipSelectNumber = ranks, dataSlice = 1, signalConfig = signalConfig, timeConfig = time,
      sdram = sdram, ddr4ModeRegs = Ddr4ModeRegs(mr2 = mr2, mr5 = mr5))
  }

  case class Built(rtl: String, top: Component, dfiNames: Seq[String], report: SpinalReport[_])

  def mcBuilt(cfg: DfiConfig): Built = {
    val task = TaskParameter(timingWidth = 5, refWidth = 23, cmdBufferSize = 1024, dataBufferSize = 1024, rspBufferSize = 1024)
    val bmbp = BmbParameter(
      addressWidth = cfg.sdram.byteAddressWidth + log2Up(cfg.chipSelectNumber),
      dataWidth = cfg.beatWidth,
      sourceWidth = 0,
      contextWidth = 2,
      lengthWidth = 10,
      alignment = BmbParameter.BurstAlignement.WORD
    )
    val report = SpinalVerilog(DfiController(bmbp, task, cfg, RowBankColumn))
    Built(report.getRtlString(), report.toplevel, leaves(report.toplevel.io.dfi, "io_dfi"), report)
  }

  // The PHY exposes the bundle as `dfi_*` (its DFI is a direct member, not under `io`), so the
  // port spelling differs from the MC's `io_dfi_*`.
  def phyNetlist(cfg: DfiConfig): String = SpinalVerilog(UsPhy(cfg)).getRtlString()
  def phyNames(cfg: DfiConfig): Seq[String] = {
    val report = SpinalVerilog(UsPhy(cfg))
    leaves(report.toplevel.dfi, "dfi")
  }
}

class DfiNetlistLintSpec extends SpinalAnyFunSuite {
  import DfiNetlistLint._

  private val debug = sys.env.get("DFI_LINT_DEBUG").isDefined
  private lazy val allCaps = mcBuilt(lintConfig(fr = 2, signalConfig = allSignalsDdr4, mr2 = 0x4, mr5 = 0x1800))

  test("the bundle's DFI surface is physically present in the MC netlist (DFI 5.2 Table 54)") {
    val names = allCaps.dfiNames
    assert(names.size > 40, s"bundle-derived DFI leaf list is suspiciously small (${names.size}): ${names.take(8)}")
    val missing = names.filterNot(n => declared(allCaps.rtl, n) || pruned(allCaps.report, n))
    if (debug) {
      println("[LINT] rtl length = " + allCaps.rtl.length)
      println(allCaps.rtl.linesIterator.take(28).mkString("\n"))
      println("[LINT] declared(io_dfi_control_cke) = " + declared(allCaps.rtl, "io_dfi_control_cke"))
      println("[LINT] raw contains io_dfi_control_cke = " + allCaps.rtl.contains("io_dfi_control_cke"))
      println("[LINT] PRUNED:")
      allCaps.report.printPruned()
    }
    assert(missing.isEmpty, s"DFI signals in the bundle that are neither in the netlist nor constant-folded (i.e. not wired at all): ${missing.mkString(", ")}")
  }

  test("control: a capability that is switched off really is absent, so the matcher has teeth") {
    val min = mcBuilt(lintConfig(fr = 2, signalConfig = minimalDdr4))
    val off = min.dfiNames
    assert(off.exists(_.startsWith("io_dfi_control")), s"even the minimal build must have the command group: $off")
    Seq("io_dfi_update_ctrlupdReq", "io_dfi_status_initStart", "io_dfi_lowPowerControl_lpCtrlReq",
      "io_dfi_rdTraining_rdlvlEn", "io_dfi_status_parityIn").foreach { n =>
      assert(!off.contains(n), s"the minimal signal config should not declare $n -- the all-caps config is the one that must")
      assert(!declared(min.rtl, n), s"$n is present in the minimal netlist; the presence test would then be vacuous")
    }
    Seq("io_dfi_update_ctrlupdReq", "io_dfi_status_initStart", "io_dfi_lowPowerControl_lpCtrlReq",
      "io_dfi_rdTraining_rdlvlEn", "io_dfi_status_parityIn").foreach { n =>
      assert(allCaps.dfiNames.contains(n), s"$n is missing from the all-caps bundle derivation (name spelling?)")
      assert(declared(allCaps.rtl, n) || pruned(allCaps.report, n), s"$n is derived from the bundle but is neither declared nor constant-folded in the all-caps netlist")
    }
  }

  test("every control-channel signal is driven, or tied with a documented reason") {
    val v = allCaps.rtl
    val names = ledger.map(_._1)
    val unknown = names.filterNot(allCaps.dfiNames.contains)
    assert(unknown.isEmpty, s"ledger entries that are not bundle leaves (typo / renamed signal): ${unknown.mkString(", ")}")
    val problems = ledger.map { case (port, want) =>
      val isDecl = declared(v, port)
      val isPruned = pruned(allCaps.report, port)
      (want, isDecl, isPruned) match {
        case (Driven, true, false) =>
          val (got, detail) = classify(v, port)
          if (got == Driven) None else Some(s"$port: expected real logic, netlist gives $got ($detail)")
        case (Driven, _, _) =>
          Some(s"$port: the ledger says this MC drives it with logic, but the netlist ${if (isPruned) "constant-folded it away" else "does not declare it"}")
        case (w, false, true) =>
          if (w.reason.nonEmpty) None else Some(s"$port: constant-folded with no documented reason")
        case (w, true, _) =>
          Some(s"$port: the ledger marks it a documented tie-off, but the port carries logic in the netlist (declared) -- re-classify it as Driven")
        case (w, false, false) =>
          Some(s"$port: the ledger marks it tied, but the port is neither declared nor constant-folded -> it is not wired at all")
      }
    }.flatten
    if (debug) ledger.foreach { case (p, w) => println(s"[LINT] $p expect=$w got=${classify(v, p)}") }
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  test("dfi_status_freqRatio carries the DFI 5.2 ratio CODE, not the ratio (sec. 3.5.3)") {
    Seq(1, 2, 4).foreach { fr =>
      val v = mcBuilt(lintConfig(fr, signalConfig = allSignalsDdr4)).rtl
      val t = terminalRhs(v, "io_dfi_status_freqRatio")
      if (debug) println(s"[LINT] fr=$fr freqRatio terminals=$t")
      assert(t.nonEmpty, s"fr=$fr: io_dfi_status_freqRatio has no driver at all")
      val expect = fr match { case 1 => "1'b0"; case 2 => "2'b01"; case 4 => "2'b10" }
      t.foreach { rhs =>
        assert(rhs == expect, s"fr=$fr: dfi_status_freqRatio = $rhs, expected $expect (DFI 5.2: 1:1=00, 1:2=01, 1:4=10, 1:8=11)")
      }
    }
  }

  test("CA parity is the XOR over the covered CA pins (JESD79-4B sec. 4.17, DFI 5.2 Table 13 note h)") {
    val v = allCaps.rtl
    val covered = Seq("actN", "rasN", "casN", "weN", "bg", "bank", "cid", "address")
    val excluded = Seq("cke", "odt", "csN")
    val rhs = driversOf(v, "io_output_parity")
    assert(rhs.nonEmpty, "no module in the netlist drives io_output_parity -> the CA parity output is undriven (DFI 5.2 sec. 4.3 forbids floating)")
    rhs.foreach { r =>
      if (debug) println(s"[LINT] parity = $r")
      assert(!isZeroLiteral(r), s"parity is tied to a constant: $r -- with MR2.A2 set (CA parity enable) the DRAM flags every command")
      covered.foreach { n =>
        assert(r.contains(s"io_output_$n") || r.contains(s"_${n}["), s"parity = $r omits $n (JESD79-4B sec. 4.17 covers ACT_n, RAS_n/A16, CAS_n/A15, WE_n/A14, A13..A0, BG, BA, CID)")
      }
      excluded.foreach { n =>
        assert(!r.contains(s"io_output_$n"), s"parity = $r covers $n, which JESD79-4B sec. 4.17 excludes")
      }
    }
  }

  test("the PHY drives every MC-facing DFI output: unimplemented channels are tied with a reason") {
    val phyReport = SpinalVerilog(UsPhy(lintConfig(fr = 2, signalConfig = allSignalsDdr4, mr5 = 0x1800)))
    val v = phyReport.getRtlString()
    val phyLedger: Seq[(String, Expect)] = Seq(
      "dfi_update_ctrlupdAck" -> Zero("this PHY has no MC-initiated update state machine, so it never acknowledges ctrlupd_req (DFI 5.2 sec. 4.9.1)"),
      "dfi_update_phyupdReq" -> Zero("no PHY-initiated/periodic leveling request (DFI 5.2 sec. 4.10.4.5)"),
      "dfi_update_phyupdType" -> Zero("phyupd_type is meaningless while phyupd_req stays low (DFI 5.2 sec. 4.10.4.5)"),
      "dfi_lowPowerControl_lpAck" -> Zero("no DFI low-power handshake in this PHY (DFI 5.2 sec. 3.6, sec. 4.13)"),
      "dfi_error_error" -> Zero("no DFI error reporting source in this PHY (DFI 5.2 sec. 3.7)"),
      "dfi_error_errorInfo" -> Zero("no DFI error reporting source in this PHY (DFI 5.2 sec. 3.7)")
    )
    val problems = phyLedger.map { case (port, want) =>
      assert(declared(v, port) || pruned(phyReport, port), s"$port is neither declared nor constant-folded by the PHY")
      if (pruned(phyReport, port) && want.reason.nonEmpty) None
      else classify(v, port) match {
        case (z, detail) if z.isInstanceOf[Zero] && want.isInstanceOf[Zero] => None
        case (got, detail) => Some(s"$port: expected $want, netlist gives $got ($detail)")
      }
    }.flatten
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  test("the PHY exposes the whole DFI surface its pads need (DFI 5.2 Table 54)") {
    val cfg = lintConfig(fr = 2, signalConfig = allSignalsDdr4, mr5 = 0x1800)
    val v = phyNetlist(cfg)
    val names = phyNames(cfg)
    assert(names.size > 40, s"PHY bundle leaf list is suspiciously small (${names.size})")
    val missing = names.filterNot(n => declared(v, n) || v.contains(n))
    assert(missing.isEmpty, s"UsPhy is missing DFI ports (${missing.size}):\n  " + missing.mkString("\n  "))
  }
}
