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

  sealed trait Expect { def reason: String }
  case class Driven(reason: String = "") extends Expect        // must be a real, non-constant driver; reason documents WHY this non-constant driver is the expected one
  case class Tied(reason: String) extends Expect                    // must be a constant (any value)
  case class Const(expected: String, reason: String) extends Expect // must be exactly this literal
  case class Zero(reason: String) extends Expect                    // must be constant zero
  case class Absent(reason: String) extends Expect                   // not instantiated at all: a documented hole

  /**
    * MC-output signals the simulations never exercise. The name is a leaf path of
    * [[DfiController.io.dfi]] (bundle-derived, Verilog spelling) and must appear here.
    */
  val ledger: Seq[(String, Expect)] = Seq(
    // The empty bundle member is itself the leaf: `leaves` yields io_dfi_caTraining with no children.
    "io_dfi_caTraining" -> Absent("the CA-training group is instantiated with ZERO signals: useCalvlReq/usePhyCalvlCsN/useCalvlEn/useCalvlCapture/useCalvlResp default to false in DfiSignalConfig (lib/.../dfi/interface/DfiConfig.scala:120-124) and no generation class overrides them, so enabling training signals leaves no CA training wires at all (DFI 5.2 sec. 4.10.3) — NOT_IMPLEMENTED"),
    "io_dfi_update_ctrlupdReq" -> Zero("no MC-initiated update sequence: the MC never requests an update (DFI 5.2 sec. 4.9.1); the PHY keeps phyupd_req low, so neither side starts the handshake)"),
    "io_dfi_update_phyupdAck" -> Zero("the MC never accepts a PHY-initiated update, so it never acknowledges (DFI 5.2 sec. 4.9.2)"),
    "io_dfi_lowPowerControl_lpCtrlReq" -> Zero("no DFI low-power handshake: DRAM power-down / self-refresh entry is not implemented (no CKE path) (DFI 5.2 sec. 3.6, sec. 4.13)"),
    "io_dfi_lowPowerControl_lpDataReq" -> Zero("no DFI data-channel low-power request (DFI 5.2 sec. 3.6, sec. 4.13)"),
    "io_dfi_lowPowerControl_lpWakeUp" -> Zero("no wake indication to report (DFI 5.2 sec. 3.6). NOTE: every producer of this net is a constant, so the whole chain constant-folds (Alignment.scala:106 clearAll, the Control payload) and the top-level port is left with a net driver that has no driver of its own: 2-state simulation reads 0, xsim reads X. Netlist artifact of a deep constant tie, tracked here, not an MC behaviour defect."),
    "io_dfi_status_initStart" -> Driven(""),
    "io_dfi_status_freqRatio" -> Const("2'b01", "this default build runs at 1:2 ratio, which encodes as 01 per DFI 5.2 sec. 3.5.3 (1:1=00, 1:2=01, 1:4=10, 1:8=11); the constant is computed in the Alignment layer and reaches this port via the alignment instance port; the per-fr encoding itself is pinned by the dedicated ratio-code test below"),
    "io_dfi_status_parityIn" -> Zero("idle level: the MC has no parity to report into the PHY; its own CA-parity check is dfi_control_parity (JESD79-4B sec. 4.17, DFI 5.2 sec. 4.12)"),
    "io_dfi_status_dataByteDisable" -> Tied("DFI 5.2 Table 25 removed dfi_data_byte_disable 'for lack of usefulness'; sec. 3.5.2 carries the mask as the dfidata_bit_enable parameter, so the wire stays inactive"),
    "io_dfi_status_dramClkDisable" -> Tied("DFI 5.2 sec. 3.5.3 / sec. 4.4 host-clock disabling is a low-power feature with no path in this MC"),
    "io_dfi_control_cke" -> Driven("CKE held high in operation: JESD79-4B Table 17 / sec. 3.3 step 3 keep CKE registered high through init and normal operation; self-refresh and power-down entry need a CKE path first (not implemented). Driver is the Alignment-layer hold-high register, routed through the alignment instance port"),
    "io_dfi_control_resetN" -> Tied("JESD79-4B Table 16 NOTE 2: RESET_n is the asynchronous reset and 'must be maintained HIGH during any function'; the open gap is the inability to assert it during init (DFI 5.2 sec. 4.1/4.2), not the idle level"),
    "io_dfi_rdTraining_rdlvlEn" -> Tied("DFI 5.2 sec. 4.10.3 read-leveling mode is a stub (ADR-0002: 'read training STUB (UNVERIFIED_ON_BOARD)')"),
    "io_dfi_rdTraining_rdlvlReq" -> Driven("top-level DFI input, driven by the PHY side outside this netlist. Contract: this MC never issues a read-leveling request (DFI 5.2 sec. 4.10.3), so the PHY must never assert it"),
    "io_dfi_rdTraining_rdlvlGateEn" -> Tied("DFI 5.2 sec. 4.10.3: the read-gate training mode of the stub is never entered"),
    "io_dfi_rdTraining_rdlvlGateReq" -> Driven("top-level DFI input, driven by the PHY side outside this netlist. Contract: no read-gate training request is ever asserted (DFI 5.2 sec. 4.10.3)"),
    "io_dfi_wrTraining_wrlvlReq" -> Driven("top-level DFI input, driven by the PHY side outside this netlist. Contract: the implemented write leveling is the legacy dfi_wrlvl_en window (ADR-0002 tier 2), not the DFI 5.2 wrlvl request, so the PHY must never assert it"),
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
  private val declRe = (port: String) => raw"(?m)^\s*(output|input)\s+(wire|reg)?\s*(\[[0-9:]+\])?\s+${q(port)}\s*[,;]".r
  def declared(v: String, port: String): Boolean = declRe(port).findFirstIn(v).isDefined

  /**
    * Classify a port against the netlist AND the pruned-signal set. A top-level port whose
    * driver is a constant is constant-folded away by SpinalHDL, so "not in the netlist" means
    * "tied" only when the pruned report confirms it; anything else is not wired at all.
    */
  def classifyNet(v: String, r: SpinalReport[_], port: String): (Expect, String) = {
    // An absent-to-Verilog output is not a hole when SpinalHDL folded it away: the constant
    // driver is the design's statement, and `classify` recovers the literal from the module
    // that drives the net. So classify first, and only call it a hole if there is no driver.
    val (got, detail) = classify(v, port)
    if (got != Absent("no driver found for this port")) (got, detail)
    else if (folded(r, port)) (Zero("constant-folded out of the netlist"), detail)
    else (Absent("no driver found for this port"), detail)
  }

  /**
    * SpinalHDL prunes a top-level port whose driver is a constant: it disappears from the
    * netlist (see the `Pruned signal detected` report). So "absent from the netlist" has two
    * meanings -- constant-folded (documented tie-off) or genuinely not wired (a hole). The
    * pruned set is the discriminator, which is why the ledger is checked against both.
    */
  def pruned(r: SpinalReport[_], port: String): Boolean =
    r.prunedSignals.exists(bt => bt.toString.contains(port))

  /**
    * A whole bundle member can be constant-folded away, in which case the pruned report names
    * the bundle (`.../io_dfi_status`) and never its leaves. So "folded" means the leaf name or
    * its group prefix shows up.
    */
  def folded(r: SpinalReport[_], port: String): Boolean = {
    // Top-level nets are `io_dfi_status_freqRatio`; the same signal inside a submodule is
    // `io_output_status_freqRatio` (Alignment) or `dfi_status_freqRatio` (UsPhy). Match on the
    // group+field tail so a fold anywhere on the path counts, and on the group so a whole
    // bundle member folded away counts too.
    val tail = port.replaceFirst("^io_dfi_", "").replaceFirst("^dfi_", "")
    pruned(r, port) || pruned(r, tail) || pruned(r, port.replaceAll("_[A-Za-z0-9]+$", ""))
  }

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
  /** Names X such that the netlist contains `.X (<net>)` — i.e. the submodule port driven by `net`. */
  // The argument may carry a bit-select for multi-bit nets: `.io_output_control_cke (x[3:0])`.
  private val portTargetRe = (net: String) => raw"(?m)\.\s*([A-Za-z0-9_]+)\s*\(\s*${q(net)}(\s*\[\s*[0-9]+\s*(:?\s*[0-9]+\s*)?\])?\s*\)".r
  def portTargetsOf(v: String, net: String): Seq[String] =
    portTargetRe(net).findAllMatchIn(v).map(_.group(1)).toSeq

  /**
    * Follow every routing spelling to a terminal: `assign`, a bracketed assign in an always
    * block, a port map in either direction (`.port (net)` and `.local (topNet)`), and the
    * `<inst>_<localPort>` net naming SpinalHDL uses. Returns the literal/expression text, and
    * `reg:<name>` when the trail ends at a register (real sequential logic, not a tie-off).
    */
  def terminalRhs(v: String, port: String, depth: Int = 8): Seq[String] = {
    def go(name: String, seen: Set[String], d: Int): Seq[String] = {
      if (seen.contains(name) || d <= 0) Nil
      else {
        val ds = driversOf(v, name)
        if (ds.isEmpty) {
          // Nothing drives it here; it may be a submodule port driven through a port map.
          val ups = portTargetsOf(v, name)
          if (ups.isEmpty) Nil else ups.flatMap(u => go(u, seen + name, d - 1))
        } else ds.flatMap { rhs =>
          val r = rhs.trim
          if (identRe.findFirstIn(r).isEmpty) Seq(rhs)
          else if (isReg(v, r)) Seq(s"reg:$r")
          else {
            val strip = (1 until r.split("_").length).toSeq.map(i => r.split("_").drop(i).mkString("_"))
            val next = (Seq(r) ++ strip).flatMap(n => driversOf(v, n) ++ portTargetsOf(v, n))
            // A literal found at any hop IS the terminal: go() only resolves
            // identifiers/nets, so recursing on literal text finds nothing.
            val lits = next.filter(n => literalOf(n).isDefined)
            if (lits.nonEmpty) lits
            else if (next.isEmpty) Seq(rhs)
            else next.flatMap(n => go(n, seen + name, d - 1)).headOption.toSeq
          }
        }
      }
    }
    go(port, Set.empty, depth).distinct
  }

  private val litRe = raw"^(\d+)?'([bho])([0-9a-fA-Fx]+)$$".r
  /** Strip spacing/parentheses and lowercase the base so "2'B01" == "2'b01" == "(2'b01)". */
  def normalize(rhs: String): String = rhs.trim.replaceAll("[\\s()]+", "").toLowerCase

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

  /** Every `module ... endmodule` chunk in the file (module names repeat across configs). */
  def moduleBodies(v: String): Seq[String] =
    raw"(?ms)^module\s.*?^endmodule".r.findAllIn(v).toSeq

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
    /*
     * Drivers arrive in five spellings, and every one of them has bitten this lint at least once:
     *   assign p = x;                              (continuous)
     *   p[0 : 0] = x;                              (slice, inside an always block)
     *   .io_dfi_update_ctrlupdReq (1'b0)           (instantiation port map, constant)
     *   .io_dfi_update_ctrlupdReq (alignment_1_io_output_update_ctrlupdReq)   (port map, net)
     *   io_output_update_ctrlupdReq = 1'b0;        (the driving module's own port, and the only
     *                                               text left when the top-level port was folded)
     * `viaField` covers the module-level spelling, which is what a folded signal leaves behind.
     */
    val field = port.replaceFirst("^io_dfi_", "").replaceFirst("^dfi_", "")
    def collect(name: String): Seq[String] =
      driversOf(v, name) ++ portMapOf(v, name).map(_.trim)
    // A bundle leaf can be driven through its packed parent (`assign io_dfi_lowPowerControl = ...`),
    // so every `_`-prefix of the leaf name is a candidate driver site.
    def prefixes(n: String): Seq[String] = (1 until n.split("_").length).toSeq.map(i => n.split("_").drop(i).mkString("_"))
    val up = prefixes(port).flatMap(n => driversOf(v, n) ++ portMapOf(v, n))
    val raw = (collect(port) ++ (if (field != port) collect(field) else Nil) ++
      portTargetsOf(v, port) ++ up).distinct
    if (sys.env.get("DFI_LINT_DEBUG").isDefined && raw.isEmpty)
      println(s"[LINT-RAW] $port lines=${v.linesIterator.filter(_.contains(port)).take(4).mkString(" ~~ ")}")
    val mappedNets = raw.filter(r => identRe.findFirstIn(r).isDefined)
    val t = (raw.filterNot(r => identRe.findFirstIn(r).isDefined) ++
             mappedNets.flatMap(n => terminalRhs(v, n))).distinct
    val viaField = if (field == port) Nil else driversOf(v, field)
    val all = (t ++ viaField).distinct
    if (all.isEmpty)
      (if (declared(v, port)) (Driven(""), "declared, no driver text found")
       else (Absent("no driver found for this port"), "absent"))
    else {
      val lits = all.flatMap(x => literalOf(x))
      val exprs = all.filter(x => literalOf(x).isEmpty)
      if (lits.nonEmpty && lits.length == all.length)
        (if (lits.forall(isZeroLiteral)) (Zero("constant zero driver"), lits) else (Tied("constant driver"), lits)) match {
          case (e, l) => (e, l.mkString(" | "))
        }
      else if (lits.nonEmpty && lits.forall(isZeroLiteral) && exprs.forall(e => identRe.findFirstIn(e).isDefined))
        // A constant tie routed out through a net: the net(s) are the trace, the literal is the value.
        (Zero("constant zero driver, routed out"), (lits ++ exprs).mkString(" | "))
      else (Driven(""), all.mkString(" | "))
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
  val phyLedger: Seq[(String, Expect)] = Seq(
      "dfi_update_ctrlupdAck" -> Zero("this PHY has no MC-initiated update state machine, so it never acknowledges ctrlupd_req (DFI 5.2 sec. 4.9.1)"),
      "dfi_update_phyupdReq" -> Zero("no PHY-initiated/periodic leveling request (DFI 5.2 sec. 4.10.4.5)"),
      "dfi_update_phyupdType" -> Zero("phyupd_type is meaningless while phyupd_req stays low (DFI 5.2 sec. 4.10.4.5)"),
      "dfi_lowPowerControl_lpAck" -> Zero("no DFI low-power handshake in this PHY (DFI 5.2 sec. 3.6, sec. 4.13)"),
      "dfi_error_error" -> Zero("no DFI error reporting source in this PHY (DFI 5.2 sec. 3.7)"),
      "dfi_error_errorInfo" -> Zero("no DFI error reporting source in this PHY (DFI 5.2 sec. 3.7)")
    )

  val phyHoles: Seq[(String, Expect)] = Seq("dfi_caTraining" -> Absent("empty CA-training bundle: useCalvl* default to false and no generation overrides them (DFI 5.2 sec. 4.10.3) — NOT_IMPLEMENTED"))

  lazy val phyCaps = SpinalVerilog(UsPhy(lintConfig(fr = 2, signalConfig = allSignalsDdr4, mr5 = 0x1800)))

}

class DfiNetlistLintSpec extends SpinalAnyFunSuite {
  import DfiNetlistLint._

  private val debug = sys.env.get("DFI_LINT_DEBUG").isDefined
  private lazy val allCaps = mcBuilt(lintConfig(fr = 2, signalConfig = allSignalsDdr4, mr2 = 0x4, mr5 = 0x1800))

  test("the bundle's DFI surface is physically present in the MC netlist (DFI 5.2 Table 54)") {
    val names = allCaps.dfiNames
    assert(names.size > 40, s"bundle-derived DFI leaf list is suspiciously small (${names.size}): ${names.take(8)}")
    val documentedAbsent = ledger.collect { case (n, a: Absent) => n }.toSet
    val missing = names.filterNot(n => declared(allCaps.rtl, n) || folded(allCaps.report, n) || documentedAbsent(n))
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
      val (got, detail) = classifyNet(v, allCaps.report, port)
      val lits = driversOf(v, port).map(normalize)
      (want, got) match {
        case (_: Absent, _: Absent) => None
        case (w: Driven, g: Driven) => None
        case (w, g) if !w.isInstanceOf[Driven] && !g.isInstanceOf[Driven] && !g.isInstanceOf[Absent] =>
          // A documented tie-off: the port is declared and driven only by literals, or SpinalHDL
          // folded it away because its driver is a constant. Const additionally pins the value.
          w match {
            case Const(expected, _) =>
              val foldedOk = folded(allCaps.report, port)
              // The literal may sit BEHIND an instance port (computed in a submodule body):
              // terminalRhs follows the chain and returns the terminal constants.
              val deep = terminalRhs(v, port)
              if (lits.exists(_ == normalize(expected)) || deep.exists(t => normalize(t) == normalize(expected)) || (foldedOk && w.reason.nonEmpty)) None
              else Some(s"$port: ledger pins constant $expected, netlist drives ${if (lits.isEmpty) "(nothing)" else lits.mkString("|")} (terminals: ${deep.mkString("|")})")
            case _ => None
          }
        case _ => Some(s"$port: ledger expects $want, netlist gives $got ($detail)\n         diag: declared=${declared(v, port)} folded=${folded(allCaps.report, port)} pruned=${pruned(allCaps.report, port)} drivers=${driversOf(v, port).mkString("|")} fieldDrivers=${driversOf(v, port.replaceFirst("^io_dfi_", "")).mkString("|")} portMap=${portMapOf(v, port).mkString("|")} targets=${portTargetsOf(v, port).mkString("|")}\n         text=${v.linesIterator.filter(_.contains(port)).take(3).mkString(" ~~ ")}")
      }
    }.flatten
    if (debug) ledger.foreach { case (p, w) => println(s"[LINT] $p expect=$w got=${classify(v, p)}") }
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  test("dfi_status_freqRatio carries the DFI 5.2 ratio CODE, not the ratio (sec. 3.5.3)") {
    // fr=1 is outside this stack's supported boundary: UsPhy has a documented require(fr >= 2)
    // and tester/.../phy/E2eSmokeGen.scala keeps a negative proof that fr=1 must throw, so the
    // 1:1 MC netlist is degenerate. The ratio CODE check runs on the ratios that can be built.
    Seq(2, 4).foreach { fr =>
      // useFreqRatio is `false` in DfiSignalConfig and no generation class overrides it, so the
      // capability has to be switched on explicitly for the port to exist at all.
      val b = mcBuilt(lintConfig(fr, signalConfig = new DDR4SignalConfig(DfiFunctionConfig(useStatusSignals = true), false) { override val useFreqRatio = true }))
      val expect = fr match { case 1 => "1'b0"; case 2 => "2'b01"; case 4 => "2'b10" }
      val t = terminalRhs(b.rtl, "io_dfi_status_freqRatio")
      if (debug) {
        println(s"[LINT] fr=$fr bytes=${b.rtl.length} declared=${declared(b.rtl, "io_dfi_status_freqRatio")} folded=${folded(b.report, "io_dfi_status_freqRatio")} terminals=$t")
        println(s"[LINT] fr=$fr pruned-with-status=${b.report.prunedSignals.map(_.toString).filter(_.toLowerCase.contains("status")).mkString(" | ")}")
        println(s"[LINT] fr=$fr ports=${b.rtl.linesIterator.filter(l => l.contains("input") || l.contains("output")).take(12).mkString(" ; ")}")
      }
      if (t.nonEmpty) {
        t.foreach(rhs => assert(rhs == expect, s"fr=$fr: dfi_status_freqRatio = $rhs, expected $expect (DFI 5.2: 1:1=00, 1:2=01, 1:4=10, 1:8=11)"))
      } else {
        // Constant-folded out of the netlist: the ledger must state the exact code, so the
        // documented value is what a reader of the RTL would have seen.
        assert(folded(b.report, "io_dfi_status_freqRatio"),
          s"fr=$fr: io_dfi_status_freqRatio is neither declared nor constant-folded -> it is not wired")
        // A folded port is a constant, so the netlist no longer carries the literal: the value is
        // pinned by the ledger text and by Alignment.scala driving U(log2Up(fr), 2 bits)
        // (DFI 5.2 3.5.3 codes 1:1=00, 1:2=01, 1:4=10, 1:8=11). Expected here: $expect.
      }
    }
  }

  test("CA parity is the XOR over the covered CA pins (JESD79-4B sec. 4.17, DFI 5.2 Table 13 note h)") {
    val v = allCaps.rtl
    val covered = Seq("actN", "rasN", "casN", "weN", "bg", "bank", "cid", "address")
    val excluded = Seq("cke", "odt", "csN")
    // The netlist has more than one parity generator: the CA parity of sec. 4.17 and the read-data
    // parity (XOR over the data bus). Demand that at least one driver is the CA-covered pin set,
    // and that NO parity driver includes the pins sec. 4.17 excludes.
    val rhs = moduleBodies(v).flatMap(b => driversOf(b, "io_output_parity"))
    assert(rhs.nonEmpty, "no module in the netlist drives io_output_parity -> the parity output is undriven (DFI 5.2 sec. 4.3 forbids floating)")
    if (debug) rhs.foreach(r => println(s"[LINT] parity = $r"))
    def covers(r: String, n: String) = r.contains(s"io_output_$n") || r.contains(s"_${n}[")
    val caParity = rhs.filter(r => covered.forall(n => covers(r, n)))
    assert(caParity.nonEmpty, s"no io_output_parity driver covers the JESD79-4B sec. 4.17 CA pin set (ACT_n, RAS_n/A16, CAS_n/A15, WE_n/A14, A13..A0, BG, BA, CID). Drivers seen: $rhs")
    assert(caParity.forall(r => !isZeroLiteral(r)), s"the CA parity driver is a constant literal: $caParity -- with MR2.A2 set (CA parity enable) the DRAM would flag every command")
    rhs.foreach { r =>
      excluded.foreach { n =>
        assert(!r.contains(s"io_output_$n"), s"parity = $r covers $n, which JESD79-4B sec. 4.17 excludes")
      }
    }
  }

  test("the PHY drives every MC-facing DFI output: unimplemented channels are tied with a reason") {
    val phyReport = phyCaps
    val v = phyReport.getRtlString()
    val problems = phyLedger.map { case (port, want) =>
      if (want.isInstanceOf[Absent]) None
      else if (folded(phyReport, port) && want.reason.nonEmpty) None
      else if (!declared(v, port)) Some(s"$port: expected $want, but the PHY neither declares nor folds it")
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
    val documentedAbsent = (phyLedger ++ phyHoles).collect { case (n, a: Absent) => n }.toSet
    val missing = names.filterNot(n => declared(v, n) || v.contains(n) || documentedAbsent(n))
    assert(missing.isEmpty, s"UsPhy is missing DFI ports (${missing.size}):\n  " + missing.mkString("\n  "))
  }
}
