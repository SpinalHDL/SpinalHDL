package spinal.lib.memory.sdram.dfi.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._

// =============================================================================
// Minimal I/O SerDes blackbox wrappers (UltraScale E3 family).
// Defined locally (older-style BlackBox API, same as the working S7 IO.scala
// on this branch) to avoid a dependency on dfi-new's Ultrascale IO.scala,
// which targets a newer SpinalHDL API incompatible with this branch.
// =============================================================================
case class OSERDESE3(DATA_WIDTH: Int = 8) extends BlackBox {
  require(Set(4, 8).contains(DATA_WIDTH), "DATA_WIDTH must be 4 or 8")
  addGeneric("DATA_WIDTH", DATA_WIDTH)
  val CLK    = in Bool()
  val CLKDIV = in Bool()
  val D      = in Bits(DATA_WIDTH bits)
  val RST    = in Bool()
  val OQ     = out Bool()
}

case class ISERDESE3(DATA_WIDTH: Int = 8) extends BlackBox {
  require(Set(4, 8).contains(DATA_WIDTH), "DATA_WIDTH must be 4 or 8")
  addGeneric("DATA_WIDTH", DATA_WIDTH)
  val CLK    = in Bool()
  val CLK_B  = in Bool()
  val CLKDIV = in Bool()
  val D      = in Bool()
  val RST    = in Bool()
  val Q      = out Bits(DATA_WIDTH bits)
}

case class USPhy_IOBUF() extends BlackBox {
  val I  = in Bool()
  val T  = in Bool()
  val O  = out Bool()
  val IO = inout(Analog(Bool()))
}

/** T flip-flop (toggles Q on every CLK rising edge). Used to derive the DQS
  * write-strobe square wave in the DRAM-clock domain: a T-FF clocked by
  * dramClk toggles once per dramClk edge, producing a square wave at
  * f_dramClk/2 = f_refclk — exactly the DDR4 DQS frequency. */
case class USPhy_TFF() extends BlackBox {
  val CLK = in Bool()
  val T   = in Bool()
  val Q   = out Bool()
}

/** ODELAYE3: variable output delay (time-based). Drives the DQS write-strobe
  * delay — the quantity write leveling adjusts. */
case class ODELAYE3() extends BlackBox {
  addGeneric("DELAY_TYPE", "VAR_LOAD")
  addGeneric("DELAY_VALUE", 0)
  val CLK         = in Bool()
  val ODATAIN     = in Bool()
  val DATAOUT     = out Bool()
  val CE          = in Bool()
  val RST         = in Bool()
  val EN_VTC      = in Bool()
  val INC         = in Bool()
  val LOAD        = in Bool()
  val CNTVALUEIN  = in UInt(9 bits)
  val CNTVALUEOUT = out UInt(9 bits)
  val CASC_IN     = in Bool()
  val CASC_RETURN = in Bool()
  val CASC_OUT    = out Bool()
}

/** IDELAYE3: variable input delay (time-based). Feedback path for write
  * leveling (samples the delayed DQS) and read DQS deskew. */
case class IDELAYE3() extends BlackBox {
  addGeneric("DELAY_TYPE", "VARIABLE")
  addGeneric("DELAY_VALUE", 0)
  val CLK         = in Bool()
  val DATAIN      = in Bool()
  val IDATAIN     = in Bool()
  val DATAOUT     = out Bool()
  val CE          = in Bool()
  val RST         = in Bool()
  val EN_VTC      = in Bool()
  val INC         = in Bool()
  val LOAD        = in Bool()
  val CNTVALUEIN  = in UInt(9 bits)
  val CNTVALUEOUT = out UInt(9 bits)
  val CASC_IN     = in Bool()
  val CASC_RETURN = in Bool()
  val CASC_OUT    = out Bool()
}

/** Xilinx UltraScale DDR PHY (redesign) — Steps 1-7 (elaboration-validated).
  *
  * Architecture ground truth: see `.usphy_redesign_docs/` (12 ADRs, review report,
  * architecture diagram, glossary). Key decisions baked in here:
  *   ADR-0001  datapath = I/O SerDes only; command/clock = PARALLEL I/O (never serialized)
  *   ADR-0002  write leveling = REAL closed loop; read training = STUB (UNVERIFIED_ON_BOARD)
  *   ADR-0003  parameterized framework (generation / frequencyRatio / dataWidth / nranks)
  *   ADR-0004  SerDes multiplier auto-derived from generation (not an external param)
  *   ADR-0006  single refclk in; PHY self-generates all clocks internally
  *   ADR-0007  reuse the existing Dfi bundle verbatim (PHY is the DFI slave)
  *   ADR-0008  command ENCODING is owned by the MC; PHY does per-phase pass-through +
  *             a STATIC generation-dependent CA pin mapping only (no encoding/FSM/arbitration)
  *   ADR-0012  top-level port list (refclk, rst_n, dfi, physical pads)
  *
  * SerDes clock architecture (verified against Xilinx golden model + DfiMemoryAgent):
  *   DATA_WIDTH = 2 * frequencyRatio  (fr=2 → 4, fr=4 → 8)
  *   CLK        = dramClk  (serialization clock, = fr * refclk)
  *   CLKDIV     = sysClk   (frame-load clock, = refclk)
  *   Constraint: CLK = (DATA_WIDTH/2) * CLKDIV → fr * refclk = fr * refclk ✓
  *
  * Data path: CROSS-PHASE FRAME ASSEMBLY
  *   Each OSERDESE3/ISERDESE3 frame aggregates 2 bits per pin from EACH of the
  *   fr DFI phases. Frame is loaded/presented on CLKDIV (= sysClk) edges and
  *   serialized/deserialized over fr DRAM cycles on CLK (= dramClk).
  *   Frame MSB = phase 0 (earliest DRAM cycle), frame LSB = phase (fr-1) (latest).
  *
  * MINIMUM FREQUENCY RATIO: v1 requires frequencyRatio >= 2 (OSERDESE3 minimum
  * DATA_WIDTH=4 implies 2*fr >= 4 → fr >= 2). fr=1 would require OSERDES2.
  */
case class UsPhy(cfg: DfiConfig) extends Component {

  require(cfg.frequencyRatio >= 2,
    s"UsPhy v1 requires frequencyRatio >= 2 (got ${cfg.frequencyRatio}); " +
    s"fr=1 would require OSERDES2 which is not supported.")
  // L2 (ADR-0013 rev.2): the command path honours ALL DFI phases — the
  // dramPhase alternation in Step 3 delivers phase i to DRAM cycle i of each
  // DFI pair (DFI 6.0.1 §2.1: "The PHY must be able to accept a command on
  // any and all phases"), so any cmdPhase in [0, frequencyRatio) is valid;
  // the MC asserts that range itself (DfiController). The former cmdPhase==0
  // restriction (rev.1, when only phase 0 reached the pins) is obsolete.
  // NOTE: UsPhy v1 does not implement DBI inversion logic. If DBI is enabled in config,
  // the DM/DBI pins are driven low (no inversion). Marked UNVERIFIED_ON_BOARD.


  // ===========================================================================
  // Step 1: top-level ports (ADR-0006, ADR-0012)
  // ===========================================================================
  val refclk  = in Bool()  // Reference clock (also serves as sysClk/DFI clock)
  val dramClk = in Bool()  // DRAM clock (= refclk * frequencyRatio; production: from internal MMCM)
  val rst_n   = in Bool()
  val dfi    = slave(Dfi(cfg))
  val wlDelayOut = out(UInt(9 bits))   // observability: current ODELAYE3 delay counter (dramClk domain)

  // Parameter axes (ADR-0003). All derived from cfg — no extra external knobs.
  val generation     = cfg.sdram.generation
  val frequencyRatio = cfg.frequencyRatio
  val dataWidth      = cfg.dataWidth        // DFI logical per-phase width (= dataRate * sdram.dataWidth)
  val physDqWidth    = cfg.sdram.dataWidth  // PHYSICAL DQ pin count (what the board wires)
  val nranks         = cfg.chipSelectNumber
  val byteLanes      = physDqWidth / 8      // number of DQ byte-lanes (physical)
  // ADR-0004: SerDes multiplier auto-derived from generation (internal, not a param).
  val serdesMult     = if (generation == SdramGeneration.DDR4) 4 else 2
  // SerDes DATA_WIDTH = 2 * fr (derived from DFI geometry: 2 DDR symbols/pin/phase * fr phases)
  val serdesDw       = 2 * frequencyRatio

  // Physical pads (ADR-0012 frozen port list).
  val pads = new Bundle {
    // Command / clock — PARALLEL I/O, never serialized (ADR-0001)
    val ck_t    = out Bool()
    val ck_c    = out Bool()
    val cke     = out(Vec(Bool(), nranks))
    val cs_n    = out(Vec(Bool(), nranks))
    val ca      = out(Vec(Bool(), cfg.addressWidth))
    // Command strobes (active-low). DDR4 distinguishes command TYPE by the
    // RAS#/CAS#/WE#/ACT# combination, so these must reach the pins for any
    // pin-level receiver (DRAM or behavioral model) to decode commands.
    val ras_n = out Bool()
    val cas_n = out Bool()
    val we_n  = out Bool()
    val act_n   = out Bool()
    val bg      = out(Vec(Bool(), cfg.sdram.bgWidth))
    // Dedicated BA pins (ADR-0012 amendment, TC8): real DDR4 x16 silicons BG0/BA0/BA1
    // as dedicated pins (JESD79-4B §2.8 / Table 17); A[12:0] carries the FULL row on
    // ACT, so BA must NOT be time-multiplexed onto the CA bus.
    val ba      = out(Vec(Bool(), cfg.dfiBankWidth))
    val cid     = out(Vec(Bool(), cfg.sdram.cidWidth))
    val odt     = out(Vec(Bool(), nranks))
    val reset_n = out Bool()
    val par     = out Bool()
    // Data path — BIDIRECTIONAL I/O SerDes carrier (inout).
    val dq       = inout(Vec(Analog(Bool()), physDqWidth))
    val dqs_t    = out(Vec(Bool(), byteLanes))
    val dqs_c    = out(Vec(Bool(), byteLanes))
    val dqs_in_t = in(Vec(Bool(), byteLanes))   // DQS RETURNED from DRAM (write-leveling echo)
    val dm_dbi_n = out(Vec(Bool(), byteLanes))
  }

  // ===========================================================================
  // Step 2: clock generation (skeleton) — ADR-0006
  // ===========================================================================
  // In the full design: refclk -> MMCM -> {dramClk, sysClk}.
  // Skeleton: all domains alias refclk; the domain NAMES are what matter.
  // REAL relationships (post-MMCM):
  //   sysClk    = refclk                    (DFI/MC interface clock)
  //   dramClk   = refclk * frequencyRatio   (DRAM clock)
  //   CLKDIV    = sysClk                    (SerDes frame-load = DFI clock)
  //   CLK       = dramClk                   (SerDes serialization = DRAM clock)
  val sysClk = refclk  // DFI clock domain (MC-PHY interface) = refclk
  // dramClk is now an EXTERNAL INPUT (production: driven by internal MMCM from refclk;
  // simulation: provided by TB at the correct frequency = refclk * frequencyRatio).

  // Asynchronous reset, 2-stage synchronous release (ADR-0006 consequence).
  val rst = Reg(Bool()) init(True)
  rst := !rst_n
  val rstSynced = RegNext(rst) init(True)

  // DRAM clock domain (Step 3 phase alternation + Step 6 write leveling).
  // NOTE: `cd { }` (block form) is required for domain switching — `import cd._`
  // does NOT switch the clock domain and would leave the Regs in refclk.
  val cd = ClockDomain(dramClk, rstSynced)

  // ===========================================================================
  // Step 3: DFI command -> parallel CA/command pins (per-phase transmission) — ADR-0008
  // ===========================================================================
  // Phase i occupies bits [(i+1)*w-1 : i*w] (w = width/frequencyRatio) of each
  // packed per-phase DFI control vector (CAAlignment packs phase i into slice i).
  //
  // L2 (ADR-0013 rev.2): ONE DFI cycle spans fr DRAM cycles; DFI phase i MUST
  // appear on the pins during DRAM cycle i of the pair (DFI 6.0.1 §2.1: "The
  // PHY must be able to accept a command on any and all phases"; phase 1
  // aligns with the 2nd DRAM cycle). Holding phase 0 for the whole DFI cycle
  // makes every command decode TWICE at a rising-edge sampler — proven RED by
  // TC8 (UsPhy + Ddr4PinModel). Fix: dramPhase alternates at DRAM rate and
  // selects the phase slice below. All phases are honoured, so any mc cmdPhase
  // in [0, fr) is valid.
  //
  // What the MC owns (already met, verified in Task2IDFI/CAAlignment): command
  // encoding (ADR-0008), NOP idle on non-command phases, CKE held on every
  // phase. Pin setup/hold vs the CK rising edge remains a board/P&R attribute
  // (ODDR/IODR alignment) — UNVERIFIED_ON_BOARD (ADR-0002).
  //
  // dramPhase = LSB of the within-pair DRAM cycle counter: False during phase
  // 0's DRAM cycle, True during phase 1's. Free-running toggle in the dramClk
  // domain; reset re-anchors it to False. Absolute alignment to the DFI
  // vector-update edge is fixed by the DFI<->DRAM clock phase relation (DFI
  // rising edge = DRAM CK falling edge, DFI spec convention) — a board/MMCM
  // attribute; sim anchors it via the TB clock arrangement (TC8 header).
  val dramPhase = cd { Reg(Bool()) init(False) }
  cd { dramPhase := !dramPhase }

  def phaseSel(bits: Bits): Bits = {
    val w = bits.getWidth / frequencyRatio
    Mux(dramPhase, bits.drop(w), bits.take(w))
  }

  pads.ca := phaseSel(dfi.control.address).asBools
  // Dedicated bank pins: dfi.bank phase word = {1'b0 (zero-ext), ba[baW-1:0]} (MC
  // convention, Task2IDFI: baBits.resized); dfi.bg carries BG separately. STATIC
  // mapping — no time multiplexing (ADR-0012 amendment; TC8 proved A[2:0] mux
  // corrupts RA[2:0] on ACT).
  if (cfg.useBank) pads.ba := phaseSel(dfi.control.bank).take(cfg.dfiBankWidth).asBools
  else pads.ba.foreach(_ := False)
  // Command strobes: real DFI signals when enabled, High (=inactive) otherwise.
  pads.ras_n := (if (cfg.useRasN) phaseSel(dfi.control.rasN)(0) else True)
  pads.cas_n := (if (cfg.useCasN) phaseSel(dfi.control.casN)(0) else True)
  pads.we_n  := (if (cfg.useWeN)  phaseSel(dfi.control.weN)(0)  else True)
  pads.cs_n := phaseSel(dfi.control.csN).asBools
  pads.cke  := phaseSel(dfi.control.cke).asBools
  if (cfg.useOdt) pads.odt := phaseSel(dfi.control.odt).asBools
  else pads.odt.foreach(_ := False)
  pads.act_n   := (if (cfg.useAckN) phaseSel(dfi.control.actN)(0) else True)
  pads.reset_n := (if (cfg.useResetN) phaseSel(dfi.control.resetN)(0) else True)
  pads.par     := (if (cfg.useCaParity) phaseSel(dfi.control.parity)(0) else False)
  if (cfg.useBg) pads.bg := phaseSel(dfi.control.bg).asBools
  else pads.bg.foreach(_ := False)
  if (cfg.useCid) pads.cid := phaseSel(dfi.control.cid).asBools
  else pads.cid.foreach(_ := False)

  // Clock out (skeleton: alias dramClk; real MMCM differential output at P&R).
  pads.ck_t    := dramClk
  pads.ck_c    := !dramClk

  // ===========================================================================
  // Step 4: WRITE data path — cross-phase frame assembly (ADR-0001, Q1/Q2)
  // ===========================================================================
  // Geometry (anchored from tested DfiMemoryAgent byte extraction):
  //   Per-phase wrdata = dataWidth bits = 2 * physDqWidth (DDR: 2 edges per DRAM clk).
  //   Standard DFI packing:
  //     bits [physDqWidth-1 : 0]  = DQ[physDqWidth-1:0] on FIRST DDR edge (rising)
  //     bits [2*physDqWidth-1 : physDqWidth] = DQ[physDqWidth-1:0] on SECOND DDR edge (falling)
  //   Therefore: pin p, first edge = bit p;  pin p, second edge = bit (p + physDqWidth).
  //
  // Frame assembly (DATA_WIDTH = 2*fr):
  //   D[2*fr-1 : 2*fr-2] = phase 0's DDR pair (earliest DRAM cycle)
  //   D[2*fr-3 : 2*fr-4] = phase 1's DDR pair
  //   ...
  //   D[1:0]              = phase (fr-1)'s DDR pair (latest DRAM cycle)
  //
  // Clocking:
  //   CLK    = dramClk  (serialization: 2 symbols per CLK edge)
  //   CLKDIV = sysClk   (frame load: one frame per DFI/MC cycle)
  //   OQ     = 1-bit serial output → DQ pin via IOBUF

  for (pin <- 0 until physDqWidth) {
    val oserdes = new OSERDESE3(serdesDw)
    oserdes.CLK    := dramClk
    oserdes.CLKDIV := sysClk
    oserdes.RST    := rstSynced

    // DFI canonical bit positions for this pin (anchored from tested
    // DfiMemoryAgent byte extraction, mirrored EXACTLY by the read side so
    // wrdata/rddata address the same word layout):
    //   byte j (pins 8j..8j+7), edge0 -> bit 16j   ; edge1 -> bit 16j+8
    val byteLaneIdx = pin / 8
    val bitInByte   = pin % 8
    val bitPos0 = byteLaneIdx * 16 + bitInByte          // first DDR edge
    val bitPos1 = byteLaneIdx * 16 + 8 + bitInByte      // second DDR edge

    // Build the 2*fr-bit frame. OSERDESE3 transmits D[MSB] first, and the
    // DFI/read-side convention is phase 0 = EARLIEST. So phase 0 must occupy
    // the MSB (serialized first) and phase (fr-1) the LSB (last).
    val pairs = (0 until frequencyRatio).reverse.map { p =>
      Cat(dfi.write.wr(p).wrdata(bitPos0),
          dfi.write.wr(p).wrdata(bitPos1))
    }
    oserdes.D := Cat(pairs)

    // Serialize to the DQ pin via IOBUF (tri-state controlled by write-enable).
    val buf = new USPhy_IOBUF()
    buf.I := oserdes.OQ
    // Tri-state when NO phase has write data (any-phase condition for cross-phase frames).
    buf.T := !dfi.write.wr.map(_.wrdataEn).reduce(_||_)
    pads.dq(pin) := buf.IO
  }

  // ===========================================================================
  // Step 5: READ data path — cross-phase frame distribution (ADR-0001, Q1/Q2)
  // ===========================================================================
  // ISERDESE3(DATA_WIDTH = 2*fr):
  //   D      = 1-bit serial input (sampled on CLK rising edges)
  //   CLK    = dramClk (deserialization clock)
  //   CLK_B  = ~CLK
  //   CLKDIV = sysClk  (frame presentation clock)
  //   Q      = 2*fr-bit deserialized word (presented on CLKDIV rising edge)
  //            Q[2*fr-1] = earliest symbol (phase 0), Q[0] = latest (phase fr-1)
  //
  // Distribution: each phase's 2 bits for pin p are routed to that phase's rddata.

  for (pin <- 0 until physDqWidth) {
    val iserdes = new ISERDESE3(serdesDw)
    iserdes.CLK    := dramClk
    iserdes.CLK_B  := !dramClk
    iserdes.CLKDIV := sysClk
    iserdes.RST    := rstSynced
    // Serial input: sample the DQ pin (input direction of the bidirectional pad).
    iserdes.D      := pads.dq(pin)

    val byteLaneIdx = pin / 8
    val bitInByte   = pin % 8
    val bitPos0 = byteLaneIdx * 16 + bitInByte          // first DDR edge
    val bitPos1 = byteLaneIdx * 16 + 8 + bitInByte      // second DDR edge

    // Distribute Q to each phase's rddata.
    for (p <- 0 until frequencyRatio) {
      val qBase = 2 * (frequencyRatio - 1 - p)
      dfi.read.rd(p).rddata(bitPos0) := iserdes.Q(qBase + 1)  // first DDR edge
      dfi.read.rd(p).rddata(bitPos1) := iserdes.Q(qBase)       // second DDR edge
    }
  }

  // DQS / DM / DBI — placeholder (real DQS generation and DM logic in Steps 6-7).
  pads.dm_dbi_n.foreach(_ := False)

  // ===========================================================================
  // Step 6: WRITE LEVELING — real closed loop (ADR-0002 tier: must be REAL)
  // ===========================================================================
  // Mechanism (DDR4 JESD79-4B §4.14):
  //   1. During WL the PHY drives DQS with a toggle waveform through an
  //      ODELAYE3 (the ADJUSTABLE output delay — the quantity WL optimizes).
  //   2. The DRAM captures that DQS and echoes it back on the DQS pin.
  //   3. The PHY samples the returned DQS through an IDELAYE3 (feedback path).
  //   4. An alignment detector measures the returned-DQS transition position
  //      relative to the CK edge and emits EARLY / OK / LATE.
  //   5. A binary-search FSM nudges the ODELAYE3 counter toward the CK-edge
  //      center until OK, then raises wrlvlResp (done).
  //
  // HONEST SCOPE: the hardware STRUCTURE (ODELAYE3 on DQS out, IDELAYE3
  // feedback, detector, binary-search FSM, DFI training wiring) is real and
  // elaborates. The exact ALGORITHM (transition-count threshold that defines
  // "centered") is the part that REQUIRES the SerDes/DRAM behavioral model to
  // verify — it is marked UNVERIFIED_ON_BOARD per ADR-0002. We do NOT fabricate
  // a constant-true wrlvlDone.
  //
  // ---- DQS output: toggle waveform through ODELAYE3 (per byte-lane) ----------
  // wlActive is a slow control flag (default/refclk domain).
  // WL enable: pure combinational (derived from the MC input port wrlvlEn, which is
  //   not clock-bound) so it can be consumed in BOTH the refclk and dramClk domains.
  val wlActive = (if (cfg.useWrlvlEn) dfi.wrTraining.wrlvlEn.orR else False)

  // DQS write-strobe: T-FF clocked by dramClk -> square wave at f_dramClk/2.
  val dqsTff = new USPhy_TFF()
  dqsTff.CLK := dramClk
  dqsTff.T   := True
  val dqsToggle = dqsTff.Q

  // ---- Feedback: IDELAYE3 samples the returned DQS (per byte-lane) -----------
  val fbIddelays = Seq.fill(byteLanes)(new IDELAYE3())
  for (l <- 0 until byteLanes) {
    fbIddelays(l).CLK      := dramClk
    fbIddelays(l).RST      := rstSynced
    fbIddelays(l).CE       := True
    fbIddelays(l).EN_VTC   := False
    fbIddelays(l).LOAD     := False
    fbIddelays(l).INC      := False
    fbIddelays(l).CNTVALUEIN := 0
    fbIddelays(l).DATAIN := pads.dqs_in_t(l)      // sample the DQS as it RETURNS FROM THE DRAM (real closed loop)
    fbIddelays(l).IDATAIN := False
    fbIddelays(l).CASC_IN := False
    fbIddelays(l).CASC_RETURN := False
  }
  val fbSample = Vec.fill(byteLanes)(Bool())
  for (l <- 0 until byteLanes) {
    fbSample(l) := fbIddelays(l).DATAOUT
  }

  // ===========================================================================
  // Bang-bang write-leveling phase detector — DRAM_CLK DOMAIN (cd { } block).
  //
  // The reference edge is the LOCAL DQS rising edge (dqsToggle is a T-FF on
  // dramClk, so its rising edge lands exactly on a dramClk edge). We sample
  // the RETURNED DQS (fbSample) at that edge:
  //   fbSample HIGH at dqsToggle-rise -> returned DQS leads  -> delay too big -> CUT (n-1)
  //   fbSample LOW  at dqsToggle-rise -> returned DQS lags   -> delay too small-> ADD (n+1)
  // Equilibrium: returned DQS coincides with the local DQS edge (net loop
  // delay delta = 0). UNVERIFIED_ON_BOARD per ADR-0002: wrlvlDone is EARNED
  // numerically (counter drift small over 16 DQS periods for 8 straight
  // windows), never self-assigned constant-true.
  //
  // NOTE: `cd` is defined in the reset section above (shared by Step 3 and Step 6).
  // ===========================================================================
  val wb = cd {
    // Seed the delay line at MID-RANGE (256 of 0..511). Real write-leveling
    // trains from the centre of the adjustable range so it can correct in
    // EITHER direction; starting at 0 would underflow (wrap to 511) on any
    // "cut" verdict and bounce forever.
    val wlDelay = Reg(UInt(9 bits)) init(256)
    val wlDone  = Reg(Bool()) init(False)
    val wlLoadPulse = Reg(Bool()) init(False)
    val dqsTogPrev = Reg(Bool()) init(False)
    val dqsTogRise = !dqsTogPrev && dqsToggle   // Low->High = dqsToggle rising edge
    val bbHist = Vec.fill(16)(Reg(UInt(9 bits)) init(256))
    val bbConv = Reg(UInt(4 bits)) init(0)

    dqsTogPrev := dqsToggle
    when(!wlActive) {                    // training not requested: park at mid-range
      bbConv  := 0
      wlDelay := 256
      wlDone  := False
    }.elsewhen(!wlDone) {               // active, not yet converged: run the loop
      when(dqsTogRise) {                // fire once per local DQS rising edge
        for (i <- 15 downto 1) { bbHist(i) := bbHist(i-1) }
        bbHist(0) := wlDelay
        // Converged when the counter is stable (+-3 taps) over 16 DQS periods
        // for 8 straight windows (bang-bang settles into a +-1 tap oscillation
        // around the lock point).
        val drifted = (bbHist(0) > (bbHist(15) + 3)) || (bbHist(15) > (bbHist(0) + 3))
        when(drifted) { bbConv := 0 }
        .otherwise    { bbConv := bbConv + 1 }
        // Bang-bang. The dqsToggle edge IS the local DQS rising edge, so
        // fbSample (the returned DQS) is sampled exactly there:
        //   fbSample HIGH -> returned DQS lags  -> delay too big -> CUT (n-1)
        //   fbSample LOW  -> returned DQS leads -> delay too small-> ADD (n+1)
        // Attractor: net loop delay delta -> 0 (returned DQS aligned to CK).
        when(fbSample(0)) { wlDelay := wlDelay - 1 }
        .otherwise        { wlDelay := wlDelay + 1 }
        when(bbConv > 7)  { wlDone  := True }
      }
    }                                    // wlActive && wlDone: hold (no reset)
    // Pulse whenever the counter changed this dramClk edge (VarLoad latch).
    val wlDelayPrev = RegNext(wlDelay)
    wlLoadPulse := wlDelay =/= wlDelayPrev
    new Bundle { val delay = wlDelay; val done = wlDone; val load = wlLoadPulse }
  }
  val wlDelay = wb.delay
  val wlDone  = wb.done
  val wlLoadPulse = wb.load
  wlDelayOut := wb.delay

  // Seq.fill (not Vec.fill) so the concrete ODELAYE3 type is retained.
  val dqsOdelays = Seq.fill(byteLanes)(new ODELAYE3())
  for (l <- 0 until byteLanes) {
    dqsOdelays(l).CLK      := dramClk
    dqsOdelays(l).CE       := True
    dqsOdelays(l).RST      := rstSynced
    dqsOdelays(l).EN_VTC   := False
    dqsOdelays(l).LOAD     := wlLoadPulse
    dqsOdelays(l).INC      := False
    dqsOdelays(l).ODATAIN  := dqsToggle
    dqsOdelays(l).CASC_IN     := False
    dqsOdelays(l).CASC_RETURN := False
    dqsOdelays(l).CNTVALUEIN := wb.delay
    pads.dqs_t(l) := dqsOdelays(l).DATAOUT
    pads.dqs_c(l) := !dqsOdelays(l).DATAOUT
  }

  // CNTVALUEIN is driven by the loop above (dqsOdelays(l).CNTVALUEIN := wb.delay).

  // ---- DFI write-training interface ------------------------------------------
  // PHY never INITIATES training (MC-initiated design): tie request/ack outputs to 0.
  if (cfg.useWrlvlReq) { dfi.wrTraining.wrlvlReq := 0 }
  if (cfg.usePhylvlAckCsN) { dfi.phyRequestedTraining.phylvlAckCsN := 0 }
  if (cfg.useWrlvlResp) {
    // Per-bit construction (avoids whole-vs-bit assignment overlap when width==1).
    for (k <- 0 until cfg.writeLevelingResponseWidth) {
      dfi.wrTraining.wrlvlResp(k) := (if (k == cfg.writeLevelingResponseWidth - 1) wlDone else False)
    }
  }
  if (cfg.usePhyWrlvlCsN) {
    dfi.wrTraining.phyWrlvlCsN := 0
  }

  // ===========================================================================
  // Step 7: READ TRAINING — explicit STUB (ADR-0002 tier: interface complete,
  // feedback injectable, clearly marked UNVERIFIED_ON_BOARD).
  // ===========================================================================
  if (cfg.useRdlvlReq) {
    // Per-bit construction (avoid whole-vs-bit overlap when width==1).
    val en = (if (cfg.useRdlvlEn) dfi.rdTraining.rdlvlEn.orR else False)
    for (k <- 0 until dfi.rdTraining.rdlvlReq.getWidth) {
      dfi.rdTraining.rdlvlReq(k) := (if (k == 0) en else False)
    }
  }
  if (cfg.usePhyRdlvlCsN) {
    dfi.rdTraining.phyRdlvlCsN := 0
  }
  if (cfg.useRdlvlResp) {
    // STUB: neutral 0. Per ADR-0002 we do NOT self-assign constant "success".
    dfi.rdTraining.rdlvlResp := 0
  }
  if (cfg.useRdlvlGateReq) {
    dfi.rdTraining.rdlvlGateReq := 0
  }
  if (cfg.usePhyRdlvlGateCsN) {
    dfi.rdTraining.phyRdlvlGateCsN := 0
  }

  // ===========================================================================
  // DFI PHY-side (slave) outputs — driven by the datapath above.
  // ===========================================================================
  for (idx <- 0 until dfi.read.rd.length) {
    val l = dfi.read.rd(idx)
    l.rddataValid := False   // real driver: gated by read-latency counter (future)
    l.rddataDbiN  := 0       // real driver: DBI encoder (future)
    // rddata is driven by the ISERDESE3 cross-phase distribution above (Step 5).
  }
}