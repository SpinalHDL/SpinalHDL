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
  // DBI/DM (JESD79-4B §4.11) is WIRED THROUGH (2026-10-31, conformance audit item 3):
  //   write: the DM pin carries the per-lane DBI_n bit (useWrDbi) or the byte-mask
  //          (non-DBI), serialized in lockstep with DQ via a dedicated OSERDESE3.
  //   read:  rddataDbiN comes from pads.dm_dbi_in (DRAM-driven DM during reads).
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
  val rdDelayOut = out(UInt(9 bits))   // observability: current read-leveling IDELAYE3 delay counter (dramClk domain)

  // DFI 6.0.1 management plane (status channel), minimal v1 (ADR-0016): this PHY has no
  // internal initialization state (no MMCM lock wait, no internal training), so
  // dfi_init_complete is asserted as soon as the bundle exists; dfi_alert_n stays high
  // (no error source). dfi_init_start is accepted but has NO internal action - there is
  // nothing to initialize.
  if (cfg.useInitStart) dfi.status.initComplete := True
  if (cfg.useAlertN) dfi.status.alertN.setAll()

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
    val dm_dbi_in = in(Vec(Bool(), byteLanes))  // DM RETURNED from DRAM during reads (DBI_n)
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

  // Write-enable (any phase) in the sysClk domain. With the silicon-semantics
  // OSERDESE3 model (CLKDIV posedge load, see sim/tc10/custom_unisims_sim.v),
  // a frame loaded at refclk edge t is serialized over the 4 DDR edges STARTING
  // AT t (model omits the ~2-CLK silicon output pipeline). The DQ burst window
  // is therefore the wren window shifted by one refclk cycle (+10ns = one full
  // DQS period, so DQ-vs-DQS relative alignment is unchanged):
  //   wren high [ws, ws+20)  ->  DQ active [ws+10, ws+35).
  val wrEnLive = dfi.write.wr.map(_.wrdataEn).reduce(_||_)
  val wrEnFall = ClockDomain(!sysClk, rstSynced) { Reg(Bool()) init(False) }
  wrEnFall := wrEnLive
  // Extended DQ-active window (pin-independent: pure RegNext chain of wrEnLive).
  // Shared by the DQ IOBUFs and the DM/DBI serializers below.
  val wrEnExt  = Reg(Bool()) init(False)
  val wrEnExt2 = Reg(Bool()) init(False)
  wrEnExt  := wrEnLive
  wrEnExt2 := wrEnExt

  for (pin <- 0 until physDqWidth) {
    val oserdes = new OSERDESE3(serdesDw)
    oserdes.CLK    := dramClk
    // Silicon wiring: CLKDIV = sysClk (non-inverted). The golden-model NBA
    // sampling workaround (CLKDIV := !sysClk) is retired now that the bench
    // uses the silicon-semantics behavioral OSERDESE3 model.
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
    // Gate D with wren: while wren is low the MC HOLDS the previous beat on
    // wrdata, which the OSERDESE3 would otherwise reload (and re-serialize)
    // every frame-load edge, duplicating the data on DQ. Force D=0 outside the
    // wren window so only in-window frames (frag0, frag1) reach the pins.
    val frame = Cat(pairs)
    for (i <- 0 until frame.getWidth) {
      oserdes.D(i) := Mux(wrEnLive, frame(i), False)
    }

    // Serialize to the DQ pin via IOBUF (tri-state controlled by write-enable).
    val buf = new USPhy_IOBUF()
    buf.I := oserdes.OQ
    // DQ-active window: wren OR wren-delayed-1-or-2-cycles. With the
    // silicon-semantics OSERDESE3 model (zero-latency first bit, 8 contiguous
    // DDR edges per 2-cycle wren window: burst [ws, ws+35)), the last data beat
    // lands ~1.5 refclk cycles after the last wren-high edge. Extending T by
    // 2 cycles covers it.
    buf.T := !(wrEnLive | wrEnExt | wrEnExt2)
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
    // READ uses simple LE byte order (matching DfiMemoryAgent convention):
    //   byte j of the 32-bit rddata word sits at bits[j*8+7 : j*8]
    //   byte 0 = DQ[7:0] @ first edge, byte 1 = DQ[15:8] @ first edge,
    //   byte 2 = DQ[7:0] @ second edge, byte 3 = DQ[15:8] @ second edge
    val rdBitPos0 = byteLaneIdx * 16 + bitInByte            // first DDR edge (mirror write bitPos)
    val rdBitPos1 = byteLaneIdx * 16 + 8 + bitInByte        // second DDR edge (mirror write bitPos)

    // EXACT INVERSE of the write frame order (write->wire->read composes to
    // identity). Ground truth from the generated netlist (pin p, fr=2):
    //   Write frame D[2fr-1:0] = {wr0.e0, wr0.e1, wr1.e0, wr1.e1} (phase 0 at
    //   MSB). OSERDESE3 emits D[2fr-1] FIRST, so the wire time order
    //   (earliest->latest) is: w1=wr0.e0, w2=wr0.e1, w3=wr1.e0, w4=wr1.e1,
    //   i.e. w(2k+1)=wr_k.e0, w(2k+2)=wr_k.e1.
    //   ISERDESE3 (silicon-semantics model): Q[0]=NEWEST, Q[2fr-1]=OLDEST,
    //   so Q[j] = w(2fr-j)  =>  Q[3]=w1=wr0.e0, Q[2]=w2=wr0.e1,
    //   Q[1]=w3=wr1.e0, Q[0]=w4=wr1.e1.
    //   Inverse: phase p e0-slot <- wr_p.e0 = w(2p+1) = Q[2fr-(2p+1)] = Q[2(fr-1-p)+1]
    //            phase p e1-slot <- wr_p.e1 = w(2p+2) = Q[2fr-(2p+2)] = Q[2(fr-1-p)]
    for (p <- 0 until frequencyRatio) {
      val qPair = 2 * (frequencyRatio - 1 - p)
      dfi.read.rd(p).rddata(rdBitPos0) := iserdes.Q(qPair + 1)
      dfi.read.rd(p).rddata(rdBitPos1) := iserdes.Q(qPair)
    }
  }

  // ===========================================================================
  // DM / DBI serialization (JESD79-4B §4.11) — LOCKSTEP WITH DQ BY CONSTRUCTION.
  // ===========================================================================
  // The DRAM samples DQ and DM together: the DM pin must present the DBI_n bit
  // (DBI mode) or byte-mask (non-DBI) of the byte currently on the DQ wire.
  // Each lane gets a DEDICATED OSERDESE3 fed with the 2*fr DM slots in the same
  // order as the DQ frame ([p0.ui0, p0.ui1, p1.ui0, p1.ui1] ...) and driven with
  // the SAME CLK/CLKDIV/RST/window as that lane's DQ OSERDESE3, so the silicon-
  // semantics FIFO model pops DM slot k exactly when DQ beat k reaches the pad.
  // Per-phase byte j layout (matches wrdata bit positions): j=0..lane-1 -> ui0,
  // j=lane.. -> ui1; hence lane l's ui0/ui1 bits are src(l) / src(l+byteLanes).
  // Outside the DQ-active window the pad parks at 1 (DM inactive / DBI_n=1 =
  // not inverted) — the model only samples DM inside collected beats.
  val dmSrc = (0 until frequencyRatio).map(p =>
    if (cfg.useWrDbi) dfi.write.wr(p).wrdataDbiN else dfi.write.wr(p).wrdataMask)
  for (l <- 0 until byteLanes) {
    val dmOs = new OSERDESE3(serdesDw)
    dmOs.CLK    := dramClk
    dmOs.CLKDIV := sysClk
    dmOs.RST    := rstSynced
    // Mirror the DQ D-gather order EXACTLY. Phase-word layout is GROUPED BY
    // LANE: lane l occupies bits [16l+15:16l] (low 8 = ui0 = byte 2l, high 8
    // = ui1 = byte 2l+1) — the DQ netlist gathers {wr_0[0], wr_0[8], wr_1[0],
    // wr_1[8]} proving lane0-ui0, lane0-ui1, lane1-ui0, lane1-ui1 wire order.
    // DM must follow: per lane Cat(ui0_db, ui1_db), ui0 at MSB (emitted first).
    val dmPairs = (0 until frequencyRatio).reverse.map(p =>
      Cat(dmSrc(p)(2 * l), dmSrc(p)(2 * l + 1)))
    val dmFrame = Cat(dmPairs)
    for (i <- 0 until dmFrame.getWidth) {
      dmOs.D(i) := Mux(wrEnLive, dmFrame(i), True)
    }
    pads.dm_dbi_n(l) := Mux(wrEnLive | wrEnExt | wrEnExt2, dmOs.OQ, True)
  }

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

  // ---------------------------------------------------------------------------
  // DQS BLANKING (JESD79-4B §4.20/§4.21, AC table tWPRE/tWPST/tDSS/tDSH/tDQSS)
  //   The DQS pad must not toggle outside the write-burst envelope: the spec
  //   warns that violating the DQS timing parameters "may cause incorrect
  //   data to be written". Inside the envelope the leveled DQS (TFF ->
  //   ODELAYE3) passes through; outside it is PARKED (frozen at the last
  //   value, hence no transitions). The WL-active span is exempt: write
  //   leveling drives DQS as a training pattern outside data bursts.
  //   Envelope (measured, silicon-semantics OSERDESE3 model): wren window
  //   [ws, ws+20) ns (2 refclk cycles) -> DQ burst [ws+5, ws+25) ns (8 beats
  //   on dramClk edges). The gate window is therefore [ws, ws+32.5) via the
  //   6-stage dramClk tail below: it covers the burst plus the tWPST
  //   postamble margin (~7.5 ns past the last beat, cf. model observer
  //   POST_UI=5). The free-running TFF is left untouched (WL detector
  //   reference and ODELAYE3 feed keep their phase); only the PAD is gated.
  //   Absolute DQS-DQ phase is NOT calibrated here (silicon property: the
  //   SerDes fabric + WL training establish it); the model observer checks
  //   BLANKING (no transitions outside the window) and activity AROUND the
  //   data with two-sided preamble grace, not the absolute phase.
  // ---------------------------------------------------------------------------
  // 2-stage dramClk-domain tail: each stage delays by a FULL dramClk period
  // (5 ns), so the gate window is [ws, ws+32.5) for a wren window [ws, ws+20).
  // Measured geometry (silicon-semantics OSERDESE3 model): DQ data beats
  // occupy [ws+5, ws+25) (8 beats on dramClk edges); the model observer's
  // post window (last beat + POST_UI=5 edges) ends at ws+35, so the DQS pad
  // must be parked by ws+32.5 at the latest. wrEnLive is refclk-domain; the
  // first dramClk sample lands 2.5 ns after its rising edge (refclk posedge
  // == dramClk negedge), so no coincident-edge race. (Silicon note: a 2FF
  // synchronizer would be inserted here; the margin absorbs its latency.)
  val dqsTail = Vec.fill(2)(cd { Reg(Bool()) init(False) })
  cd {
    dqsTail(0) := wrEnLive
    dqsTail(1) := dqsTail(0)
  }
  val dqsParks = Vec.fill(byteLanes)(cd { Reg(Bool()) init(False) })

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
  }
  // Gate the pad in the dramClk domain: live inside the envelope, parked
  // outside. dqsWinSlow is COMPUTED HERE (dramClk domain) so the tail vector
  // is never read cross-domain (the refclk->dramClk reads of wrEnLive/wlActive
  // are plain synchronous crossings into this domain).
  cd {
    val dqsWinSlow = wrEnLive | Cat(dqsTail).orR | wlActive
    for (l <- 0 until byteLanes) {
      when(dqsWinSlow) { dqsParks(l) := dqsOdelays(l).DATAOUT }
      pads.dqs_t(l) := Mux(dqsWinSlow, dqsOdelays(l).DATAOUT, dqsParks(l))
      pads.dqs_c(l) := !pads.dqs_t(l)
    }
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
  // Step 7: READ LEVELING — protocol closed loop + real adjustable element
  // (ADR-0002 tier A upgraded to Plan A: interface + algorithm complete; the
  // functional coupling of the trained delay to DQ sampling is a MODEL
  // boundary — the behavioral ISERDESE3 samples on the dramClk grid, not on
  // the delayed DQS, per ADR-0014's zero-latency-model spirit).
  //
  // Structure (mirrors the write-leveling closed loop):
  //   * Adjustable element: IDELAYE3 per byte-lane on the RECEIVE DQS path
  //     (pads.dqs_in_t -> IDELAYE3(CNTVALUEIN = rlDelay) -> rlSample).
  //   * Reference: the free-running dqsToggle T-FF (dramClk grid) — in
  //     silicon the reference is the DRAM's read DQS; the bench supplies a
  //     TB-side oscillator on the receive pad during rdlvlEn (documented
  //     simulation surrogate).
  //   * Bang-bang detector: samples rlSample(0) at each dqsToggle rising
  //     edge; HIGH -> delay too big -> CUT, LOW -> too small -> ADD.
  //     Attractor: delayed edge lands half-period (2500ps) past the
  //     reference edge, i.e. rlDelay*50ps ~= 2500ps (mod 5000ps).
  //   * Convergence: counter drift < 3 taps over 16 edges for 8 straight
  //     windows -> rlDone EARNED -> rdlvlResp asserted (never
  //     self-assigned constant-true).
  //   * rdlvlReq stays 0: Plan A needs no training-read exchange (the
  //     reference is the pad oscillator, not a data eye).
  // ===========================================================================
  val rlActive = (if (cfg.useRdlvlEn) dfi.rdTraining.rdlvlEn.orR else False)

  // Receive-path delay elements (the quantity read leveling optimizes).
  val rlIddelays = Seq.fill(byteLanes)(new IDELAYE3())
  for (l <- 0 until byteLanes) {
    rlIddelays(l).CLK      := dramClk
    rlIddelays(l).RST      := rstSynced
    rlIddelays(l).CE       := True
    rlIddelays(l).EN_VTC   := False
    rlIddelays(l).LOAD     := False
    rlIddelays(l).INC      := False
    rlIddelays(l).DATAIN   := pads.dqs_in_t(l)   // DQS as it ARRIVES from the DRAM
    rlIddelays(l).IDATAIN  := False
    rlIddelays(l).CASC_IN  := False
    rlIddelays(l).CASC_RETURN := False
  }
  val rlSample = Vec.fill(byteLanes)(Bool())

  val rl = cd {
    // Seed mid-range (256 of 0..511) so the walk can correct either way.
    val rlDelay = Reg(UInt(9 bits)) init(256)
    val rlDone  = Reg(Bool()) init(False)
    val togPrev = Reg(Bool()) init(False)
    val togRise = !togPrev && dqsToggle
    val bbHist  = Vec.fill(16)(Reg(UInt(9 bits)) init(256))
    val bbConv  = Reg(UInt(4 bits)) init(0)

    togPrev := dqsToggle
    when(!rlActive) {                      // park at mid-range between sessions
      bbConv  := 0
      rlDelay := 256
      rlDone  := False
    }.elsewhen(!rlDone) {                  // active, not yet converged
      when(togRise) {
        for (i <- 15 downto 1) { bbHist(i) := bbHist(i-1) }
        bbHist(0) := rlDelay
        val drifted = (bbHist(0) > (bbHist(15) + 3)) || (bbHist(15) > (bbHist(0) + 3))
        when(drifted) { bbConv := 0 }
        .otherwise    { bbConv := bbConv + 1 }
        // Bang-bang (same polarity convention as the WL detector).
        when(rlSample(0)) { rlDelay := rlDelay - 1 }
        .otherwise        { rlDelay := rlDelay + 1 }
        when(bbConv > 7)  { rlDone  := True }
      }
    }                                      // rlActive && rlDone: hold
    new Bundle { val delay = rlDelay; val done = rlDone }
  }
  for (l <- 0 until byteLanes) {
    rlIddelays(l).CNTVALUEIN := rl.delay
    rlSample(l) := rlIddelays(l).DATAOUT
  }
  rdDelayOut := rl.delay

  if (cfg.useRdlvlReq) {
    dfi.rdTraining.rdlvlReq := 0   // Plan A: no training-read exchange needed
  }
  if (cfg.usePhyRdlvlCsN) {
    dfi.rdTraining.phyRdlvlCsN := 0
  }
  if (cfg.useRdlvlResp) {
    // EARNED by the convergence check (mirrors wrlvlResp); never constant.
    for (k <- 0 until dfi.rdTraining.rdlvlResp.getWidth) {
      dfi.rdTraining.rdlvlResp(k) := (if (k == 0) rl.done else False)
    }
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
  // Read data valid: tPhyRdlat DFI cycles after rden on the same phase.
  // Implemented as a pure delay of rden: the ISERDESE3 continuously
  // deserializes DQ, so rddataValid simply mirrors rden shifted by tPhyRdlat.
  // Duration matches rden duration (beatCount DFI cycles for BL8/fr=2).
  val tPhyRdlatVal = cfg.timeConfig.tPhyRdlat
  for (p <- 0 until frequencyRatio) {
    val rdenHist = History(dfi.read.rden(p), tPhyRdlatVal + 1)
    dfi.read.rd(p).rddataValid := rdenHist(tPhyRdlatVal)
    // DBI_n read-back (JESD79-4B §4.11): the DRAM drives DM during reads; the
    // MC decodes (DfiAlignment) only when useRdDbi (mr5 A12) — otherwise the
    // value is ignored, so wiring it unconditionally is safe.
    if (cfg.useRddataDbiN) {
      // Field is dbiWidth = dataRate*byteLanes bits (one DBI_n per UI per lane);
      // the flat model drives one DM value per lane, replicated across UIs.
      for (j <- 0 until (cfg.dataWidth / 8)) dfi.read.rd(p).rddataDbiN(j) := pads.dm_dbi_in(j % byteLanes)
    }
  }
}