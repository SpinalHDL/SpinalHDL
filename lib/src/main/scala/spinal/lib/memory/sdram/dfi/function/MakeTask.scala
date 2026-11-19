package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

case class Ddr4ModeRegs(
    mr0: Int = 0x130, // BL8 fixed(A1:A0=00), sequential(A3=0), CL=15 (A12,A6,A5,A4,A2=0,0,1,1,0), DLL reset (A8=1)
    mr1: Int = 0x000, // output enabled, no write leveling, push-pull, AL=0, DLL enable (A0=1 set by init? see note)
    mr2: Int = 0x000, // CWL=5, no AL
    mr3: Int = 0x000, // push-pull CA, normal drive strength
    mr4: Int = 0x000, // normal self-refresh temp range, ASR off
    mr5: Int = 0x000, // no CA parity, PPB off
    mr6: Int = 0x000 // RTT_NOM off, srf not enabled
)

/** One micro-program step of the generic init engine. The per-generation protocol lives in
  * the TABLE CONTENT (a preset), not in the engine: the engine knows nothing about pin
  * encodings — the consumer (MakeTask init override) maps kind onto the command bus, and
  * CmdTxd maps MRS/ZQ onto the generation's RAS/CAS/WE encoding.
  *   kind == MRS    : write mode register mrIndex with opCode
  *   kind == ZQ     : ZQCL calibration
  *   kind == PRE_ALL: precharge all banks
  *   kind == REF    : one refresh
  * waitCycles = how many cycles the engine waits AFTER issuing this step (before the next). */
case class SdramInitStep(kind: Int, mrIndex: Int, opCode: Int, waitCycles: Int)
object SdramInitStep {
  val MRS = 0
  val ZQ = 1
  val PRE_ALL = 2
  val REF = 3

  /** Built-in default init micro-program per generation — the SINGLE arbiter of "which
    * generation ships with which preset". Deliberately keyed on generation IDENTITY: the
    * preset payload (the per-gen MR layout) is generation-specific, so a capability flag
    * would be the WRONG key here (a future DDR5 has a modern command bus but none of these
    * MR tables). Adding a generation = one branch here, not a hunt through task builders.
    * DDR3/DDR2 presets are dual-source verified (see Ddr3InitSequencer/Ddr2InitSequencer);
    * SDR/MYDDR ship init-less (explicit DfiConfig.initSteps injection remains the escape
    * hatch for any generation). */
  def defaultFor(generation: SdramGeneration, modeRegs: Ddr4ModeRegs, fr: Int = 2): List[SdramInitStep] =
    // DDR4 spacings anchored to JESD79-4B nCK values (tMRD = 8 nCK, tZQinit = 1024 nCK),
    // converted to MC cycles by the frequency ratio so the preset is correct at every fr.
    generation match {
      case SdramGeneration.DDR4 => Ddr4InitSequencer(modeRegs = modeRegs, tMrdCycles = scala.math.ceil(8.0 / fr).toInt, zqInitCycles = scala.math.ceil(1024.0 / fr).toInt)
      case SdramGeneration.DDR3 => Ddr3InitSequencer()
      case SdramGeneration.DDR2 => Ddr2InitSequencer()
      case _                     => Nil
    }
}

case class SdramInitOp() extends Bundle {
  val kind = UInt(2 bits)
  val mrIndex = UInt(3 bits)
  val opCode = UInt(16 bits)
}

/** Generation-agnostic SDRAM power-on init engine: plays an ordered step table
  * (Seq[SdramInitStep]) as a micro-program — xprWaitCycles after reset, then one step per
  * timer expiry, each step's own waitCycles reloading the timer, done at table end. Any
  * init shape expressible as MRS/ZQ/PRE_ALL/REF steps works (DDR4 "MRS file then ZQCL",
  * DDR3 same, DDR2 PRE/EMRS/MRS/REF interleaved with no ZQ). */
case class SdramInitSequencer(
    steps: Seq[SdramInitStep],
    xprWaitCycles: Int
) extends Component {
  val io = new Bundle {
    val initOp = master(Flow(SdramInitOp()))
    val busy = out Bool ()
    val done = out Bool ()
  }
  // All table Vecs are 16-entry and the step index is 4-bit: SpinalHDL checks the index
  // width against log2Up(Vec size) at elaboration, so widths must line up exactly.
  val kinds = Vec.tabulate(16)(i => U(if (i < steps.length) steps(i).kind else 0, 2 bits))
  val mrIdxs = Vec.tabulate(16)(i => U(if (i < steps.length) steps(i).mrIndex else 0, 3 bits))
  val ops = Vec.tabulate(16)(i => U(if (i < steps.length) steps(i).opCode else 0, 16 bits))
  val waits = Vec.tabulate(16)(i => U(if (i < steps.length) steps(i).waitCycles else 0, 16 bits))
  val step = Reg(UInt(4 bits)) init (0)
  val timer = Reg(UInt(16 bits)) init (xprWaitCycles)
  val doneReg = RegInit(False)

  io.busy := !doneReg
  io.done := doneReg
  io.initOp.valid := False
  io.initOp.kind := 0
  io.initOp.mrIndex := 0
  io.initOp.opCode := 0

  when(!doneReg) {
    when(timer =/= 0) {
      timer := timer - 1
    } otherwise {
      when(step < steps.length) {
        io.initOp.valid := True
        io.initOp.kind := kinds(step)
        io.initOp.mrIndex := mrIdxs(step)
        io.initOp.opCode := ops(step)
        step := step + 1
        timer := waits(step)
      } otherwise {
        doneReg := True
      }
    }
  }
}

/** DDR4 preset step table (JESD79-4B §4.9.1 / Figure 6):
  * MRS MR3, MR6, MR5, MR4, MR2, MR1, MR0 (tMRD apart; MR0 last with DLL reset bit set) ->
  * ZQCL. The initial xprWait lives in the engine, not in this table. Bit-for-bit equivalent
  * to the pre-step-table engine output (verified by the DDR4 regression tests). */
object Ddr4InitSequencer {
  def apply(modeRegs: Ddr4ModeRegs, tMrdCycles: Int, zqInitCycles: Int): List[SdramInitStep] = {
    val ops = List(modeRegs.mr0, modeRegs.mr1, modeRegs.mr2, modeRegs.mr3, modeRegs.mr4, modeRegs.mr5, modeRegs.mr6)
    List(3, 6, 5, 4, 2, 1, 0).map(mr => SdramInitStep(SdramInitStep.MRS, mr, ops(mr), tMrdCycles)) :+
      SdramInitStep(SdramInitStep.ZQ, 0, 0, zqInitCycles)
  }
}

/** DDR3 preset step table (dual-source verified):
  *  - source A: litedram (enjoy-digital/litedram master, litedram/init.py,
  *    get_ddr3_phy_init_sequence, fetched 2026-11-01) — step ORDER and MR formulas;
  *  - source B: in-repo Micron ddr3.v model (tester/src/test/python/spinal/SdramXdr/
  *    Ddr3ModelTester/ddr3.v) — legacy command encodings (MRS/EMRS = RAS+CAS+WE, bank =
  *    MR/EMRS number, OP on address; ZQCL = RAS+CAS+WE_n + A10);
  *  - source C: DfiControllerTester "DfiController_DDR3_fr2_init" simulation (DfiMemoryAgent
  *    reference model) — end-to-end proof the engine + consumer deliver this table on the bus.
  * Order: MRS MR2 -> MR3 -> MR1 -> MR0 (DLL reset last) -> ZQCL. Fixed electrical defaults
  * per litedram (CL=CWL=5, BL8, ODT RON=34 Ohm / RTT_NOM=60 Ohm); board-specific overrides
  * go through DfiConfig.initSteps. waitCycles are simulation timeline knobs carried over from
  * the validated tester preset, NOT protocol timings (real tMRD/tZQLA are enforced by the
  * scheduler once traffic starts). */
object Ddr3InitSequencer {
  def apply(tMrdCycles: Int = 8, zqCalCycles: Int = 512): List[SdramInitStep] = List(
    SdramInitStep(SdramInitStep.MRS, 2, 0x200, tMrdCycles), // CWL=5 (A3:0), RTT_WR=60 Ohm (A9)
    SdramInitStep(SdramInitStep.MRS, 3, 0x000, tMrdCycles), // DLL enabled
    SdramInitStep(SdramInitStep.MRS, 1, 0x006, tMrdCycles), // RON=34 Ohm (A1), RTT_NOM=60 Ohm (A2)
    SdramInitStep(SdramInitStep.MRS, 0, 0x310, tMrdCycles), // BL8 (A1:0=00), CL5 (A4), WR5 (A9), DLL reset (A8)
    SdramInitStep(SdramInitStep.ZQ, 0, 0, zqCalCycles)
  )
}

/** DDR2 preset step table (dual-source verified):
  *  - source A: litedram master litedram/init.py get_ddr2_phy_init_sequence — step ORDER and
  *    MR formula mr = log2(bl) + (cl<<4) + ((wr-1)<<9), reset_dll = 1<<8, ocd = 7<<7;
  *  - source B: in-repo Micron ddr2.v model (tester/src/test/python/spinal/SdramXdr/
  *    Ddr2ModelTester/ddr2.v) — legacy command encodings;
  *  - source C: DfiControllerTester "DfiController_DDR2_fr2_init" simulation.
  * Order: PRE_ALL -> EMRS3 -> EMRS2 -> EMRS1 -> MRS (DLL reset) -> PRE_ALL -> REF x2 ->
  * MRS -> EMRS1 (OCD default) -> EMRS1 (OCD exit). DDR2 HAS NO ZQCL STEP. Fixed defaults:
  * BL4, CL5, WR=5 (conservative; litedram derives WR from timing via get_write_recovery +
  * round_up_to_mr_value). DLL-reset step waits 256 (covers ~200 tDLL lock cycles). mrIndex
  * 1/2/3 = EMRS1/2/3 (the consumer maps bank = MR/EMRS number). */
object Ddr2InitSequencer {
  def apply(tMrdCycles: Int = 8, dllLockCycles: Int = 256): List[SdramInitStep] = {
    val mr0 = 0x452 // BL4 (A1), CL5 (A7:A4), WR5 ((5-1)<<9)
    List(
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, tMrdCycles),
      SdramInitStep(SdramInitStep.MRS, 3, 0x000, tMrdCycles), // EMRS3
      SdramInitStep(SdramInitStep.MRS, 2, 0x000, tMrdCycles), // EMRS2
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, tMrdCycles), // EMRS1
      SdramInitStep(SdramInitStep.MRS, 0, mr0 | 0x100, dllLockCycles), // MRS MR0 + DLL reset (A8)
      SdramInitStep(SdramInitStep.PRE_ALL, 0, 0, tMrdCycles),
      SdramInitStep(SdramInitStep.REF, 0, 0, tMrdCycles),
      SdramInitStep(SdramInitStep.REF, 0, 0, tMrdCycles),
      SdramInitStep(SdramInitStep.MRS, 0, mr0, tMrdCycles),
      SdramInitStep(SdramInitStep.MRS, 1, 0x380, tMrdCycles), // EMRS1 OCD default (7<<7)
      SdramInitStep(SdramInitStep.MRS, 1, 0x000, tMrdCycles) // EMRS1 OCD exit
    )
  }
}

case class MakeTask(taskConfig: TaskConfig, dfiConfig: DfiConfig, addrMap: AddrMap) extends Component {
  val io = new Bundle {
    val cmd = slave(Stream(TaskWrRdCmd(taskConfig, dfiConfig)))
    val halt = out Bool ()
    val writeDataToken = slave(Stream(Event))
    val output = master(OpTasks(taskConfig, dfiConfig))
    // DFI ctrlmsg training / legacy write-leveling: while a training handshake is in flight,
    // task spawning and refresh are paused globally (same gating pattern as initDone).
    // Absent when both channels are disabled — every gate below then folds to its
    // pre-training expression.
    val trainBusy =
      (dfiConfig.signalConfig.useCtrlMsg || dfiConfig.signalConfig.useLegacyWrLvl) generate in(Bool())
    // initDone exported through the io boundary only for the boot-training orchestrator
    // (BmbBridge forwards it to DfiController's BootTrainer). Reading the init engine's io
    // from the GRANDPARENT would be a hierarchy violation; every other config has no port.
    val initDoneOut = (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) generate out(Bool())
  }
  val timeConfig = TaskTimingConfig(dfiConfig)
  def trainBlocked: Bool =
    if (dfiConfig.signalConfig.useCtrlMsg || dfiConfig.signalConfig.useLegacyWrLvl) io.trainBusy else False
  val sdram = dfiConfig.sdram

  // Init hook: config-carried step table wins (DfiConfig.initSteps, e.g. the DDR3/DDR2
  // presets in the tester); otherwise the generation's built-in preset, arbitrated in one
  // place (SdramInitStep.defaultFor). Generations without a preset run init-less exactly as
  // before (no engine component is instantiated, initDone == True).
  val initSeq: List[SdramInitStep] =
    dfiConfig.initSteps.getOrElse(SdramInitStep.defaultFor(sdram.generation, dfiConfig.ddr4ModeRegs, fr = dfiConfig.frequencyRatio))
  // xprWaitCycles: the tXPR pause before the first MRS. On real silicon tXPR = Max(tXS, 5 nCK)
  // ~= tXS (200 us after RESET_n de-assertion, JESD79-4B sec. 3.3 step 5) and is absorbed by the
  // board's power-up sequencing (SdramConfig.ddrStartDelay = 600 us). Here it is a SIMULATION
  // pacing value (32 MC cycles) that merely lets reset/CKE settle before the first MRS — it is
  // NOT a protocol timing and must not be read as an assertion on tXPR.
  val initEngine = if (initSeq.nonEmpty) SdramInitSequencer(initSeq, xprWaitCycles = 32) else null
  def initDone: Bool = if (initEngine != null) initEngine.io.done else True
  if (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) io.initDoneOut := initDone
  val banksRow = Mem(UInt(dfiConfig.sdram.rowWidth bits), sdram.bankCount)
  val CCD =
    (dfiConfig.beatCount > 1) generate timing(
      io.output.read || io.output.write,
      dfiConfig.beatCount - 2,
      log2Up(dfiConfig.beatCount)
    )
  val RFC = timing(io.output.refresh, timeConfig.RFC, taskConfig.taskParameter.timingWidth + 3)
  // Bank-group-aware ACT-to-ACT spacing. Capability gate (bgWidth>0), not a generation
  // check: L/S split only means something when bank groups exist. bgWidth==0 (all DDR1/2/3
  // configs today) keeps a single global RRD timer, bit-identical to dev.
  // With bank groups: both timers reload on EVERY ACT. The candidate's relation to the last ACT's bank
  // group selects which gates it: same group -> tRRD_L, different group -> tRRD_S.
  // (tRRD_L/S are both measured from the previous ACT, so both timers must be live after it.)
  val RRD_busy: Bool = if (dfiConfig.sdram.bgWidth > 0) {
    new Area {
      val lastBg = Reg(Bits(dfiConfig.bankGroupWidth bits)) init (B(0, dfiConfig.bankGroupWidth bits))
      val lastBgValid = RegInit(False)
      when(io.output.active) {
        lastBg := io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
        lastBgValid := True
      }
      val sameGroup = lastBg === io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
      val timerL = timing(io.output.active, timeConfig.RRD)
      val timerS = timing(io.output.active, timeConfig.RRD_S)
      val out = lastBgValid && Mux(sameGroup, timerL.busy, timerS.busy)
    }.out
  } else {
    timing(io.output.active, timeConfig.RRD).busy
  }
  // Bank-group-aware WRITE-to-READ turnaround (capability gate bgWidth>0, same rationale
  // as RRD_busy). Legacy (bgWidth==0): one global WTR timer (unchanged). With bank groups:
  // both timers reload on every write;
  // a read in the same bank group as the last write waits tWTR (L), a different group tWTR_S.
  val WTR_busy: Bool = if (dfiConfig.sdram.bgWidth > 0) {
    new Area {
      val lastBg = Reg(Bits(dfiConfig.bankGroupWidth bits)) init (B(0, dfiConfig.bankGroupWidth bits))
      val lastBgValid = RegInit(False)
      when(io.output.write) {
        lastBg := io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
        lastBgValid := True
      }
      val sameGroup = lastBg === io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
      val timerL = timing(io.output.write, timeConfig.WTR)
      val timerS = timing(io.output.write, timeConfig.WTR_S)
      val out = lastBgValid && Mux(sameGroup, timerL.busy, timerS.busy)
    }.out
  } else {
    timing(io.output.write, timeConfig.WTR).busy
  }
  // Bank-group-aware CAS-to-CAS (tCCD) between column commands (capability gate bgWidth>0).
  // Legacy (bgWidth==0): no extra gate (the station's 1-cycle block already provides
  // tCCD_S-equivalent spacing). With bank groups: a column command into the SAME bank group as the previous column command must wait
  // tCCD_L (4nCK = 2 MC cycles at fr2); cross-group keeps the existing 1-cycle spacing (tCCD_S).
  val CCD_BG = if (dfiConfig.sdram.bgWidth > 0) new Area {
    val lastBg = Reg(Bits(dfiConfig.bankGroupWidth bits)) init (B(0, dfiConfig.bankGroupWidth bits))
    val lastValid = RegInit(False)
    when(io.output.read || io.output.write) {
      lastBg := io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
      lastValid := True
    }
    val sameGroup = lastBg === io.output.address.bank(dfiConfig.dfiBankWidth, dfiConfig.bankGroupWidth bits).asBits
    val timerL = timing(io.output.read || io.output.write, timeConfig.CCD_L)
    // tCCD_S: consecutive column commands into DIFFERENT bank groups need their own spacing
    // (JESD79-4B sec. 4.19, 8 nCK at 8 Gb). Only tCCD_L was gated; cross-group traffic fell back
    // to the station's one-cycle block, i.e. 2 nCK at fr2 and 1 nCK at fr1.
    val timerS = timing(io.output.read || io.output.write, timeConfig.CCD_S)
    val busy = lastValid && (if (timeConfig.CCD_S >= timeConfig.CCD_L) timerS.busy
                             else Mux(sameGroup, timerL.busy, timerS.busy))
  } else null
  def ccdBgBusy: Bool = if (CCD_BG != null) CCD_BG.busy else False
  val RTW = timing(io.output.read, timeConfig.RTW)
  val RP = timing(io.output.prechargeAll, timeConfig.RP + 1)
  // --- DFI 5.2 multi-chip-select data-burst gap (tphy_wrcsgap / tphy_rdcsgap, Tables 16/19) ---
  // When two consecutive same-direction column bursts target DIFFERENT chip-selects, the PHY
  // needs an EXTRA tphy_wrcsgap/tphy_rdcsgap (data clocks) between the two data windows, on top
  // of the back-to-back window length. The base window spacing is already enforced by the CCD
  // timer; this adds ONLY the extra gap, active solely on a CS switch, so single-CS and gap==0
  // configs elaborate bit-identically to before (zero regression). Command interval ==
  // data-window interval (each window trails its command by the fixed tphy_wrdata/tphy_rdlat
  // offset), so spacing the COMMANDS transitively satisfies the dfi_wrdata_cs / dfi_rddata_cs
  // window gap the spec mandates.
  val wrCsGap: Bool = if (dfiConfig.chipSelectNumber >= 2 && timeConfig.tPhyWrCsGapMc > 0) {
    new Area {
      val csWidth = log2Up(dfiConfig.chipSelectNumber)
      val lastCs = Reg(UInt(csWidth bits)) init (U(0, csWidth bits))
      val lastValid = RegInit(False)
      // Counts the interval since the last issued write command (reload on every write).
      val timer = timing(io.output.write, dfiConfig.beatCount + timeConfig.tPhyWrCsGapMc)
      val switching = lastValid && (io.output.address.cs =/= lastCs)
      when(io.output.write) { lastCs := io.output.address.cs; lastValid := True }
      val busy = switching && timer.busy
    }.busy
  } else False
  val rdCsGap: Bool = if (dfiConfig.chipSelectNumber >= 2 && timeConfig.tPhyRdCsGapMc > 0) {
    new Area {
      val csWidth = log2Up(dfiConfig.chipSelectNumber)
      val lastCs = Reg(UInt(csWidth bits)) init (U(0, csWidth bits))
      val lastValid = RegInit(False)
      val timer = timing(io.output.read, dfiConfig.beatCount + timeConfig.tPhyRdCsGapMc)
      val switching = lastValid && (io.output.address.cs =/= lastCs)
      when(io.output.read) { lastCs := io.output.address.cs; lastValid := True }
      val busy = switching && timer.busy
    }.busy
  } else False
  val FAW = sdram.generation.FAW generate new Area { // Can be optimized
    val trigger = io.output.active
    val ptr = RegInit(U"00")
    val slots = (0 to 3).map(i => timing(ptr === i && trigger, timeConfig.FAW))
    val busyNext = Vec(slots.map(_.busy)).read(ptr + 1)
    ptr := ptr + U(trigger)
  }
  val banks = for (bankId <- 0 until sdram.bankCount) yield new Area {
    val hits = io.output.address.bank === bankId
    def gate(task: Bool) = hits & task

    val activeNext = Bool()
    val active = RegNext(activeNext) init (False)
    activeNext := active
    when(gate(io.output.precharge) || io.output.prechargeAll) {
      activeNext := False
    } otherwise {
      when(gate(io.output.active)) {
        activeNext := True
      }
    }

    val WR = timing(gate(io.output.write), timeConfig.WR)
    val RAS = timing(gate(io.output.active), timeConfig.RAS)
    val RP = timing(gate(io.output.precharge), timeConfig.RP)
    val RCD = timing(gate(io.output.active), timeConfig.RCD)
    val RTP = timing(gate(io.output.read), timeConfig.RTP)

    val allowPrecharge = !WR.busy && !RAS.busy && !RTP.busy
    val allowActive = !RP.busy
    val allowWrite = !RCD.busy
    val allowRead = !RCD.busy
  }
  val allowPrechargeAll = banks.map(_.allowPrecharge).andR
  val taskConstructor = new Area {
    val input = io.cmd.stage()
    val address = BusAddress(dfiConfig)
    val addrMapping = new Area {
      val rbcAddress = address.getRBCAddress(input.address)
      val addrMapMethod = AddrMapMethod(dfiConfig, addrMap)
      val taskAddress = addrMapMethod.addressMap(rbcAddress)
      address.byte.assignFromBits(address.getByteAddress(input.address).asBits)
      address.cs.assignFromBits(address.getCsAddress(input.address).asBits)
      address.assignUnassignedByName(taskAddress.address)
    }
    val status = Status()
    status.allowPrecharge := True
    status.allowActive := !RRD_busy && (if (sdram.generation.FAW) !FAW.busyNext else True)
    status.allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !wrCsGap
    status.allowRead := !WTR_busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !rdCsGap
    status.bankHit := banksRow.readAsync(address.bank) === address.row
    status.bankActive := banks.map(_.active).read(address.bank)
    status.employ(address)
  }
  val columnBurstShift = log2Up(dfiConfig.transferPerBurst)
  // Defaults so every generation drives the DDR4-only op flags; the init override below
  // (init-aware configs only) takes precedence during the power-up sequence. initCsN idles
  // all-ones (no rank selected by the broadcast path) outside the init window.
  io.output.mrs := False
  io.output.zq := False
  io.output.initCsN.setAll()

  val columnBurstMask = (sdram.columnSize - 1) - ((io.cmd.stationLengthMax - 1) << columnBurstShift)
  val station = new Area {
    val valid = RegInit(False)
    val status = Reg(Status())
    val address = Reg(BusAddress(dfiConfig))
    val write = Reg(Bool())
    val context = Reg(Bits(taskConfig.contextWidth bits))
    val offset, offsetLast = Reg(UInt(io.cmd.stationLengthWidth + 1 bits))

    import status._
    allowPrecharge := True
    allowActive := !RRD_busy && (if (sdram.generation.FAW) !FAW.busyNext else True)
    allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !wrCsGap
    allowRead := !WTR_busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !rdCsGap
    bankHit.init(False)
    bankActive.init(False)
    status.employ(address)
    when(io.output.active) {
      status.allowActive := False
    }
    if (CCD != null) when(io.output.read || io.output.write) {
      status.allowRead := False
      status.allowWrite := False
    }
    else {
      when(io.output.read) {
        status.allowWrite := False
      }
      when(io.output.write) {
        status.allowRead := False
      }
    }

    val inputActive = !bankActive
    val inputPrecharge = bankActive && !bankHit
    // Tail auto-precharge: after the last data beat, close the row this task left open.
    // Without it the row stays open until the next task (or the idle tail, 20k+ MC), and
    // the ACT->PRE gap blows the watchdog's tRAS-max ceiling (9*tREFI). Explicit PRE, not
    // DFI WRA/RDA: AUTO_PRECHARGE_BIT is cleared in Task2IDFI, and the explicit PRE reuses
    // the per-bank PRE path (tRP gate included). Gated on the bank's allowPrecharge so the
    // tRAS-min/WR/RTP tails run out first — the station simply lingers (the timers
    // self-expire, no deadlock); if the row is already closed (defensive) the tail ends
    // without a PRE.
    val inTail = offset > offsetLast
    val bankAllowPrecharge = banks.map(_.allowPrecharge).read(address.bank)
    val inputPrechargeTail = inTail && bankActive && bankAllowPrecharge
    val inputAccess = bankActive && bankHit && !inTail
    val inputWrite = bankActive && bankHit && !inTail && write
    val inputRead = bankActive && bankHit && !inTail && !write

    // The write issue gate is evaluated COMBINATORIALLY. The `status` Reg above latches the
    // same expression one cycle early, and the global timers (CCD, CCD_BG, RTW, cs gaps) load
    // on the very cycle their command is issued — so the stale registered copy sees the
    // pre-load (expired) timer state and lets the immediately following cycle through every
    // margin: effective spacing collapsed to 1 MC cycle, which violates tCCD_L (caught by
    // DfiTimingWatchdog on DDR4 fr4 back-to-back same-group WRs).
    // Issue discipline: the registered `status` copy above used to enforce "no two
    // read/write commands within 1 MC cycle" by force-clearing status.allowRead/allowWrite
    // on any issue (the `if (CCD != null) when(...)` block above). Evaluating the gates
    // combinationally (required for tCCD_L/tWTR_S conformance) drops that registered
    // exclusion, so restate it explicitly: after any READ/WRITE issue, the next READ/WRITE
    // waits one MC cycle. This is also a HARD interface constraint, not just pacing:
    // RdDataRxd opens a beatCount-wide sliding rden window per read task (Task2IDFI
    // io.rden := History(...).orR); read tasks closer than beatCount MC cycles merge their
    // windows, and the memory side schedules fewer rddata slots than the read needs,
    // losing trailing beats (DfiController_DDR3_fr2_legacy_wrlvl: 16 back-to-back RDs
    // drained only 20/32 rsp beats). For beatCount=2 the required spacing is exactly 2;
    // the CCD timer (loadValue = beatCount-2) is vacuous there, so this exclusion is the
    // spacing. The register breaks the read->gate->read path, so no combinational loop.
    val rwIssuedPrev = RegInit(False)
    rwIssuedPrev := io.output.read | io.output.write

    val allowWriteNow =
      !rwIssuedPrev && !RTW.busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !wrCsGap &&
        banks.map(_.allowWrite).read(address.bank)

    val doActive = inputActive && allowActive
    val doPrecharge = inputPrecharge && allowPrecharge
    val doWrite = inputWrite && allowWriteNow && io.writeDataToken.valid
    val allowReadNow =
      !rwIssuedPrev && !WTR_busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy && !rdCsGap &&
        banks.map(_.allowRead).read(address.bank)
    val doRead = inputRead && allowReadNow
    val doAccess = doWrite || doRead
    val doSomething = valid && (doActive || doPrecharge || doWrite || doRead)

    val blockedByWriteToken = inputWrite && allowWriteNow && !io.writeDataToken.valid // For debug visualisation

    val last = offset === offsetLast
    io.output.address.column := address.column | (offset << columnBurstShift).resized
    io.output.address.assignUnassignedByName(address)
    io.output.context := context
    // Gate ACT/PRE on the station being armed, exactly like write/read below. Ungated, the
    // uninitialized bank-state registers (X at t=0) make inputActive/inputPrecharge X before
    // the first command, and Verilog's if(X)=true turns that into a phantom ACT<->PRE storm
    // on the DFI bus (latent in SpinalSim, whose when(X) does not fire; visible in xsim and
    // as a power-on phantom command on real silicon, where FFs power up 0, not X).
    // tRP: a PRE on this bank loads its per-bank RP timer; the next ACT on the bank must wait
    // for it to expire. The per-bank allowActive (!RP.busy) only fed doActive (bookkeeping) —
    // the output bypassed it, so a row-switch PRE was followed by its ACT 1 MC later (tRP
    // floor 2). Gate the ACT output on the target bank's allowActive: combinational from the
    // RP counter (self-expiring -> no deadlock), X-free at t=0 (RP.value is randBoot and
    // valid is RegInit(False)), and self-consistent because all bank-state bookkeeping keys
    // off io.output.active itself.
    io.output.active := inputActive & valid & banks.map(_.allowActive).read(address.bank)
    io.output.precharge := (inputPrecharge || inputPrechargeTail) & valid
    io.output.write := doWrite & valid
    io.output.read := doRead & valid
    io.output.last := last

    io.writeDataToken.ready.clear()
    io.writeDataToken.ready.setWhen(io.output.write)

    when(doAccess & valid) {
      offset := offset + 1
    }
    when(inTail & valid) {
      // Row-close tail: the PRE fires iff the row is open and the bank's allowPrecharge has
      // expired (inputPrechargeTail); drop the station either way — precharge done this
      // cycle, or moot because the row was already closed.
      when(!bankActive || bankAllowPrecharge) {
        valid := False
      }
    }
  }

  val refreshStream = Event
  val refresher = Refresher(taskConfig, dfiConfig)
  refresher.io.refresh.valid <> io.halt
  refresher.io.refresh <> refreshStream

  val selectedAddress = io.output.address
  // True from the entry-decision cycle until the refresh FSM is back in idle (RFC tail done).
  // Holds the taskConstructor pipe payload (ready=0) for the whole sequence: the halt freezes
  // the upstream BMB, but a payload already parked in the m2sPipe at freeze time would
  // otherwise spawn a station burst one cycle after entry — a straddling burst whose
  // bank-state snapshot pre-dates the PRE_ALL keeps driving beats on the row PRE_ALL just
  // closed, and the next task on that bank re-ACTs tRFC-tight after the REF (measured: ACT
  // 22 MC after REF, tRFC floor 26). Holding the spawn until the FSM is idle makes the
  // deferred burst take a FRESH snapshot (row closed -> ACT first) with the ACT
  // structurally >= tRFC after the REF. Entry itself stays burst-boundary (askRefresh):
  // the in-flight burst is done before entry, so nothing is torn.
  val refreshSeqActive = Bool()
  val loader = new Area {
    taskConstructor.input.ready := !station.valid && initDone && !trainBlocked && !refreshSeqActive
    val offset = taskConstructor.address.column(columnBurstShift, io.cmd.stationLengthWidth bits)
    val offsetLast = offset +^ taskConstructor.input.length
    // BmbAlignedSpliter guarantees sub-commands never cross a bytePerTaskMax-aligned boundary, so
    // offset + length fits the station range. Make that invariant explicit: with the +1-bit wide
    // offsetLast, a violation is detected before any truncation can wrap the comparison.
    // Gated on input.valid: the m2sPipe stage latches garbage payload while valid is low
    // (collapsible bubble), and this combinatorial assertion would otherwise trip on those cycles.
    when(taskConstructor.input.valid) {
      assert(
        offsetLast <= io.cmd.stationLengthMax - 1,
        "SDRAM task bursts cross the station boundary: offset + length must fit in stationLengthWidth bits"
      )
    }
    val canSpawn = !station.valid && initDone && !trainBlocked && !refreshSeqActive
    // Insert taskConstructor into one free station
    when(taskConstructor.input.fire && canSpawn) {
      station.valid := True
      station.status := taskConstructor.status
      station.address.column := taskConstructor.address.column & columnBurstMask
      station.address.assignUnassignedByName(taskConstructor.address)
      station.write := taskConstructor.input.write
      station.context := taskConstructor.input.context
      station.offset := offset.resized
      station.offsetLast := offsetLast
    }
  }
  // Burst-boundary acceptance with halt-engaged entry. The upstream halt (refresher pending ->
  // io.halt -> BmbBridge RegNext -> BMB haltWhen) FREEZES the taskConstructor pipe 2 cycles after
  // pending rises. Gating the entry on that freeze being active (RegNext(RegNext(refreshStream.valid)))
  // guarantees no command can spawn a station burst mid-sequence — closing the 1-cycle hole whose
  // torn burst forced tRFC-violating re-ACTs. Crucially this gates on the pipe being FROZEN, not
  // EMPTY: the station is only empty ~1 clk between back-to-back bursts, whereas the pipe is empty
  // only on rare master pauses; a pipe-EMPTY entry term therefore waited ~60 MC for a pause and
  // slipped the whole tREFI window. Frozen-but-nonempty is fine: the held command cannot fire.
  // The entry lands on the next station-empty moment (in-flight burst done). Never the
  // combinatorial "no traffic this cycle" flag: gating the sequence start on it stalls the
  // sequence mid-flight whenever a beat arrives and slips the whole tREFI window.
  val haltEngaged = RegNext(RegNext(refreshStream.valid))
  // Require BOTH the live request and the engaged halt. haltEngaged lags the release by 2
  // cycles: on a level-gate on haltEngaged alone the idle FSM would see askRefresh still high
  // right after refreshStream.ready and RELAUNCH a spurious PRE_ALL/REF sequence. That spurious
  // REF can collide with a freshly spawned burst's ACT on the same cycle, win the command
  // arbitration and silently drop the ACT while the station's bank tracking still books the row
  // open -> subsequent beats hit a closed row -> data corruption. Requiring refreshStream.valid
  // (dropped by the ready pulse) closes the relaunch window; the haltEngaged term still enforces
  // the frozen-pipe entry guarantee.
  val askRefresh = refreshStream.valid && haltEngaged && !station.valid && initDone && !trainBlocked
  when(station.doSomething) {
    banksRow.write(selectedAddress.bank, selectedAddress.row)
  }
  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val prechargeAllCmd = new State
    val refreshCmd = new State
    val refreshReady = new State
    // Entry: burst-boundary acceptance (askRefresh): the sequence only starts when the
    // station holds no burst. Mid-sequence: TIMER-ONLY gates — the sequence must advance on
    // its own schedule, or traffic slipping in mid-flight slips the whole tREFI window
    // (measured: 3735 MC REF->REF gaps when these gates carried the combinatorial
    // "no traffic this cycle" flag). Nothing new can enter the station meanwhile: the
    // refresher's pending flag halts the upstream BMB cmd path for the whole sequence
    // (BmbBridge: RegNext(maketask.io.halt) <> bmbAdapter.io.halt -> haltWhen), so no ACT
    // can land inside the PRE_ALL->REF window and no burst is ever torn.
    idle.whenIsActive(when(askRefresh) { goto(prechargeAllCmd) })
    prechargeAllCmd.whenIsActive(when(RegNext(allowPrechargeAll)) { goto(refreshCmd) })
    refreshCmd.whenIsActive(when(!RP.busy) { goto(refreshReady) })
    refreshReady.whenIsActive(when(!RFC.busy) { goto(idle) })

    prechargeAllCmd.onExit(io.output.prechargeAll := True)
    refreshCmd.onExit(io.output.refresh := True)
    refreshReady.onExit(refreshStream.ready := True)
  }

  refreshStream.ready := False
  io.output.prechargeAll := False
  io.output.refresh := False
  // askRefresh covers the entry-decision cycle (the FSM only leaves idle one cycle later);
  // !fsm.isActive(fsm.idle) covers the rest of the sequence. Together they hold the pipe
  // payload (see refreshSeqActive above) from entry until the RFC tail has elapsed.
  refreshSeqActive := askRefresh || !fsm.isActive(fsm.idle)

  // Init override (step-table engine). While init runs, the normal command path is held off:
  // the station cannot spawn, the refresh FSM is gated, and active/precharge are forced low
  // here so no ACT/PRE leaks onto the DFI bus before init completes (the agent asserts on
  // that). Each step kind maps onto one op flag; CmdTxd does the generation-specific pin
  // encoding. This block sits AFTER the prechargeAll/refresh False defaults above — later
  // textual assignments win, so only here can init REF/PRE_ALL steps override them. The FSM's
  // own onExit assignments are emitted afterElaboration and would beat this block, but they
  // can never fire during init (askRefresh is gated on initDone), so there is no conflict.
  if (initEngine != null) {
    val op = initEngine.io.initOp
    when(!initEngine.io.done) {
      io.output.active := False
      io.output.precharge := False
      io.output.last := True
      // Init broadcast mask (elaboration-time constant, no runtime latch): every rank selected
      // for the whole init window. DRAM chip-select is a low-active broadcast bus and init
      // commands are rank-agnostic, so MRS/ZQCL/PRE_ALL/REF fan out to ALL ranks in the same
      // cycle — the step table still plays exactly once (init duration unchanged), and which
      // rank gets the program no longer depends on the unreset station cs latch (which used to
      // flip per build: the cs-latch lottery). CmdTxd REPLACES its runtime cs decode with this
      // mask for the window's duration; io.output.address.cs keeps its continuous assign.
      io.output.initCsN.clearAll()
      when(op.valid) {
        switch(op.kind) {
          is(SdramInitStep.MRS) {
            io.output.mrs := True
            // MR select on the bank field, OP code on the row field. DDR4: bank = {BG0=MRSEL2,
            // BA1:0=MRSEL1:0} (flat index); legacy DDR3/DDR2: bank = BA2:0 = MR/EMRS number
            // (EMRS = BA1/2/3 set by mrIndex). Truncation of opCode is safe for the supported
            // geometries: all defined OP bits of the presets fit in 13 address bits.
            io.output.address.bank := op.mrIndex.resized
            io.output.address.row := op.opCode.resized
          }
          is(SdramInitStep.ZQ) { io.output.zq := True } // CmdTxd sets A10=H (ZQCL)
          is(SdramInitStep.PRE_ALL) { io.output.prechargeAll := True }
          is(SdramInitStep.REF) { io.output.refresh := True }
        }
      }
    }
  }

  def timing(loadValid: Bool, loadValue: UInt, timingWidth: Int = taskConfig.taskParameter.timingWidth) = new Area {
    val value = Reg(UInt(timingWidth bits)) randBoot ()
    val increment = value =/= loadValue.resized
    val busy = CombInit(increment)
    value := value + increment.asUInt.resized
    when(loadValid) {
      value := 0
    }
  }

  case class Status() extends Bundle {
    val bankActive = Bool()
    val bankHit = Bool()
    val allowPrecharge = Bool()
    val allowActive = Bool()
    val allowWrite = Bool()
    val allowRead = Bool()

    def employ(address: BusAddress): Unit = {
      allowPrecharge clearWhen (!banks.map(_.allowPrecharge).read(address.bank))
      allowActive clearWhen (!banks.map(_.allowActive).read(address.bank))
      allowWrite clearWhen (!banks.map(_.allowWrite).read(address.bank))
      allowRead clearWhen (!banks.map(_.allowRead).read(address.bank))

      when(io.output.address.bank === address.bank) {
        when(io.output.precharge) {
          bankActive := False
        }
        when(io.output.active) {
          bankActive := True
          bankHit := io.output.address.row === address.row
          allowRead := False
          allowWrite := False
          allowPrecharge := False
        }
        when(io.output.read || io.output.write) {
          allowPrecharge := False
        }
        when(io.output.precharge) {
          allowActive := False
        }
      }
    }
  }
}
