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
  val readyForRefresh = True
  val sdram = dfiConfig.sdram

  // Init hook: init micro-program = config-carried step table (DfiConfig.initSteps, e.g. the
  // DDR3/DDR2 presets in the tester) if present; otherwise DDR4 falls back to its named
  // preset (dev behavior, unchanged), every other generation runs init-less exactly as on
  // dev (old tests bit-identical: no engine component is instantiated, initDone == True).
  val initSeq: List[SdramInitStep] = dfiConfig.initSteps.getOrElse(
    if (sdram.generation == SdramGeneration.DDR4)
      Ddr4InitSequencer(modeRegs = dfiConfig.ddr4ModeRegs, tMrdCycles = 4, zqInitCycles = 512)
    else Nil
  )
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
    val busy = lastValid && sameGroup && timerL.busy
  } else null
  def ccdBgBusy: Bool = if (CCD_BG != null) CCD_BG.busy else False
  val RTW = timing(io.output.read, timeConfig.RTW)
  val RP = timing(io.output.prechargeAll, timeConfig.RP + 1)
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
    status.allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy
    status.allowRead := !WTR_busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy
    status.bankHit := banksRow.readAsync(address.bank) === address.row
    status.bankActive := banks.map(_.active).read(address.bank)
    status.employ(address)
    readyForRefresh clearWhen (input.valid)
  }
  val columnBurstShift = log2Up(dfiConfig.transferPerBurst)
  // Defaults so every generation drives the DDR4-only op flags; the init override below
  // (init-aware configs only) takes precedence during the power-up sequence. initCsN idles
  // all-ones (no rank selected by the broadcast path) outside the init window.
  io.output.mrs := False
  io.output.zq := False
  io.output.initCsN.setAll()

  readyForRefresh clearWhen (io.cmd.valid)
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
    allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy
    allowRead := !WTR_busy && (if (CCD != null) !CCD.busy else True) && !ccdBgBusy
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
    val inputAccess = bankActive && bankHit
    val inputWrite = bankActive && bankHit && write
    val inputRead = bankActive && bankHit && !write

    val doActive = inputActive && allowActive
    val doPrecharge = inputPrecharge && allowPrecharge
    val doWrite = inputWrite && allowWrite && io.writeDataToken.valid
    val doRead = inputRead && allowRead
    val doAccess = doWrite || doRead
    val doSomething = valid && (doActive || doPrecharge || doWrite || doRead)

    val blockedByWriteToken = inputWrite && allowWrite && !io.writeDataToken.valid // For debug visualisation

    val last = offset === offsetLast
    io.output.address.column := address.column | (offset << columnBurstShift).resized
    io.output.address.assignUnassignedByName(address)
    io.output.context := context
    // Gate ACT/PRE on the station being armed, exactly like write/read below. Ungated, the
    // uninitialized bank-state registers (X at t=0) make inputActive/inputPrecharge X before
    // the first command, and Verilog's if(X)=true turns that into a phantom ACT<->PRE storm
    // on the DFI bus (latent in SpinalSim, whose when(X) does not fire; visible in xsim and
    // as a power-on phantom command on real silicon, where FFs power up 0, not X).
    io.output.active := inputActive & valid
    io.output.precharge := inputPrecharge & valid
    io.output.write := doWrite & valid
    io.output.read := doRead & valid
    io.output.last := last

    io.writeDataToken.ready.clear()
    io.writeDataToken.ready.setWhen(io.output.write)

    when(doAccess & valid) {
      offset := offset + 1
      when(last) {
        valid := False
      }
    }
    readyForRefresh clearWhen (valid)
  }

  val refreshStream = Event
  val refresher = Refresher(taskConfig, dfiConfig)
  refresher.io.refresh.valid <> io.halt
  refresher.io.refresh <> refreshStream

  val selectedAddress = io.output.address
  val loader = new Area {
    taskConstructor.input.ready := !station.valid && initDone && !trainBlocked
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
    val canSpawn = !station.valid && initDone && !trainBlocked
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
  val askRefresh = refreshStream.valid && readyForRefresh && initDone && !trainBlocked
  when(station.doSomething) {
    banksRow.write(selectedAddress.bank, selectedAddress.row)
  }
  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val prechargeAllCmd = new State
    val refreshCmd = new State
    val refreshReady = new State
    idle.whenIsActive(when(askRefresh) { goto(prechargeAllCmd) })
    prechargeAllCmd.whenIsActive(when(RegNext(allowPrechargeAll) & askRefresh) { goto(refreshCmd) })
    refreshCmd.whenIsActive(when(!RP.busy & askRefresh) { goto(refreshReady) })
    refreshReady.whenIsActive(when(!RFC.busy & askRefresh) { goto(idle) })

    prechargeAllCmd.onExit(io.output.prechargeAll := True)
    refreshCmd.onExit(io.output.refresh := True)
    refreshReady.onExit(refreshStream.ready := True)
  }

  refreshStream.ready := False
  io.output.prechargeAll := False
  io.output.refresh := False

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
