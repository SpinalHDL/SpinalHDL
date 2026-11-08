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

/** DDR4 power-on init sequence (simplified, JESD79-4B 4.9.1 / Figure 6):
  * xprWait -> MRS MR3, MR6, MR5, MR4, MR2, MR1, MR0 (tMRD apart) -> ZQCL -> zqInitCycles -> done.
  * Emits Ddr4InitOp pulses consumed by MakeTask onto the task port. */
case class Ddr4Init(
    modeRegs: Ddr4ModeRegs,
    tMrdCycles: Int = 2,
    zqInitCycles: Int = 256,
    xprWaitCycles: Int = 8
) extends Component {
  val io = new Bundle {
    val initOp = master(Flow(Ddr4InitOp()))
    val busy = out Bool ()
    val done = out Bool ()
  }
  val orderList = List(3, 6, 5, 4, 2, 1, 0) // spec order; MR0 last with DLL reset bit set
  // Both Vecs are 16-entry and every index is 4-bit wide: SpinalHDL checks the index
  // width against log2Up(Vec size) at elaboration, so widths must line up exactly
  // (step: 4 bits -> order; order entries: 4 bits -> opCodes).
  val order = Vec.tabulate(16)(i => U(if (i < orderList.length) orderList(i) else 0, 4 bits))
  val opCodeList = List(modeRegs.mr0, modeRegs.mr1, modeRegs.mr2, modeRegs.mr3, modeRegs.mr4, modeRegs.mr5, modeRegs.mr6)
  val opCodes = Vec.tabulate(16)(i => U(if (i < opCodeList.length) opCodeList(i) else 0, 16 bits))
  val step = Reg(UInt(4 bits)) init (0)
  val timer = Reg(UInt(16 bits)) init (xprWaitCycles)
  val doneReg = RegInit(False)

  io.busy := !doneReg
  io.done := doneReg
  io.initOp.valid := False
  io.initOp.mrs := False
  io.initOp.zq := False
  io.initOp.mrIndex := 0
  io.initOp.opCode := 0

  when(!doneReg) {
    when(timer =/= 0) {
      timer := timer - 1
    } otherwise {
      when(step < orderList.length) {
        io.initOp.valid := True
        io.initOp.mrs := True
        io.initOp.mrIndex := order(step).resized // 4->3 bits: values are 0..7 by construction
        io.initOp.opCode := opCodes(order(step))
        step := step + 1
        timer := tMrdCycles
      } elsewhen (step === orderList.length) {
        io.initOp.valid := True
        io.initOp.zq := True
        step := step + 1
        timer := zqInitCycles
      } otherwise {
        doneReg := True
      }
    }
  }
}

case class Ddr4InitOp() extends Bundle {
  val mrs = Bool()
  val zq = Bool()
  val mrIndex = UInt(3 bits)
  val opCode = UInt(16 bits)
}

case class MakeTask(taskConfig: TaskConfig, dfiConfig: DfiConfig, addrMap: AddrMap) extends Component {
  val io = new Bundle {
    val cmd = slave(Stream(TaskWrRdCmd(taskConfig, dfiConfig)))
    val halt = out Bool ()
    val writeDataToken = slave(Stream(Event))
    val output = master(OpTasks(taskConfig, dfiConfig))
  }
  val timeConfig = TaskTimingConfig(dfiConfig)
  val readyForRefresh = True
  val sdram = dfiConfig.sdram

  val ddr4Init = (sdram.generation == SdramGeneration.DDR4) generate Ddr4Init(
    modeRegs = Ddr4ModeRegs(),
    tMrdCycles = 4,
    zqInitCycles = 512,
    xprWaitCycles = 32
  )
  def initDone: Bool = if (ddr4Init != null) ddr4Init.io.done else True
  val banksRow = Mem(UInt(dfiConfig.sdram.rowWidth bits), sdram.bankCount)
  val CCD =
    (dfiConfig.beatCount > 1) generate timing(
      io.output.read || io.output.write,
      dfiConfig.beatCount - 2,
      log2Up(dfiConfig.beatCount)
    )
  val RFC = timing(io.output.refresh, timeConfig.RFC, taskConfig.taskParameter.timingWidth + 3)
  // Bank-group-aware ACT-to-ACT spacing.
  // Legacy (DDR2/DDR3): a single global RRD timer, exactly as before.
  // DDR4: both timers reload on EVERY ACT. The candidate's relation to the last ACT's bank
  // group selects which gates it: same group -> tRRD_L, different group -> tRRD_S.
  // (tRRD_L/S are both measured from the previous ACT, so both timers must be live after it.)
  val RRD_busy: Bool = if (dfiConfig.sdram.generation == SdramGeneration.DDR4) {
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
  // Bank-group-aware WRITE-to-READ turnaround.
  // Legacy: one global WTR timer (unchanged). DDR4: both timers reload on every write;
  // a read in the same bank group as the last write waits tWTR (L), a different group tWTR_S.
  val WTR_busy: Bool = if (dfiConfig.sdram.generation == SdramGeneration.DDR4) {
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
  // Bank-group-aware CAS-to-CAS (tCCD) between column commands.
  // Legacy: no extra gate (the station's 1-cycle block already provides tCCD_S-equivalent spacing).
  // DDR4: a column command into the SAME bank group as the previous column command must wait
  // tCCD_L (4nCK = 2 MC cycles at fr2); cross-group keeps the existing 1-cycle spacing (tCCD_S).
  val CCD_BG = if (dfiConfig.sdram.generation == SdramGeneration.DDR4) new Area {
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
  // (DDR4 only) takes precedence during the power-up sequence.
  io.output.mrs := False
  io.output.zq := False

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
    io.output.active := inputActive
    io.output.precharge := inputPrecharge
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
    taskConstructor.input.ready := !station.valid && initDone
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
    val canSpawn = !station.valid && initDone
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
  val askRefresh = refreshStream.valid && readyForRefresh && initDone
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

  // DDR4 MRS/ZQ override (conditional, so no assignment overlap with the station path).
  // While init runs, the normal command path is held off: the station cannot spawn, the
  // refresh FSM is gated, and active/precharge are forced low here so no ACT/PRE leaks
  // onto the DFI bus before MRS+ZQCL complete (the agent asserts on that).
  if (ddr4Init != null) {
    when(!ddr4Init.io.done) {
      io.output.active := False
      io.output.precharge := False
      io.output.mrs := ddr4Init.io.initOp.mrs
      io.output.zq := ddr4Init.io.initOp.zq
      // MR select rides the low 3 bits of the flat bank index: {BG0=MRSEL2, BA1:0=MRSEL1:0}
      io.output.address.bank := ddr4Init.io.initOp.mrIndex.resized
      // OP code rides the row field (MRS: A15:0 = OP). Truncation is safe for the
      // supported geometries: all defined OP bits of the default mode regs fit in 13 bits.
      io.output.address.row := ddr4Init.io.initOp.opCode.resized
      io.output.last := True
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
  refreshStream.ready := False
  io.output.prechargeAll := False
  io.output.refresh := False

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
