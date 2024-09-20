package spinal.lib.memory.sdram.dfi.foundation

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.memory.sdram.dfi.interface._

case class MakeTask(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val cmd = slave(Stream(TaskWrRdCmd(tpp, tpa)))
    val refresh = slave(Event)
    val writeDataToken = slave(Stream(Event))
    val output = master(OpTasks(tpa))
  }
  val timeConfig = TaskTimingConfig(tpa)
  val readyForRefresh = True

  val banksRow = Mem(UInt(config.sdram.rowWidth bits), config.sdram.bankCount)
  val CCD =
    (config.beatCount > 1) generate Timing(
      io.output.read || io.output.write,
      config.beatCount - 2,
      log2Up(config.beatCount)
    )
  val RFC = Timing(io.output.refresh, timeConfig.RFC, tp.timingWidth + 3)
  val RRD = Timing(io.output.active, timeConfig.RRD)
  val WTR = Timing(io.output.write, timeConfig.WTR)
  val RTW = Timing(io.output.read, timeConfig.RTW)
  val RP = Timing(io.output.prechargeAll, timeConfig.RP + 1)
  val FAW = generation.FAW generate new Area { // Can be optimized
    val trigger = io.output.active
    val ptr = RegInit(U"00")
    val slots = (0 to 3).map(i => Timing(ptr === i && trigger, timeConfig.FAW))
    val busyNext = Vec(slots.map(_.busy)).read(ptr + 1)
    ptr := ptr + U(trigger)
  }
  val banks = for (bankId <- 0 until config.sdram.bankCount) yield new Area {
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

    val WR = Timing(gate(io.output.write), timeConfig.WR)
    val RAS = Timing(gate(io.output.active), timeConfig.RAS)
    val RP = Timing(gate(io.output.precharge), timeConfig.RP)
    val RCD = Timing(gate(io.output.active), timeConfig.RCD)
    val RTP = Timing(gate(io.output.read), timeConfig.RTP)

    val allowPrecharge = !WR.busy && !RAS.busy && !RTP.busy
    val allowActive = !RP.busy
    val allowWrite = !RCD.busy
    val allowRead = !RCD.busy
  }
  val allowPrechargeAll = banks.map(_.allowPrecharge).andR
  val taskConstructor = new Area {
    val input = io.cmd.stage()
    val address = input.address.as(BusAddress(config.sdram, tpa.config))
    val status = Status()
    status.allowPrecharge := True
    status.allowActive := !RRD.busy && (if (generation.FAW) !FAW.busyNext else True)
    status.allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True)
    status.allowRead := !WTR.busy && (if (CCD != null) !CCD.busy else True)
    status.bankHit := banksRow.readAsync(address.bank) === address.row
    status.bankActive := banks.map(_.active).read(address.bank)
    status.employ(address)
    readyForRefresh clearWhen (input.valid)
  }
  val columnBurstShift = log2Up(config.transferPerBurst)

  readyForRefresh clearWhen (io.cmd.valid)
  val columnBurstMask = (config.sdram.columnSize - 1) - (tpa.stationLengthMax - 1 << columnBurstShift)
  val stations = new Area {
    val valid = RegInit(False)
    val status = Reg(Status())
    val address = Reg(BusAddress(tpa.config.sdram, tpa.config))
    val write = Reg(Bool())
    val context = Reg(Bits(contextWidth bits))
    val offset, offsetLast = Reg(UInt(tpa.stationLengthWidth bits))

    import status._
    allowPrecharge := True
    allowActive := !RRD.busy && (if (generation.FAW) !FAW.busyNext else True)
    allowWrite := !RTW.busy && (if (CCD != null) !CCD.busy else True)
    allowRead := !WTR.busy && (if (CCD != null) !CCD.busy else True)
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

    val fire = False // It is the last cycle for this station
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
        fire := True
      }
    }
    readyForRefresh clearWhen (valid)
  }
  val selectedAddress = io.output.address
  val loader = new Area {
    taskConstructor.input.ready := !stations.valid
    val offset = taskConstructor.address.column(columnBurstShift, tpa.stationLengthWidth bits)
    val offsetLast = offset + taskConstructor.input.length
    val canSpawn = !stations.valid
    // Insert taskConstructor into one free station
    when(taskConstructor.input.valid && canSpawn) {
      stations.valid := True
      stations.status := taskConstructor.status
      stations.address.column := taskConstructor.address.column & columnBurstMask
      stations.address.assignUnassignedByName(taskConstructor.address)
      stations.write := taskConstructor.input.write
      stations.context := taskConstructor.input.context
      stations.offset := offset
      stations.offsetLast := offsetLast
    }
  }
  val askRefresh = io.refresh.valid && readyForRefresh
  when(stations.doSomething) {
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
    refreshReady.onExit(io.refresh.ready := True)
  }

  def Timing(loadValid: Bool, loadValue: UInt, timingWidth: Int = tp.timingWidth) = new Area {
    val value = Reg(UInt(timingWidth bits)) randBoot ()
    val increment = value =/= loadValue.resized
    val busy = CombInit(increment)
    value := value + increment.asUInt.resized
    when(loadValid) {
      value := 0
    }
  }
  io.refresh.ready := False
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
