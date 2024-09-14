package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.memory.sdram.dfi.Interface._

case class MakeTask(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Component{
  import tpa._
  val io = new Bundle{
    val cmd = slave(Stream(TaskCmd(tpp, tpa)))
    val refresh = slave(Event)
    val writeDataToken = slave(Stream(Event))
    val output = master(OpTasks(tpa))
  }
  val config = TaskTimingConfig(tpa)
  val readyForRefresh = True

  val banksRow = Mem(UInt(pl.sdram.rowWidth bits), pl.sdram.bankCount)

  def Timing(loadValid : Bool, loadValue : UInt, timingWidth : Int = tp.timingWidth) = new Area{
    val value = Reg(UInt(timingWidth bits)) randBoot()
    val increment = value =/= loadValue.resized
    val busy = CombInit(increment)
    value := value + increment.asUInt.resized
    when(loadValid) {
      value := 0
    }
  }

  val CCD = (pl.beatCount > 1) generate Timing(io.output.task.read || io.output.task.write, pl.beatCount-2, log2Up(pl.beatCount))
  val RFC = Timing(io.output.refresh,  config.RFC, tp.timingWidth+3)
  val RRD = Timing(io.output.task.active,  config.RRD)
  val WTR = Timing(io.output.task.write,  config.WTR)
  val RTW = Timing(io.output.task.read,  config.RTW)
  val RP  = Timing(io.output.prechargeAll,  config.RP + 1)
  val FAW = generation.FAW generate new Area{ //Can be optimized
    val trigger = io.output.task.active
    val ptr = RegInit(U"00")
    val slots = (0 to 3).map(i => Timing(ptr === i && trigger,  config.FAW))
    val busyNext =  Vec(slots.map(_.busy)).read(ptr+1)
    ptr := ptr + U(trigger)
  }


  val banks = for(bankId <- 0 until pl.sdram.bankCount) yield new Area {
    val hits = io.output.task.address.bank === bankId
    def gate(task: Bool) = hits & task

    val activeNext = Bool()
    val active = RegNext(activeNext) init(False)
    activeNext := active
    when(gate(io.output.task.precharge) || io.output.prechargeAll){
      activeNext := False
    }otherwise {
      when(gate(io.output.task.active)){
        activeNext := True
      }
    }

    val WR  = Timing(gate(io.output.task.write),  config.WR)
    val RAS = Timing(gate(io.output.task.active),  config.RAS)
    val RP  = Timing(gate(io.output.task.precharge),  config.RP)
    val RCD = Timing(gate(io.output.task.active),  config.RCD)
    val RTP = Timing(gate(io.output.task.read),  config.RTP)

    val allowPrecharge = !WR.busy && !RAS.busy && !RTP.busy
    val allowActive = !RP.busy
    val allowWrite = !RCD.busy
    val allowRead = !RCD.busy
  }
  val allowPrechargeAll = banks.map(_.allowPrecharge).andR

  case class Status() extends Bundle {
    val bankActive = Bool()
    val bankHit = Bool()
    val allowPrecharge = Bool()
    val allowActive = Bool()
    val allowWrite = Bool()
    val allowRead = Bool()

    def patch(address: BusAddress): Unit = {
      allowPrecharge clearWhen (!banks.map(_.allowPrecharge).read(address.bank))
      allowActive clearWhen (!banks.map(_.allowActive).read(address.bank))
      allowWrite clearWhen (!banks.map(_.allowWrite).read(address.bank))
      allowRead clearWhen (!banks.map(_.allowRead).read(address.bank))

      when(io.output.task.address.bank === address.bank) {
        when(io.output.task.precharge) {
          bankActive := False
        }
        when(io.output.task.active) {
          bankActive := True
          bankHit := io.output.task.address.row === address.row
          allowRead := False
          allowWrite := False
          allowPrecharge := False
        }
        when(io.output.task.read || io.output.task.write) {
          allowPrecharge := False
        }
        when(io.output.task.precharge) {
          allowActive := False
        }
      }
    }
  }
  
  
  readyForRefresh clearWhen(io.cmd.valid)
  
  val taskConstructor = new Area {
    val input = io.cmd.stage()
    val address = input.address.as(BusAddress(pl.sdram,tpa.config))
    val status = Status()
    status.allowPrecharge := True
    status.allowActive := !RRD.busy && (if(generation.FAW) !FAW.busyNext else True)
    status.allowWrite := !RTW.busy && (if(CCD != null) !CCD.busy else True)
    status.allowRead := !WTR.busy &&  (if(CCD != null) !CCD.busy else True)
    status.bankHit := banksRow.readAsync(address.bank) === address.row
    status.bankActive := banks.map(_.active).read(address.bank)
    status.patch(address)
    readyForRefresh clearWhen(input.valid)
  }
  val columnBurstShift = log2Up(pl.transferPerBurst)
  val columnBurstMask  = (pl.sdram.columnSize-1) - (tpa.stationLengthMax-1 << columnBurstShift)
  val stations = new Area {
    val valid = RegInit(False)
    val status = Reg(Status())
    val address = Reg(BusAddress(tpa.pl.sdram,tpa.config))
    val write = Reg(Bool())
    val context = Reg(Bits(backendContextWidth bits))
    val offset, offsetLast = Reg(UInt(tpa.stationLengthWidth bits))

    import status._
    allowPrecharge := True
    allowActive := !RRD.busy && (if(generation.FAW) !FAW.busyNext else True)
    allowWrite := !RTW.busy && (if(CCD != null) !CCD.busy else True)
    allowRead := !WTR.busy &&  (if(CCD != null) !CCD.busy else True)
    status.patch(address)

    val inputActive = !bankActive
    val inputPrecharge = bankActive && !bankHit
    val inputAccess =  bankActive && bankHit
    val inputWrite =  bankActive && bankHit && write
    val inputRead  = bankActive && bankHit && !write

    val doActive = inputActive && allowActive
    val doPrecharge = inputPrecharge && allowPrecharge
    val doWrite = inputWrite && allowWrite && io.writeDataToken.valid
    val doRead = inputRead && allowRead
    val doAccess = doWrite || doRead
    val doSomething = valid && (doActive || doPrecharge || doWrite || doRead)

    val blockedByWriteToken = inputWrite && allowWrite && !io.writeDataToken.valid //For debug visualisation

    val fire = False //It is the last cycle for this station
    val last = offset === offsetLast
    val cmdOutputPayload = OpTask(tpa)
    io.output.task.address.column := address.column | (offset << columnBurstShift).resized
    io.output.task.address.assignUnassignedByName(address)
    io.output.task.context := context
    io.output.task.active := inputActive
    io.output.task.precharge := inputPrecharge
    io.output.task.write := doWrite & valid
    io.output.task.read := doRead & valid
    io.output.task.last := last

    io.writeDataToken.ready.clear()
    io.writeDataToken.ready.setWhen(io.output.task.write)

    when(doAccess & valid){
      offset := offset + 1
      when(last) {
        valid := False
        fire := True
      }
    }

    readyForRefresh clearWhen(valid)
  }

  val loader = new Area{
    taskConstructor.input.ready := !stations.valid
    val offset = taskConstructor.address.column(columnBurstShift, tpa.stationLengthWidth bits)
    val offsetLast = offset + taskConstructor.input.length
      val canSpawn = !stations.valid
      //Insert taskConstructor into one free station
      when(taskConstructor.input.valid && canSpawn) {
        stations.valid          := True
        stations.status         := taskConstructor.status
        stations.address.column := taskConstructor.address.column & columnBurstMask
        stations.address.assignUnassignedByName(taskConstructor.address)
        stations.write          := taskConstructor.input.write
        stations.context        := taskConstructor.input.context
        stations.offset         := offset
        stations.offsetLast     := offsetLast
      }
  }

  val askRefresh = io.refresh.valid && readyForRefresh
  io.refresh.ready := False
  io.output.prechargeAll := False
  io.output.refresh := False

  val fsm = new StateMachine{
    val idle            = new State with EntryPoint
    val prechargeAllCmd = new State
    val refreshCmd      = new State
    val refreshReady    = new State
    idle.whenIsActive(when(askRefresh){goto(prechargeAllCmd)})
    prechargeAllCmd.whenIsActive(when(RegNext(allowPrechargeAll) & askRefresh){goto(refreshCmd)})
    refreshCmd.whenIsActive(when(!RP.busy & askRefresh){goto(refreshReady)})
    refreshReady.whenIsActive(when(!RFC.busy & askRefresh){goto(idle)})

    prechargeAllCmd.onExit(io.output.prechargeAll := True)
    refreshCmd.onExit(io.output.refresh := True)
    refreshReady.onExit(io.refresh.ready := True)
  }

  val stationsPatch = new Area{
    import stations._
    when(io.output.task.active){
      status.allowActive := False
    }

    if(CCD != null) when(io.output.task.read || io.output.task.write){
      status.allowRead := False
      status.allowWrite := False
    } else {
      when(io.output.task.read){
        status.allowWrite := False
      }
      when(io.output.task.write){
        status.allowRead := False
      }
    }
  }

  val selectedAddress =  io.output.task.address
  when(stations.doSomething){
    banksRow.write(selectedAddress.bank, selectedAddress.row)
  }

}
