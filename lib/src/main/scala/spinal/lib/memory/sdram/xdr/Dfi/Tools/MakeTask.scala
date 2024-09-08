package spinal.lib.memory.sdram.xdr.Dfi.Tools

import spinal.core._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{BusAddress, DfiConfig, SdramAddress, CorePortParameter, CoreCmd, CoreConfig, CoreParameterAggregate, CoreTask, CoreTasks}
import spinal.lib._

case class MakeTask(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Component{
  import cpa._
  val io = new Bundle{
    val cmd = slave(Stream(CoreCmd(cpp, cpa)))
    val refresh = slave(Event)
    val writeDataTockens = in UInt(cpp.writeTockenInterfaceWidth bits)
    val output = master(CoreTasks(cpa))
  }
  val config = CoreConfig(cpa)
  val readyForRefresh = True

  val banksRow = Mem(UInt(pl.sdram.rowWidth bits), pl.sdram.bankCount)

  def Timing(loadValid : Bool, loadValue : UInt, timingWidth : Int = cp.timingWidth) = new Area{
    val value = Reg(UInt(timingWidth bits)) randBoot()
    val notZero = value =/= loadValue.resized
    val busy = CombInit(notZero)
    value := value + notZero.asUInt.resized
    when(loadValid) {
      value := 0
    }
  }

  val CCD = (pl.beatCount > 1) generate Timing(io.output.task.read || io.output.task.write, pl.beatCount-2, log2Up(pl.beatCount))
  val RFC = Timing(io.output.refresh,  config.RFC, cp.timingWidth+3)
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
    val hits = B(io.output.task.address.bank === bankId)
    def portEvent(f : CoreTask => Bool) = (hits & B(f(io.output.task))).orR

    val activeNext = Bool()
    val active = RegNext(activeNext) init(False)
    activeNext := active
    when(portEvent(p => p.precharge) || io.output.prechargeAll){
      activeNext := False
    }
    when(portEvent(p => p.active)){
      activeNext := True
    }

    val WR  = Timing(portEvent(p => p.write),  config.WR)
    val RAS = Timing(portEvent(p => p.active),  config.RAS)
    val RP  = Timing(portEvent(p => p.precharge),  config.RP)
    val RCD = Timing(portEvent(p => p.active),  config.RCD)
    val RTP = Timing(portEvent(p => p.read),  config.RTP)

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
  val writeTockens =  new Area{
    val canWrite = cpp.canWrite
    val consume = io.output.task.write
    val counter = canWrite generate Reg(UInt(log2Up(cpp.writeTockenBufferSize + 1) bits)).init(0)
    if(canWrite) {
      counter := counter + io.writeDataTockens - (U(consume) << log2Up(pl.beatCount))
    }
    val ready = if(canWrite) RegInit(False) setWhen(counter >= pl.beatCount) clearWhen(consume && counter < pl.beatCount*2) else True
  }

  case class Task() extends Bundle{
    val write = Bool()
    val address = UInt(pl.sdram.byteAddressWidth+chipSelectWidth bits)
    val context = Bits(backendContextWidth bits)
    val burstLast = Bool()
    val length = Reg(UInt(cpa.stationLengthWidth bits))
//    val portId = UInt(log2Up(cpp.size) bits)
  }

  val inputsArbiter = new Area {
    def inputs = io.cmd

    val output = Stream(Task())
    val inputsValids = inputs.valid
    output.valid := inputsValids
    inputs.ready := output.ready

    val converted = cloneOf(output.payload)
    converted.write := inputs.write
    converted.address := inputs.address
    converted.context := inputs.context.resized
    converted.burstLast := inputs.burstLast
    converted.length := inputs.length

    output.payload :=  converted

    readyForRefresh clearWhen(inputsValids)
  }
  val taskConstructor = new Area {
    val s0 = new Area {
      def input = inputsArbiter.output
      val portAddress = input.address.as(BusAddress(pl.sdram,cpa.config))
    }
    val s1 = new Area {
      val input = s0.input.stage()
      val address = input.address.as(BusAddress(pl.sdram,cpa.config))
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
  }
  val columnBurstShift = log2Up(pl.transferPerBurst)
  val columnBurstMask  = (pl.sdram.columnSize-1) - (cpa.stationLengthMax-1 << columnBurstShift)
  val stations = new Area {
    val valid = RegInit(False)
    val status = Reg(Status())
    val address = Reg(BusAddress(cpa.pl.sdram,cpa.config))
    val write = Reg(Bool())
    val context = Reg(Bits(backendContextWidth bits))
    val offset, offsetLast = Reg(UInt(cpa.stationLengthWidth bits))

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
    val doWrite = inputWrite && allowWrite && writeTockens.ready
    val doRead = inputRead && allowRead
    val doAccess = doWrite || doRead
    val doSomething = valid && (doActive || doPrecharge || doWrite || doRead)

    val blockedByWriteTocken = inputWrite && allowWrite && !writeTockens.ready //For debug visualisation

    val fire = False //It is the last cycle for this station
    val last = offset === offsetLast
    val cmdOutputPayload = CoreTask(cpa)
    io.output.task.address.byte := address.byte
    io.output.task.address.column := address.column | (offset << columnBurstShift).resized
    io.output.task.address.row := address.row
    io.output.task.address.bank := address.bank
    io.output.task.address.csAddr := address.csAddr
    io.output.task.context := context
    io.output.task.active := inputActive
    io.output.task.precharge := inputPrecharge
    io.output.task.write := doWrite & valid
    io.output.task.read := doRead & valid
    io.output.task.last := last

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
    taskConstructor.s1.input.ready := !stations.valid
    val offset = taskConstructor.s1.address.column(columnBurstShift, cpa.stationLengthWidth bits)
    val offsetLast = offset + taskConstructor.s1.input.length
      val canSpawn = !stations.valid
      //Insert taskConstructor into one free station
      when(taskConstructor.s1.input.valid && canSpawn) {
        stations.valid          := True
        stations.status         := taskConstructor.s1.status
        stations.address.byte   := taskConstructor.s1.address.byte
        stations.address.column := taskConstructor.s1.address.column & columnBurstMask
        stations.address.row    := taskConstructor.s1.address.row
        stations.address.bank   := taskConstructor.s1.address.bank
        stations.address.csAddr := taskConstructor.s1.address.csAddr
        stations.offset         := offset
        stations.offsetLast     := offsetLast
        stations.write          := taskConstructor.s1.input.write
        stations.context        := taskConstructor.s1.input.context
      }
  }

val askRefresh = io.refresh.valid && readyForRefresh
  io.refresh.ready := False
  io.output.prechargeAll := False
  io.output.refresh := False
  val refreshState = RegInit(U"00")
  when(askRefresh){
    switch(refreshState){
      is(0) { //Dummy state to ensure allowPrechargeAll propagation
        refreshState := 1
      }
      is(1) {
        when(RegNext(allowPrechargeAll)) {
          io.output.prechargeAll := True
          refreshState := 2
        }
      }
      is(2) {
        when(!RP.busy) {
          io.output.refresh := True
          refreshState := 3
        }
      }
      is(3){
        when(!RFC.busy){
          io.refresh.ready := True
          refreshState := 0
        }
      }
    }
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
