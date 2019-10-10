package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Tasker(cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val refresh = slave(Event)
    val inputs = Vec(cpp.map(cpp => slave(Stream(Fragment(CoreCmd(cpp, cpa))))))
    val output = master(CoreTasks(cpa))
  }

  val banksRow = Mem(UInt(pl.sdram.rowWidth bits), pl.bankCount)

  def Timing(loadValid : Bool, loadValue : UInt, timingWidth : Int = cp.timingWidth) = new Area{
    val value = Reg(UInt(timingWidth bits)) init(0)
    val notZero = value =/= 0
    val busyNext = notZero  || loadValid
    val busy = RegNext(busyNext)
    value := value - notZero.asUInt
    when(loadValid) { value := loadValue }
  }

//  val CCD = Timing(trigger.CCD, pl.sdram.generation.CCD/pl.dataRatio-1)
  val RFC = Timing(io.output.refresh, io.config.RFC, cp.timingWidth+3)
  val RRD = Timing(io.output.ports.map(p => p.active).orR, io.config.RRD)
  val WTR = Timing(io.output.ports.map(p => p.write).orR, io.config.WTR)
  val RTW = Timing(io.output.ports.map(p => p.read).orR, io.config.RTW)
  val RP  = Timing(io.output.prechargeAll, io.config.RP)
  val FAW = generation.FAW generate new Area{ //Can be optimized
    val trigger = io.output.ports.map(p => p.active).orR
    val ptr = RegInit(U"00")
    val slots = (0 to 3).map(i => Timing(ptr === i && trigger, io.config.FAW))
//    val busyNext = Vec(slots.map(_.busy)).read(ptr) || (Vec(slots.map(_.busy)).read(ptr+1) && trigger)
    val busyNext =  Vec(slots.map(_.busy)).read(ptr+1)
    ptr := ptr + U(trigger)
  }


  val banks = for(bankId <- 0 until pl.bankCount) yield new Area {
    val hits = B(io.output.ports.map(_.address.bank === bankId))
    def portEvent(f : CoreTask => Bool) = (hits & B(io.output.ports.map(f))).orR

    val activeNext = Bool
    val active = RegNext(activeNext) init(False)
    activeNext := active
    when(portEvent(p => p.precharge) || io.output.prechargeAll){
      activeNext := False
    }
    when(portEvent(p => p.active)){
      activeNext := True
    }



    val WR  = Timing(portEvent(p => p.write), io.config.WR)
    val RAS = Timing(portEvent(p => p.active), io.config.RAS)
    val RP  = Timing(portEvent(p => p.precharge), io.config.RP)
    val RCD = Timing(portEvent(p => p.active), io.config.RCD)
    val RTP = Timing(portEvent(p => p.read && p.last), io.config.RTP)

    val allowPrecharge = RegNext(!WR.busyNext && !RAS.busyNext && !RTP.busyNext)
    val allowActive = RegNext(!RP.busyNext && !RRD.busyNext && !FAW.busyNext)
    val allowWrite = RegNext(!RCD.busyNext && !RTW.busyNext)
    val allowRead = RegNext(!RCD.busyNext  && !WTR.busyNext)
  }
  val allowPrechargeAll = banks.map(_.allowPrecharge).orR
//  val allowPrechargeAll = RegNext(banks.map(_.allowPrechargeNext).orR)



  val gates = for ((port, inputId) <- io.inputs.zipWithIndex) yield new Area {

    val bankActive = Reg(Bool)
    val bankHit = Reg(Bool)
    val portAddress = port.address.as(SdramAddress(pl.sdram))
    val input = port.stage()
    val address = input.address.as(SdramAddress(pl.sdram))
//    val patchAddress = input.isStall ? portAddress | address
    val empty = !(input.valid || port.valid)

//    val bankHit = banksRow.readAsync(address.bank) === address.row
//    val bankActive = banks.map(_.active ).read(address.bank)
    val allowPrecharge = Reg(Bool)
    val allowActive = Reg(Bool)
    val allowWrite = Reg(Bool)
    val allowRead = Reg(Bool)

    def patch(address : SdramAddress): Unit ={
      allowPrecharge := banks.map(_.allowPrecharge ).read(address.bank)
      allowActive := banks.map(_.allowActive ).read(address.bank)
      allowWrite := banks.map(_.allowWrite ).read(address.bank)
      allowRead := banks.map(_.allowRead ).read(address.bank)

      for(output <- io.output.ports){
        when(output.address.bank === address.bank) {
          when(output.precharge){
            bankActive := False
          }
          when(output.active) {
            bankActive := True
            bankHit := output.address.row === address.row
            allowRead := False
            allowWrite := False
          }
          when(output.read || output.write){
            allowPrecharge := False
          }
          when(output.precharge){
            allowActive := False
          }
        }
      }
    }

    when(!input.isStall) {
      bankHit := banksRow.readAsync(portAddress.bank) === portAddress.row
      bankActive := banks.map(_.active ).read(portAddress.bank)
      patch(portAddress)
    } otherwise {
      patch(address)
    }

    when(io.output.ports.map(_.active).orR){
      allowActive := False
    }
    when(io.output.ports.map(_.read).orR){
      allowWrite := False
    }
    when(io.output.ports.map(_.write).orR){
      allowRead := False
    }

    val inputActive = !bankActive
    val inputPrecharge = bankActive && !bankHit
    val inputWrite =  bankActive && bankHit && input.write
    val inputRead  = bankActive && bankHit && !input.write
    val inibated = False

    val doActive = inputActive && allowPrecharge
    val doPrecharge = inputPrecharge && allowActive
    val doWrite = inputWrite && allowWrite
    val doRead = inputRead && allowRead
    val doSomething = input.valid && (doActive || doPrecharge || doWrite || doRead)

    val cmdOutputPayload = CoreTask(cpa)
    io.output.ports(inputId).last := input.last
    io.output.ports(inputId).source := inputId
    io.output.ports(inputId).address := address
    io.output.ports(inputId).data := input.data
    io.output.ports(inputId).mask := input.mask
    io.output.ports(inputId).context := input.context.resized
    io.output.ports(inputId).active := inputActive
    io.output.ports(inputId).precharge := inputPrecharge
    io.output.ports(inputId).write := inputWrite
    io.output.ports(inputId).read := inputRead
  }

  val arbiter = new Area{
    val arbiterState = RegInit(B(1, cpp.size bits))
    def OhArbiter(that : Seq[Bool]) = OHMasking.roundRobin(that.asBits, arbiterState)

    val masked = OhArbiter(gates.map(_.doSomething))
    for((gate, gateId) <- gates.zipWithIndex){
      val idxs = (gateId + 1 until gates.size) ++ (0 until gateId)
      var watches = List[Bool]()
      for(otherId <- idxs; other = gates(otherId)){
        watches = arbiterState(otherId) :: watches
        val bankMatch = other.address.bank === gate.address.bank
        when(other.input.valid && watches.orR) {
          when((gate.inputActive || gate.inputPrecharge) && bankMatch) {
            gate.inibated := True
          }
          when(gate.inputWrite && other.inputRead || gate.inputRead && other.inputWrite){
            gate.inibated := True
          }
        }
      }
    }



    val tockenIncrement = (masked & arbiterState & gates.map(g => g.bankActive && g.bankHit && g.input.last).asBits).orR
    val tocken = Reg(UInt(log2Up(cp.portTockenMax) bits)) init(0)
    when(tockenIncrement){
      tocken := tocken + 1
    }

    when(!(gates.map(_.input.valid).asBits & arbiterState).orR || tockenIncrement && ((gates.map(_.input.burstLast).asBits & arbiterState).orR && tocken >= cp.portTockenMin || tocken === cp.portTockenMax-1)){
      arbiterState := arbiterState.rotateLeft(1)
      tocken := 0
    }

    for((gate, sel, port) <- (gates, masked.asBools,io.output.ports).zipped){
      when(!sel){
        port.read := False
        port.write := False
        port.active := False
        port.precharge := False
      }
      gate.input.ready := sel && gate.bankActive && gate.bankHit
    }

    val askRefresh = io.refresh.valid && gates.map(_.empty).orR
    io.refresh.ready := False
    io.output.prechargeAll := False
    io.output.refresh := False
    val refreshState = RegInit(U"00")
    when(askRefresh){
      switch(refreshState){
        is(0) {
          when(allowPrechargeAll) {
            io.output.prechargeAll := True
            refreshState := 1
          }
        }
        is(1) {
          when(!RP.busy) {
            io.output.refresh := True
            refreshState := 2
          }
        }
        is(2){
          when(!RFC.busy){
            io.refresh.ready := True
            refreshState := 0
          }
        }
      }
    }
  }

  val selectedAddress = MuxOH(arbiter.masked, io.output.ports.map(_.address))
  when(arbiter.masked.orR){
    banksRow.write(selectedAddress.bank, selectedAddress.row)
  }
}
