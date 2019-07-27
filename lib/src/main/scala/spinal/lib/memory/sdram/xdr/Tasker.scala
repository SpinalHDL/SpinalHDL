package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Tasker(cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val backendFull = in Bool()
    val refresh = slave(Event)
    val inputs = Vec(cpp.map(cpp => slave(Stream(Fragment(CoreCmd(cpp, cpa))))))
    val output = master(Flow(Fragment(CoreTask(cpa))))
  }

  //Request to load timings counters
  val trigger = new Area{
    val WR,RAS,RP,RCD,WTR,CCD,RFC,RTP,RRD = False
    val FAW = pl.withFaw generate False
  }

  val banks = for(bankId <- 0 until pl.bankCount) yield new Area {
    val active = RegInit(False)
    val row = Reg(UInt(pl.sdram.rowWidth bits))

    val hit = io.output.address.bank === bankId
    val WR  = Timing(hit && trigger.WR, io.config.WR)
    val RAS = Timing(hit && trigger.RAS, io.config.RAS)
    val RP  = Timing(hit && trigger.RP, io.config.RP)
    val RCD = Timing(hit && trigger.RCD, io.config.RCD)
    val RTP = Timing(hit && trigger.RTP, io.config.RTP)
    val allowPrecharge = !WR.busy && !RAS.busy
  }
  val banksActive = banks.map(_.active).orR

  def Timing(loadValid : Bool, loadValue : UInt) = new Area{
    val value = Reg(UInt(cp.timingWidth bits)) init(0)
    val busy = value =/= 0
    value := value - busy.asUInt
    when(loadValid) { value := loadValue }
  }






  val CCD = Timing(trigger.CCD, pl.CCD-1)
  val RFC = Timing(trigger.RFC, io.config.RFC)
  val RRD = Timing(trigger.RRD, io.config.RRD)
  val WTR = Timing(trigger.WTR, io.config.WTR)
  val FAW = pl.withFaw generate new Area{
    val ptr = RegInit(U"00")
    val slots = (0 to 3).map(i => Timing(ptr === i && trigger.FAW, io.config.FAW))
    val busy = slots.map(_.busy).read(ptr)
    ptr := ptr + U(trigger.FAW)
  }


  val gates = for ((port, inputId) <- io.inputs.zipWithIndex) yield new Area {
    def input = port
    val address = port.address.as(SdramAddress(pl.sdram))
    val bankActive = banks.map(_.active).read(address.bank)
    val bankRow = banks.map(_.row).read(address.bank)
    val inputActive = !bankActive
    val inputPrecharge = bankActive && bankRow =/= address.row
    val inputWrite =  input.write && !inputActive && !inputPrecharge
    val inputRead  = !input.write && !inputActive && !inputPrecharge
    val inibated = False

    val RP  = banks.map(_.RP .busy).read(address.bank)
    val RCD = banks.map(_.RCD.busy).read(address.bank)
    val RTP = banks.map(_.RTP.busy).read(address.bank)
    val allowPrecharge = banks.map(_.allowPrecharge).read(address.bank)

    val doActive = inputActive && !RFC.busy && !RP && !RRD.busy
    val doPrecharge = inputPrecharge && allowPrecharge
    val doWrite = inputWrite && !RCD && !CCD.busy && !RTP
    val doRead = inputRead && !RCD && !CCD.busy && !WTR.busy

    val doSomething = input.valid && (doActive || doPrecharge || doWrite || doRead)

    val cmdOutputPayload = Fragment(CoreTask(cpa))
    cmdOutputPayload.last := port.last || inputActive || inputPrecharge
    cmdOutputPayload.fragment.source := inputId
    cmdOutputPayload.fragment.address := address
    cmdOutputPayload.fragment.data := port.data
    cmdOutputPayload.fragment.mask := port.mask
    cmdOutputPayload.fragment.context := port.context
    cmdOutputPayload.fragment.all := False
    cmdOutputPayload.fragment.kind := (port.write ? FrontendCmdOutputKind.WRITE | FrontendCmdOutputKind.READ)
    when(inputActive){ cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.ACTIVE }
    when(inputPrecharge){ cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.PRECHARGE }
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
            gate.inibated := False
          }
          when(gate.inputWrite && other.inputRead || gate.inputRead && other.inputWrite){
            gate.inibated := False
          }
        }
      }
    }

    val locks = gates.map(!_.input.first)
    when(locks.orR){
      masked := B(locks)
    }



    val askRefresh = io.refresh.valid && !locks.orR
    val tocken = Reg(UInt(log2Up(cp.portTocken) bits)) init(0)
    when(masked === arbiterState && !askRefresh && !io.backendFull && (io.output.kind === FrontendCmdOutputKind.WRITE || io.output.kind === FrontendCmdOutputKind.READ)){
      tocken := tocken + 1
      when(tocken === cp.portTocken-1){
        arbiterState := arbiterState.rotateRight(1)
        if(!isPow2(cp.portTocken)) tocken := 0
      }
    }


    io.output.valid :=  masked.orR && !askRefresh && !io.backendFull
    io.output.payload := MuxOH(masked, gates.map(_.cmdOutputPayload))
    for((gate, sel) <- (gates, masked.asBools).zipped){
      gate.input.ready := sel && !askRefresh && !io.backendFull && !gate.inputPrecharge && !gate.inputActive
    }

    io.refresh.ready := False
    when(askRefresh){
      gates.foreach(_.input.ready := False)
      when(banksActive){
        io.output.kind := FrontendCmdOutputKind.PRECHARGE
        io.output.all := True
        io.output.last := True
        when(banks.map(_.allowPrecharge).orR){
          io.output.valid := True
        }
      } otherwise {
        io.output.kind := FrontendCmdOutputKind.REFRESH
        io.output.last := True
        when(!banks.map(_.RP.busy).orR){
          io.output.valid := True
          io.refresh.ready := True
        }
      }
    }
  }

  when(io.output.fire){
    banks.map(_.row).write(io.output.address.bank, io.output.address.row)
    switch(io.output.kind) {
      is(FrontendCmdOutputKind.READ) {
        trigger.CCD := True
        trigger.RTP := True
      }
      is(FrontendCmdOutputKind.WRITE) {
        when(io.output.first) {
          trigger.CCD := True
          trigger.WTR := True
          trigger.WR := True
        }
      }
      is(FrontendCmdOutputKind.ACTIVE) {
        banks.map(_.active).write(io.output.address.bank, True)
        trigger.RAS := True
        trigger.RCD := True
        trigger.RRD := True
        if(pl.withFaw) trigger.FAW := True
      }
      is(FrontendCmdOutputKind.PRECHARGE) {
        trigger.RP := True
        banks.map(_.active).write(io.output.address.bank, False)
        when(io.output.all){
          banks.foreach(_.active := False)
        }
      }
      is(FrontendCmdOutputKind.REFRESH) {
        trigger.RFC := True
      }
    }
  }
}
