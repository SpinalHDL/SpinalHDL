package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Tasker(cp: CoreParameter) extends Component{
  def pl = cp.pl
  def ml = pl.ml

  val io = new Bundle {
    val backendFull = in Bool()
    val refresh = slave(Event)
    val inputs = Vec(slave(Stream(Fragment(CoreCmd(cp)))), cp.portCount)
    val output = master(Stream(Fragment(FrontendCmdOutput(cp))))
  }

  val banks = for(bankId <- 0 until ml.bankCount) yield Reg(new Bundle {
    val active = Bool()
    val row = UInt(ml.rowWidth bits)
  })
  val banksActive = banks.map(_.active).orR

  val gates = for ((port, inputId) <- io.inputs.zipWithIndex) yield new Area {
//    val s0 = new Area{
//      val address = port.address.as(SdramAddress(ml))
//      val bank = banks.read(address.bank)
//      val needActive = !bank.active
//      val needPrecharge = bank.active && bank.row =/= address.row
//    }
    val s1 = new Area{
//      val input = port.stage()
//      val inputActive = RegNextWhen(s0.needActive, input.ready)
//      val inputPrecharge = RegNextWhen(s0.needPrecharge, input.ready)
      def input = port
      val address = port.address.as(SdramAddress(ml))
      val bank = banks.read(address.bank)
      val inputActive = !bank.active
      val inputPrecharge = bank.active && bank.row =/= address.row

      val doActive = input.valid && inputActive
      val doPrecharge = input.valid && inputPrecharge
      val doWrite = input.valid && input.write && !doActive && !doPrecharge
      val doRead = input.valid && !input.write && !doActive && !doPrecharge
      val doNotLast = input.valid && !input.last

      val cmdOutputPayload = Fragment(FrontendCmdOutput(cp))
      cmdOutputPayload.last := port.last
      cmdOutputPayload.fragment.source := inputId
      cmdOutputPayload.fragment.address := address
      cmdOutputPayload.fragment.data := port.data
      cmdOutputPayload.fragment.mask := port.mask
      cmdOutputPayload.fragment.context := port.context
      cmdOutputPayload.fragment.all := False
      cmdOutputPayload.fragment.kind := (port.write ? FrontendCmdOutputKind.WRITE | FrontendCmdOutputKind.READ)
      when(doActive){ cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.ACTIVE }
      when(doPrecharge){ cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.PRECHARGE }
    }
  }

  val arbiter = new Area{
    val arbiterState = RegInit(B(1, cp.portCount bits))
    val writeFirst = RegInit(False)
    def OhArbiter(that : Seq[Bool]) = OHMasking.roundRobin(that.asBits, arbiterState)

    val ohPrecharge = OhArbiter(gates.map(_.s1.doPrecharge))
    val ohActive = OhArbiter(gates.map(_.s1.doActive))
    val ohWrite = OhArbiter(gates.map(_.s1.doWrite))
    val ohRead = OhArbiter(gates.map(_.s1.doRead))
    val ohNotLast = OhArbiter(gates.map(_.s1.doNotLast))

    val pendingPrecharge = gates.map(_.s1.doPrecharge).orR
    val pendingActive = gates.map(_.s1.doActive).orR
    val pendingWrite = gates.map(_.s1.doWrite).orR
    val pendingRead = gates.map(_.s1.doRead).orR
    val pendingNotLast = gates.map(_.s1.doNotLast).orR

    def maskClear = B(0, cp.portCount bits)
    val maskedPrecharge = (!pendingNotLast && !pendingRead && !pendingWrite && !pendingActive) ? ohPrecharge | maskClear
    val maskedActive = (!pendingNotLast && !pendingRead && !pendingWrite)  ? ohActive | maskClear
    val maskedWrite = (!pendingNotLast && !(!writeFirst && pendingRead)) ? ohWrite | maskClear
    val maskedRead = (!pendingNotLast && !(writeFirst && pendingWrite))? ohRead | maskClear
    val maskedNotLast = ohNotLast

    val masked = maskedPrecharge | maskedActive | maskedWrite | maskedRead | maskedNotLast
    val doRefresh = io.refresh.valid && !pendingNotLast

    when(io.output.fire && masked.orR){
      arbiterState := masked
    }

    writeFirst setWhen(!pendingRead) clearWhen(!pendingWrite)

    io.output.valid :=  doRefresh || pendingPrecharge || pendingActive || (!io.backendFull && (pendingWrite || pendingRead))
    io.output.payload := MuxOH(masked, gates.map(_.s1.cmdOutputPayload))
    for((gate, sel) <- (gates, masked.asBools).zipped){
      gate.s1.input.ready := io.output.ready && sel && !io.backendFull
    }

    io.refresh.ready := False
    when(doRefresh){
      gates.foreach(_.s1.input.ready := False)
      when(banksActive){
        io.output.kind := FrontendCmdOutputKind.PRECHARGE
        io.output.all := True
      } otherwise {
        io.output.kind := FrontendCmdOutputKind.REFRESH
        io.refresh.ready := io.output.ready
      }
    }
  }

  val outputBank = Vec(banks)(io.output.address.bank)
  when(io.output.fire){
    outputBank.row := io.output.address.row
    switch(io.output.kind) {
      is(FrontendCmdOutputKind.ACTIVE) {
        outputBank.active := True
      }
      is(FrontendCmdOutputKind.PRECHARGE) {
        outputBank.active := False
        when(io.output.all){
          banks.foreach(_.active := False)
        }
      }
    }
  }
}
