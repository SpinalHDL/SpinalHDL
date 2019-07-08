package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Tasker(cp: CoreParameter) extends Component{
  def pl = cp.pl
  def ml = pl.ml

  val io = new Bundle {
    val inputs = Vec(slave(Stream(Fragment(CoreCmd(cp)))), cp.portCount)
    val output = master(Stream(Fragment(FrontendCmdOutput(cp))))
  }

  val banks = for(bankId <- 0 until ml.bankWidth) yield Reg(new Bundle {
    val active = Bool()
    val row = UInt(ml.rowWidth bits)
  })

  val gates = for (port <- io.inputs) yield new Area {
    val address = port.address.as(SdramAddress(ml))
    val bank = banks.read(address.bank)
    val cmdOutputPayload = Fragment(FrontendCmdOutput(cp))
    cmdOutputPayload.last := port.last
    cmdOutputPayload.fragment.address := address
    cmdOutputPayload.fragment.data := port.data
    //      cmdOutputPayload.active = !bank.active
    //      cmdOutputPayload.precharge = bank.active && bank.row =/= address.row
    //      cmdOutputPayload.refresh = False
    cmdOutputPayload.fragment.all := False
    cmdOutputPayload.fragment.kind := (port.write ? FrontendCmdOutputKind.WRITE | FrontendCmdOutputKind.READ)
    when(!bank.active){
      cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.ACTIVE
    } elsewhen(bank.row =/= address.row){
      cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.PRECHARGE
    }
    val output = port.toEvent().translateWith(cmdOutputPayload)
  }

  val output = StreamArbiterFactory.transactionLock.lowerFirst.on(gates.map(_.output))
  val outputBank = Vec(banks)(output.address.bank)
  when(output.fire){
    outputBank.row := output.address.row
    switch(output.kind) {
      is(FrontendCmdOutputKind.ACTIVE) {
        outputBank.active := True
      }
      is(FrontendCmdOutputKind.PRECHARGE) {
        outputBank.active := False
        when(output.all){
          banks.foreach(_.active := False)
        }
      }
    }
  }
}
