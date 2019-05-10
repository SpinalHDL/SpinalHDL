package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}


case class BmbToApb3Bridge(apb3Config: Apb3Config,
                           bmbParameter : BmbParameter,
                           pipelineBridge : Boolean) extends Component{
  assert(apb3Config.dataWidth == bmbParameter.dataWidth)
  assert(bmbParameter.lengthWidth == log2Up(bmbParameter.dataWidth/8))

  val io = new Bundle {
    val input = slave(Bmb(bmbParameter))
    val output = master(Apb3(apb3Config))
  }

  val bmbBuffer = Bmb(bmbParameter)
  bmbBuffer.cmd << (if(pipelineBridge) io.input.cmd.halfPipe() else io.input.cmd).haltWhen(io.input.rsp.isStall)
  bmbBuffer.rsp >-> io.input.rsp

  val state = RegInit(False)
  bmbBuffer.cmd.ready := False

  io.output.PSEL(0) := bmbBuffer.cmd.valid
  io.output.PENABLE := state
  io.output.PWRITE  := bmbBuffer.cmd.isWrite
  io.output.PADDR   := bmbBuffer.cmd.address.resized
  io.output.PWDATA  := bmbBuffer.cmd.data

  bmbBuffer.rsp.valid := False
  bmbBuffer.rsp.data  := io.output.PRDATA
  when(!state) {
    state := bmbBuffer.cmd.valid
  } otherwise {
    when(io.output.PREADY){
      state := False
      bmbBuffer.rsp.valid := True
      bmbBuffer.cmd.ready := True
    }
  }

  io.input.rsp.source  := RegNextWhen(io.input.cmd.source,  io.input.cmd.ready)
  io.input.rsp.context := RegNextWhen(io.input.cmd.context, io.input.cmd.ready)

  if(apb3Config.useSlaveError) {
    bmbBuffer.rsp.setSuccess()
    when(io.output.PSLVERROR) {
      bmbBuffer.rsp.setError()
    }
  }
}