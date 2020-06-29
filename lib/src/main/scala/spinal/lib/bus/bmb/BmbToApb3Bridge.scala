package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}



object BmbToApb3Bridge{
  def busCapabilities(addressWidth : Int,
                      dataWidth : Int) = BmbAccessCapabilities(
    addressWidth    = addressWidth,
    dataWidth       = dataWidth,
    lengthWidthMax  = log2Up(dataWidth/8),
    alignment       = BmbParameter.BurstAlignement.LENGTH
  )
}


case class BmbToApb3Bridge(apb3Config: Apb3Config,
                           bmbParameter : BmbParameter,
                           pipelineBridge : Boolean) extends Component{
  assert(apb3Config.dataWidth == bmbParameter.access.dataWidth)
  assert(bmbParameter.access.lengthWidth == log2Up(bmbParameter.access.dataWidth/8))

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

  bmbBuffer.rsp.source  := io.input.cmd.source
  bmbBuffer.rsp.context := io.input.cmd.context
  bmbBuffer.rsp.last := True
  
  bmbBuffer.rsp.setSuccess()
  if(apb3Config.useSlaveError) {
    when(io.output.PSLVERROR) {
      bmbBuffer.rsp.setError()
    }
  }
}