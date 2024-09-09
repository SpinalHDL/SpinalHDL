package spinal.lib.memory.sdram.Dfi.Tools

import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb.BmbPortParameter
import spinal.lib.memory.sdram.Dfi.Interface.{CoreParameterAggregate, CorePort, CorePortParameter}
//import spinal.lib.memory.sdram.xdr.
import spinal.lib._


case class BmbToCorePort(ip : BmbParameter, cpp : CorePortParameter, cpa : CoreParameterAggregate, pp : BmbPortParameter) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool()
    val output = master(CorePort(cpp, cpa))
  }

  case class Context() extends Bundle{
    val source = UInt(ip.access.sourceWidth bits)
    val input = Bits(ip.access.contextWidth bits)
  }

//  val cmdToRspCount = io.output.cmd.write ? U(1) | (io.output.cmd.length +^ 1) << log2Up(cpa.pl.beatCount)
  val cmdToRspCount = io.output.cmd.write ? U(0) | (io.output.cmd.length +^ 1) << log2Up(cpa.pl.beatCount)

  val rspPendingCounter = Reg(UInt(log2Up(pp.rspBufferSize + 1) bits)) init(0)
  rspPendingCounter := rspPendingCounter + (io.input.cmd.lastFire ? cmdToRspCount | U(0)) - U(io.output.rsp.fire)

  val toManyRsp = (U"0" @@ rspPendingCounter) + cmdToRspCount > pp.rspBufferSize //pp.rspBufferSize - pp.beatPerBurst*cpa.pl.beatCount //Pessimistic

  io.input.cmd.ready := io.output.cmd.ready && !toManyRsp
  if(ip.access.canWrite) io.input.cmd.ready clearWhen(!io.output.writeData.ready)

  val cmdContext = Context()
  cmdContext.input := io.input.cmd.context
  cmdContext.source := io.input.cmd.source

  io.output.cmd.valid := io.input.cmd.firstFire
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  assert(widthOf(io.output.cmd.length) >= widthOf(io.input.cmd.length) - log2Up(cpa.pl.bytePerBurst))
  io.output.cmd.length := (io.input.cmd.length >> log2Up(cpa.pl.bytePerBurst)).resized
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.burstLast := io.inputBurstLast

  if(ip.access.canWrite) {
    io.output.writeData.valid := io.input.cmd.fire && io.input.cmd.isWrite
    io.output.writeData.data := io.input.cmd.data
    io.output.writeData.mask := io.input.cmd.mask
  }

  val rspContext = io.output.rsp.context.as(Context())
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.last := io.output.rsp.last
  if(ip.access.canRead) io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.input
  io.input.rsp.source := rspContext.source
}
