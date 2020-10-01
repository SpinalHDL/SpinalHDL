package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbSourceRemover{
  def getOutputParameter(inputParameter : BmbParameter) = {
    var aggregated = inputParameter.access.aggregated
    aggregated = aggregated.copy(contextWidth = inputParameter.access.aggregated.contextWidth + inputParameter.access.sourceWidth)
    inputParameter.copy(access = inputParameter.access.withSingleSource(aggregated))
  }
}

case class BmbSourceRemover(p: BmbParameter) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(BmbSourceRemover.getOutputParameter(p)))
  }

  case class Context() extends Bundle{
    val source = UInt(p.access.sourceWidth bits)
    val context = Bits(p.access.contextWidth bits)
  }

  val cmdContext = Context()
  cmdContext.source := io.input.cmd.source
  cmdContext.context := io.input.cmd.context
  io.output.cmd.arbitrationFrom(io.input.cmd)
  io.output.cmd.payload.assignSomeByName(io.input.cmd.payload)
  io.output.cmd.source.removeAssignments()
  io.output.cmd.context.removeAssignments() := B(cmdContext)

  val rspContext = io.output.rsp.context.as(Context())
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.payload.assignSomeByName(io.output.rsp.payload)
  io.input.rsp.source.removeAssignments()  := rspContext.source
  io.input.rsp.context.removeAssignments() := rspContext.context
}
