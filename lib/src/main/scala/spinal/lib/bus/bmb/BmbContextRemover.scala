package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._


object BmbContextRemover{
  def getOutputParameter(inputParameter : BmbParameter) = {
    inputParameter.copy(access = inputParameter.access.copy(sources = List(0 -> inputParameter.access.aggregated.copy(contextWidth = 0)).toMapLinked()))
  }
}

case class BmbContextRemover(p: BmbParameter,
                             pendingMax : Int) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(BmbContextRemover.getOutputParameter(p)))
  }

  val (fifoFork, cmdFork) = StreamFork2(io.input.cmd)
  io.output.cmd << cmdFork
  io.output.cmd.context.removeAssignments()
  io.output.cmd.source.removeAssignments()

  case class Ctx() extends Bundle{
    val source = UInt(p.access.sourceWidth bits)
    val context = Bits(p.access.contextWidth bits)
  }
  val pushCtx = Ctx()
  pushCtx.source := fifoFork.source
  pushCtx.context := fifoFork.context
  val popCtx = fifoFork.throwWhen(!fifoFork.first).translateWith(pushCtx).queue(pendingMax).m2sPipe()
  popCtx.ready := io.output.rsp.valid && io.output.rsp.last && io.input.rsp.ready

  io.input.rsp.arbitrationFrom(io.output.rsp.haltWhen(!popCtx.valid))
  io.input.rsp.payload := io.output.rsp.payload
  io.input.rsp.context.removeAssignments() := popCtx.context
  io.input.rsp.source.removeAssignments() := popCtx.source
}
