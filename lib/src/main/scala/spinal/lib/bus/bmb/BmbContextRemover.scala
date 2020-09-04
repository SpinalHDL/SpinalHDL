package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._


object BmbContextRemover{
  def getOutputParameter(inputParameter : BmbParameter) = {
    inputParameter.copy(access = inputParameter.access.sourcesTransform(_.copy(contextWidth = 0)))
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

  val popCtx = fifoFork.throwWhen(!fifoFork.first).translateWith(fifoFork.context).queue(pendingMax)
  popCtx.ready := io.output.rsp.valid && io.output.rsp.last && io.input.rsp.ready

  io.input.rsp.arbitrationFrom(io.output.rsp.haltWhen(!popCtx.valid))
  io.input.rsp.payload := io.output.rsp.payload
  io.input.rsp.context.removeAssignments() := popCtx.payload
}
