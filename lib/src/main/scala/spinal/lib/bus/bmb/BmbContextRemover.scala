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

  val popCtx = fifoFork.translateWith(fifoFork.context).queue(pendingMax)

  io.input.rsp.arbitrationFrom(StreamJoin(popCtx, io.output.rsp))
  io.input.rsp.payload := io.output.rsp.payload
  io.input.rsp.context.removeAssignments() := popCtx.payload
}
