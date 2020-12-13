package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbInvalidateMonitor{
  def outputAccessParameter(inputParameter : BmbAccessParameter) = inputParameter.sourcesTransform(s => s.copy(
    contextWidth = 1 + s.contextWidth + inputParameter.addressWidth + s.lengthWidth
  ))
  def outputInvalidationParameter() = BmbInvalidationParameter()
}

case class BmbInvalidateMonitor(inputParameter : BmbParameter,
                                pendingInvMax : Int) extends Component{
  val outputParameter = BmbInvalidateMonitor.outputAccessParameter(inputParameter.access)

  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  case class Context() extends Bundle{
    val context = Bits(inputParameter.access.contextWidth bits)
    val address = UInt(inputParameter.access.addressWidth bits)
    val length  = UInt(inputParameter.access.lengthWidth bits)
    val write = Bool()
  }

  val cmdLogic = new Area {
    val cmdContext = Context()
    cmdContext.context := io.input.cmd.context
    cmdContext.write := io.input.cmd.isWrite
    cmdContext.address := io.input.cmd.address
    cmdContext.length := io.input.cmd.length

    io.output.cmd << io.input.cmd
    io.output.cmd.context.removeAssignments().assignFromBits(B(cmdContext))
  }


  val rspLogic = new Area {
    val rspContext = io.output.rsp.context.as(Context())
    val (rspToRsp, rspToInv, rspToSync) = StreamFork3(io.output.rsp)

    io.input.rsp << rspToRsp
    io.input.rsp.context.removeAssignments() := rspContext.context

    val rspToInvFiltred = rspToInv.takeWhen(rspContext.write)
    io.input.inv.arbitrationFrom(rspToInvFiltred)
    io.input.inv.address := rspContext.address
    io.input.inv.length := rspContext.length
    io.input.inv.source := rspToInvFiltred.source
    io.input.inv.all    := False

    val rspToSyncFiltred = rspToSync.translateWith(rspToInv.source).takeWhen(rspContext.write)
  }


  val ackLogic = new Area{
    val syncSource = rspLogic.rspToSyncFiltred.s2mPipe().queue(pendingInvMax)
    syncSource.ready := io.input.ack.fire

    io.input.sync.arbitrationFrom(io.input.ack)
    io.input.sync.source := syncSource.payload
  }
}