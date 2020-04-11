package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbInvalidateMonitor{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(canInvalidate = false, contextWidth = inputParameter.contextWidth + 1)
}

case class BmbInvalidateMonitor(inputParameter : BmbParameter, pendingInvMax : Int) {
  assert(inputParameter.alignment == BmbParameter.BurstAlignement.LENGTH)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(BmbInvalidateMonitor.outputParameter(inputParameter)))
  }

  val ackCounter = CounterUpDown(
    stateCount = log2Up(pendingInvMax) + 1,
    incWhen    = io.input.ack.fire,
    decWhen    = io.output.rsp.fire && io.output.rsp.context.msb
  )
  val invCounter = CounterUpDown(
    stateCount = log2Up(pendingInvMax) + 1,
    incWhen    = io.input.inv.fire,
    decWhen    = io.output.rsp.fire && io.output.rsp.context.msb
  )

  val haltInv = invCounter.msb //Protect counter from overflow
  val haltRsp = ackCounter === 0 //Ensure that writes is coherent


  val (cmdToOutput, cmdToInv) = StreamFork2(io.input.cmd)

  io.output.cmd << cmdToOutput
  io.output.rsp.haltWhen(haltRsp) >> io.input.rsp

  io.output.cmd.context.removeAssignments() := cmdToOutput.isWrite ## cmdToOutput.context
  io.input.rsp.context.removeAssignments()  := io.output.rsp.context.resized

  val cmdToInvFiltred = cmdToInv.takeWhen(cmdToInv.isWrite && cmdToInv.isFirst).haltWhen(haltInv)
  io.input.inv.arbitrationFrom(cmdToInvFiltred)
  io.input.inv.address := cmdToInvFiltred.address
  io.input.inv.length := cmdToInvFiltred.length
  io.input.inv.source := cmdToInvFiltred.source
  io.input.inv.all    := False

  io.input.ack.ready := True
}
