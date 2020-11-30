package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._


case class BmbWriteRetainer(p: BmbParameter, queueSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Bmb(BmbSourceRemover.getOutputParameter(p)))
  }

  val cmdBuffer = io.input.cmd.queue(queueSize).stage()
  val tocken = CounterUpDown(
    stateCount = queueSize,
    incWhen = io.input.cmd.lastFire,
    decWhen = cmdBuffer.lastFire
  )

  val halt = cmdBuffer.first && tocken === 0
  io.output.cmd << cmdBuffer.haltWhen(halt)

  io.input.rsp << io.output.rsp
}
