package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._


//p should be canInvalidate only
class BmbInvalidationArbiter(p : BmbParameter,
                             portCount : Int,
                             pendingMax : Int) extends Component{
  val io = new Bundle {
    val inputs = Vec(master(Bmb(p)), portCount)
    val output = slave(Bmb(p))
  }


  val invArbiter = StreamArbiterFactory.roundRobin.transactionLock.build(BmbInv(p), portCount)
  invArbiter.io.inputs <> Vec(io.inputs.map(_.inv))

  val (ctxFork, outputFork) = StreamFork2(invArbiter.io.output, synchronous = true)
  io.output.inv << outputFork

  val ctx = ctxFork.translateWith(invArbiter.io.chosen).queueLowLatency(pendingMax, latency = 1)
  ctx.ready := io.output.ack.fire

  for((input, portId) <- io.inputs.zipWithIndex){
    val hit = portId === ctx.payload
    input.ack.valid := io.output.ack.valid && hit
    input.ack.payload := io.output.ack.payload
  }

  io.output.ack.ready := io.inputs.map(_.ack.ready).read(ctx.payload)
}
