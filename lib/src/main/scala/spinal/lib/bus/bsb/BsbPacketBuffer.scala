package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._

class BsbPacketBuffer(p : BsbParameter, bufferWords : Int, packetsMax : Int) extends Component{
  val io = new Bundle{
    val input = slave(Bsb(p))
    val output = master(Bsb(p))
  }

  val counter = CounterUpDown(
    packetsMax + 1,
    io.input.fire && io.input.last,
    io.output.fire && io.output.last
  )
  val fifo = StreamFifo(BsbTransaction(p), bufferWords)
  fifo.io.push << io.input.haltWhen(counter.mayOverflow)
  fifo.io.pop.haltWhen(counter === 0) >> io.output
}
