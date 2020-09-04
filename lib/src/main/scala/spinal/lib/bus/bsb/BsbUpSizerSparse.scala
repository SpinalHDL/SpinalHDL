package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._

class BsbUpSizerSparse(p : BsbParameter, outputBytes : Int) extends Component{
  val io = new Bundle{
    val input = slave(Bsb(p))
    val output = master(Bsb(p.copy(byteCount = outputBytes)))
  }

  assert(p.byteCount <= outputBytes)
  val ratio = outputBytes/p.byteCount
  io.output.arbitrationFrom(io.input)
  io.output.data.assignDontCare()
  io.output.data(io.input.data.range)   := io.input.data
  io.output.mask   := io.input.mask.resized
  io.output.source := io.input.source
  io.output.sink   := io.input.sink
  io.output.last   := io.input.last
}
