package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._

class BsbDownSizerSparse(val p : BsbParameter, outputBytes : Int) extends Component{
  val io = new Bundle{
    val input = slave(Bsb(p))
    val output = master(Bsb(p.copy(byteCount = outputBytes)))
  }

  assert(p.byteCount >= outputBytes)
  val ratio = p.byteCount/outputBytes
  val counter = Reg(UInt(log2Up(ratio) bits)) init(0)
  val end = counter === ratio-1

  when(io.output.fire){
    counter := counter + 1
  }

  io.input.ready := io.output.ready && end
  io.output.valid := io.input.valid
  io.output.data   := io.input.data.subdivideIn(outputBytes*8 bits).read(counter)
  io.output.mask   := io.input.mask.subdivideIn(outputBytes bits).read(counter)
  io.output.source := io.input.source
  io.output.sink   := io.input.sink
  io.output.last   := io.input.last && end
}
