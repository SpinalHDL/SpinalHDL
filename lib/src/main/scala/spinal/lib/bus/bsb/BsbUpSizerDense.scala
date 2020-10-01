package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._

class BsbUpSizerDense(p : BsbParameter, outputBytes : Int) extends Component{
  val io = new Bundle{
    val input = slave(Bsb(p))
    val output = master(Bsb(p.copy(byteCount = outputBytes)))
  }

  assert(p.byteCount <= outputBytes)
  val ratio = outputBytes/p.byteCount

  val valid = RegInit(False)
  val counter = Reg(UInt(log2Up(ratio) bits)) init(0)
  val buffer = Reg(BsbTransaction(p.copy(byteCount = outputBytes)))
  buffer.last.init(False)
  buffer.mask.init(0)

  val full = counter === 0 || buffer.last
  val canAggregate = valid && !buffer.last && !full && buffer.source === io.input.source && buffer.sink === io.input.sink
  val onOutput = Bool()
  val counterSample = canAggregate ? counter | U(0)

  when(io.output.fire){
    valid := False
    buffer.mask := 0
  }
  when(io.input.fire){
    valid := True
    buffer.source := io.input.source
    buffer.sink := io.input.sink
    buffer.data.subdivideIn(ratio slices)(counterSample) := io.input.data
    buffer.mask.subdivideIn(ratio slices)(counterSample) := io.input.mask
    buffer.last := io.input.last
    counter := counterSample + 1
  }

  io.output.valid   := valid && (valid && full || io.input.valid && !canAggregate)
  io.output.payload := buffer
  io.input.ready := !valid || canAggregate || io.output.ready
}
