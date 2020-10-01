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


case class BsbDownSizerAlignedMultiWidth(val p : BsbParameter, outputBytes : List[Int]) extends Component{
  val io = new Bundle{
    val sel = in UInt(log2Up(outputBytes.length) bits)
    val input = slave(Bsb(p))
    val output = master(Bsb(p.copy(byteCount = outputBytes.max, withMask = false)))
  }

  assert(p.byteCount >= outputBytes.max)
  val counter = Reg(UInt(log2Up(p.byteCount) bits)) init(0)
  val end = False

  when(io.output.fire){
    counter := counter + outputBytes.map(U(_)).read(io.sel).resized
  }
  when(io.input.fire){
    counter := 0
  }

  io.input.ready := io.output.ready && end
  io.output.valid := io.input.valid
  io.output.source := io.input.source
  io.output.sink   := io.input.sink
  io.output.last   := io.input.last && end

  io.output.data.assignDontCare()
  io.output.mask := 0

  switch(io.sel){
    for((bytes, id) <- outputBytes.zipWithIndex){
      is(id){
        val masks = io.input.mask.subdivideIn(bytes bits)
        io.output.data(0, bytes*8 bits) := io.input.data.subdivideIn(bytes*8 bits).read(counter >> log2Up(bytes))
        io.output.mask(0, bytes bits)   := masks.read(counter >> log2Up(bytes))
        end setWhen(counter === p.byteCount - bytes)
      }
    }
  }
}
