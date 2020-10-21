package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

class Axi4SharedToAxi3Shared(p : Axi4Config) extends Component{
  val io = new Bundle {
    val input = slave(Axi4Shared(p))
    val output = master(Axi4Shared(p))
    val output_aw_id = out UInt(p.idWidth bits)
  }

  val (cmdFork, writeFork) = StreamFork2(io.input.arw)
  io.output.arw << cmdFork

  val writeId = writeFork.translateWith(writeFork.id).throwWhen(!writeFork.write).queueLowLatency(4, 1)
  writeId.ready := io.output.w.fire && io.output.w.last
  io.output.w << io.input.w.haltWhen(!writeId.valid)
  io.output_aw_id := writeId.payload

  io.input.r << io.output.r
  io.input.b << io.output.b
}
