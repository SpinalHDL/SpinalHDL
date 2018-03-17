package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

case class WishboneDecoder(config : WishboneConfig, decodings : Seq[SizeMapping]) extends Component {
  val io = new Bundle {
    val input = slave(Wishbone(config))
    val outputs = Vec(master(Wishbone(config)),decodings.size)
  }

  io.outputs.map(_.clearAll)
  io.input.clearAll()

  for((slave, select) <- decodings.zipWithIndex){
    when(slave.hit(io.input.ADR) && io.input.CYC){
      io.outputs(select) <> io.input
    }
  }
}