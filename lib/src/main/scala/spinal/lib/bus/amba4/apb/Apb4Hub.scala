package spinal.lib.bus.amba4.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

class Apb4Hub(c: Apb4Config, maps: Seq[SizeMapping]) extends Component {
  val io = new Bundle {
    val mst  = slave(Apb4(c))
    val slvs = Vec(master(Apb4(c.copy(selWidth = 1))), maps.size)
  }
  io.slvs.zip(Apb4Decoder(io.mst, maps)).map{case(a, b) => a << b}
}
