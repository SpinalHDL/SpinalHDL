//TODO: FIx if nothing is selected, drive bus with zeroes
package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

object WishboneArbiter{
  def apply(config : WishboneConfig, inputCount : Int, priority : Int = 1) = new WishboneArbiter(config, inputCount, priority)
}

class WishboneArbiter(config : WishboneConfig, inputCount : Int, priority : Int = 1) extends Component{
  val io = new Bundle{
    val inputs = Vec(slave(Wishbone(config)), inputCount)
    val output = master(Wishbone(config))
  }
  val status = Vec(io.inputs.map(_.CYC))
  val select = Reg(UInt(log2Up(inputCount) bits)) init(0)

  io.inputs.map(_.clearAll)

  when(!status(select)){
    select := OHToUInt(OHMasking.roundRobin(status.asBits, B(priority)))
  }
    io.inputs(select) <> io.output
}