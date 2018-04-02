package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

/** Factory for [[spinal.lib.bus.wishbone.WishboneArbiter]] instances. */
object WishboneArbiter{
  /** Create a wishbone Arbiter/multiplexer
    * the arbiter will not switch to other interfaces until the selected master/input CYC line goes to zero
    * @constructor create a WishboneArbiter instance
    * @param config it will use for configuring all the input/output wishbone port
    * @param inputCount the number of master interface
    * @param priority wich interface is considerate first in the roundrobin algorithm
    * @return a instantiated WishboneArbiter class
    */
  def apply(config : WishboneConfig, inputCount : Int, priority : Int = 1) = new WishboneArbiter(config, inputCount, priority)

  /** Create a wishbone Arbiter/multiplexer
    * @param masters a list of master interfaces
    * @param slave the slave interface, the [[spinal.lib.bus.wishbone.Wishbone.config]] will be used for all input/output
    */
  def apply(masters: Seq[Wishbone], slave: Wishbone): WishboneArbiter = {
    val arbiter = new WishboneArbiter(slave.config, masters.size)
    arbiter.io.output <> slave
    (arbiter.io.inputs, masters).zipped.map(_ <> _)
    arbiter.setPartialName(slave,"arbiter")
  }
}

/** Create a wishbone Arbiter/multiplexer
  * the arbiter will not switch to other interfaces until the selected master/input CYC line goes to zero
  * @constructor create a WishboneArbiter instance
  * @param config it will use for configuring all the input/output wishbone port
  * @param inputCount the number of master interface
  * @param priority wich interface is considerate first in the roundrobin algorithm
  */
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