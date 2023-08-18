package spinal.lib.misc.plic

import spinal.core._
import spinal.lib.bus.amba4.axilite._
import spinal.lib._

class AxiLite4Plic(sourceCount : Int, targetCount : Int) extends Component{
  val priorityWidth = 2
  val plicMapping = PlicMapping.sifive
  import plicMapping._

  val io = new Bundle {
    val bus = slave(AxiLite4(AxiLite4Config(22, 32)))
    val sources = in Bits (sourceCount bits)
    val targets = out Bits (targetCount bits)
  }

  val gateways = (for ((source, id) <- (io.sources.asBools, 1 to sourceCount).zipped) yield PlicGatewayActiveHigh(
    source = source,
    id = id,
    priorityWidth = priorityWidth
  )).toSeq

  val targets = for (i <- 0 until targetCount) yield PlicTarget(
    id = i,
    gateways = gateways,
    priorityWidth = priorityWidth
  )

  io.targets := targets.map(_.iep).asBits

  val bus = new AxiLite4SlaveFactory(io.bus)
  val mapping = PlicMapper(bus, plicMapping)(
    gateways = gateways,
    targets = targets
  )
}
