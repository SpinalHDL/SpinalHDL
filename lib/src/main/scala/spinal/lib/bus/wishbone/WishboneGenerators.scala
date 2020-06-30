package spinal.lib.bus.wishbone

import spinal.lib.bus.bmb.BmbSmpInterconnectGenerator
import spinal.lib.generator._

case class WishboneToBmbGenerator()(implicit interconnect : BmbSmpInterconnectGenerator = null) extends Generator{
  val config = createDependency[WishboneConfig]
  val wishbone = produce(logic.io.input)
  val bmb = produce(logic.io.output)
  val logic = add task WishboneToBmb(config)

  if(interconnect != null) interconnect.addMaster(
    accessRequirements = config.derivate(WishboneToBmb.getBmbRequirements),
    bus = bmb
  )
}