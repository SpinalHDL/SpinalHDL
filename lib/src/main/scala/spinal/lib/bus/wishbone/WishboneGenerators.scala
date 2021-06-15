package spinal.lib.bus.wishbone

import spinal.lib.bus.bmb.BmbInterconnectGenerator
import spinal.core._
import spinal.core.fiber._

case class WishboneToBmbGenerator()(implicit interconnect : BmbInterconnectGenerator = null) extends Area{
  val config = Handle[WishboneConfig]
  val wishbone = Handle(logic.io.input)
  val bmb = Handle(logic.io.output)
  val logic = Handle(WishboneToBmb(config))

  if(interconnect != null) interconnect.addMaster(
    accessRequirements = config.derivate(WishboneToBmb.getBmbRequirements),
    bus = bmb
  )
}