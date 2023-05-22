package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.Elab
import spinal.lib.{master, slave}

//While create a interconnect master as an io of the toplevel
class MasterBus(p : M2sParameters)(implicit ic : Interconnect) extends Area{
  val node = ic.createMaster()
  val logic = Elab build new Area{
    node.m2s.parameters.load(p)
    node.m2s.setProposedFromParameters() //Here, we just ignore the negotiation phase
    slave(node.bus)
  }
}

//While create a interconnect slave as an io of the toplevel
class SlaveBus(m2sSupport : M2sSupport)(implicit ic : Interconnect) extends Area{
  val node = ic.createSlave()
  val logic = Elab build new Area {
    node.s2m.none() //We do not want to implement memory coherency
    node.m2s.supported.load(m2sSupport)
    master(node.bus)
  }
}