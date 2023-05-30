package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.Elab
import spinal.lib.{master, slave}

//While create a interconnect master as an io of the toplevel
class MasterBus(p : M2sParameters) extends Area{
  val node = InterconnectNode.master()
  val logic = Elab build new Area{
    node.m2s.parameters.load(p)
    node.m2s.setProposedFromParameters() //Here, we just ignore the negotiation phase
    node.s2m.supported.load(node.s2m.proposed)
    slave(node.bus)
  }
}

//While create a interconnect slave as an io of the toplevel
class SlaveBus(m2sSupport : M2sSupport, s2mParameters: S2mParameters = S2mParameters.none) extends Area{
  val node = InterconnectNode.slave()
  val logic = Elab build new Area {
    node.s2m.parameters.load(s2mParameters)
    node.s2m.setProposedFromParameters()
    node.m2s.supported.load(m2sSupport.copy(transfers = node.m2s.proposed.transfers.intersect(m2sSupport.transfers)))
    master(node.bus)
  }
}