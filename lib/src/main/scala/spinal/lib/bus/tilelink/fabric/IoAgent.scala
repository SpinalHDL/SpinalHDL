package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.bus.tilelink.{M2sParameters, M2sSupport, S2mParameters}
import spinal.lib.{master, slave}

//Will create a interconnect master as an io of the toplevel
class MasterBus(p : M2sParameters) extends Area{
  val node = Node.master()
  val logic = Fiber build new Area{
    node.m2s.parameters.load(p)
    node.m2s.setProposedFromParameters() //Here, we just ignore the negotiation phase
    node.s2m.supported.load(node.s2m.proposed)
    slave(node.bus)
  }
}

//Will create a interconnect slave as an io of the toplevel
class SlaveBus(m2sSupport : M2sSupport, s2mParameters: S2mParameters = S2mParameters.none) extends Area{
  val node = Node.slave()
  val logic = Fiber build new Area {
    node.m2s.supported.load(m2sSupport.copy(transfers = node.m2s.proposed.transfers.intersect(m2sSupport.transfers)))
    node.s2m.parameters.load(s2mParameters)
    node.s2m.setProposedFromParameters()
    master(node.bus)
  }
}

//Will create a interconnect slave as an io of the toplevel
class SlaveBusAny() extends Area{
  val node = Node.slave()
  val logic = Fiber build new Area {
    node.m2s.supported.load(node.m2s.proposed)
    node.s2m.none()
    master(node.bus)
  }
}