package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{M2sParameters, M2sSupport, S2mAgent, S2mParameters, S2mTransfers, SizeRange}
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
class SlaveBusAny(sinkCount : Int = 8) extends Area{
  val node = Node.slave()
  val logic = Fiber build new Area {
    node.m2s.supported.load(node.m2s.proposed)
    node.m2s.parameters.withBCE match {
      case false => node.s2m.none()
      case true => node.s2m forceParameters S2mParameters(
        List(
          S2mAgent(
            name = null,
            sinkId = SizeMapping(0, sinkCount),
            emits = S2mTransfers(
              probe = node.m2s.parameters.emits.acquireB mincover node.m2s.parameters.emits.acquireT
            )
          )
        )
      )
    }
    master(node.bus)
  }
}
