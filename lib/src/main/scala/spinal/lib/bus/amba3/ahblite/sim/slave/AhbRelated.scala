package spinal.lib.bus.amba3.ahblite.sim.slave

import spinal.lib.sim.protocolSim.slave._

/** Extends ProtocolRelated for AHB
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
trait AhbRelated extends ProtocolRelated {
  type Resp = AhbResp
  type Req = AhbReq
}
