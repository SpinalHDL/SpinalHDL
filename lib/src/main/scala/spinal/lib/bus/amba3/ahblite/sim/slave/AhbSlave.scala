package spinal.lib.bus.amba3.ahblite.sim.slave

import spinal.lib.sim.protocolSim.slave._
import spinal.lib.bus.amba3.ahblite.sim._

/** Extends ProtocolSlave for AHB
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
trait AhbSlave extends ProtocolSlave with AhbRelated {
  // Section 5.1.1 (this is the default)
  val ready: Resp = AhbResp(HREADY = true, HRESP = Hresp.OKAY)

  // Section 5.1.1
  def done(rdata: Data): Resp = AhbResp(HRDATA = rdata)

  // Section 5.1.2
  val pending: Resp = AhbResp(HREADY = false)

  // Section 5.1.3
  def error1: AhbResp = AhbResp(HRESP = Hresp.ERROR, HREADY = false)
  def error2: AhbResp = AhbResp(HRESP = Hresp.ERROR, HREADY = true)

  /** Builds an AHB error
    *
    * @return
    *   Handler for 2-cycle AHB error response
    */
  val errorBuilder: HandlerBuilder = { req =>
    var firstCycle = true;
    { () =>
      val resp = if (firstCycle) error1 else error2
      firstCycle = false
      resp
    }
  }
}
