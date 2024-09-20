package spinal.lib.bus.amba3.ahblite.sim.slave

import spinal.lib.sim.protocolSim.slave._
import spinal.lib.bus.amba3.ahblite.sim._

/** Response to a transfer
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
// Section 2.3 Slave signals
// Default values from section 5.1.1
case class AhbResp(
    val HREADY: Boolean = true,
    val HRESP: Boolean = Hresp.OKAY,
    val HRDATA: BigInt = 0
)

