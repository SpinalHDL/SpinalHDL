package spinal.lib.bus.amba3.ahblite.sim.slave

import spinal.lib.sim.protocolSim.slave._

/** An AHB request
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
// Section 2.2 Master signals and Section 3.1 Basic transfers
case class AhbReq(
    HADDR: BigInt,
    HWRITE: Boolean,
    HSIZE: Int,
    HBURST: Int,
    HPROT: Int,
    HTRANS: Int,
    HMASTLOCK: Boolean,
    HREADY: () => Boolean,
    HWDATA: () => BigInt
) extends Request
    with AhbRelated {
  def isWrite: Boolean = HWRITE

  def addr: BigInt = HADDR

  def wdata: Option[BigInt] = if (isWrite) Some(HWDATA()) else None
}
