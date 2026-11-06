package spinal.lib.bus.amba3.ahblite.sim

import spinal.lib.bus.amba3.ahblite.AhbLite3Config
import spinal.core.ClockDomain

/** Checks that things match specifications
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object isOk {

  /** Checks that the configuration matches the specifications */
  def apply(cfg: AhbLite3Config): Boolean = {
    // Section 2.2 Master signals
    cfg.addressWidth == 32 &&
    // Section 6.2 Data bus width
    List(8, 16, 32, 64, 128, 256, 512, 1024).contains(cfg.dataWidth)
  }

  /** Checks that the clock domain matches the specifications */
  def apply(cd: ClockDomain): Boolean = {
    // Subsection 7.1.1 Clock
    cd.config.clockEdge == spinal.core.RISING &&
    // Subsection 7.1.2 Reset
    cd.config.resetActiveLevel == spinal.core.LOW
  }
}
