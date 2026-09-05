package spinal.lib.bus.amba3.ahblite.sim

/** Possible values of HWRITE
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Hwrite {
  // Table 2-2 Master signals in section 2.2 Master signals

  /** When HIGH this signal indicates a write transfer */
  val READ = false

  /** When LOW this signal indicates a read transfer */
  val WRITE = true
}
