package spinal.lib.bus.amba3.ahblite.sim

/** Possible values of HRESP
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Hresp {
  // Table 5-1 HRESP signal in section 5.1 Slave transfer responses

  /** The transfer has either completed successfully or additional cycles are
    * required for the slave to complete the request.
    */
  val OKAY = false

  /** An error has occurred during the transfer */
  val ERROR = true
}
