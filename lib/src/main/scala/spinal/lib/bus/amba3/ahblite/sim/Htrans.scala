package spinal.lib.bus.amba3.ahblite.sim

/** Possible values of HTRANS
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Htrans {
  // Table 3-1 Transfer type encoding in section 3.2 Transfer types

  /** Indicates that no data transfer is required */
  val IDLE = 0

  /** Enables masters to insert idle cycles in the middle of a burst */
  val BUSY = 1

  /** Indicates a single transfer or the first transfer of a burst */
  val NONSEQ = 2

  /** The remaining transfers in a burst are SEQUENTIAL and the address is
    * related to the previous transfer.
    */
  val SEQ = 3
}
