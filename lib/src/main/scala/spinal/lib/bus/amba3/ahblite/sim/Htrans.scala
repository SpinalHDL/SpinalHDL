package spinal.lib.bus.amba3.ahblite.sim

import scala.language.implicitConversions

/** Possible values of HTRANS
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Htrans {
  // Table 3-1 Transfer type encoding in section 3.2 Transfer types

  /** Indicates that no data transfer is required */
  val IDLE = 0

  /** Indicates that no data transfer is required */
  val idle = Htrans(IDLE)

  /** Enables masters to insert idle cycles in the middle of a burst */
  val BUSY = 1

    /** Enables masters to insert idle cycles in the middle of a burst */
  val busy = Htrans(BUSY)

  /** Indicates a single transfer or the first transfer of a burst */
  val NONSEQ = 2

  /** Indicates a single transfer or the first transfer of a burst */
  val nonseq = Htrans(NONSEQ)

  /** The remaining transfers in a burst are SEQUENTIAL and the address is
    * related to the previous transfer.
    */
  val SEQ = 3

  /** The remaining transfers in a burst are SEQUENTIAL and the address is
    * related to the previous transfer.
    */
  val seq = Htrans(SEQ)

  /** Htrans can be implicitly converted to Int */
  implicit def htrans2int(htrans: Htrans): Int = htrans.value
}

case class Htrans(value: Int)
