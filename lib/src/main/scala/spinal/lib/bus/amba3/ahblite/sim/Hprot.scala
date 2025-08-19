package spinal.lib.bus.amba3.ahblite.sim

import scala.language.implicitConversions

/** Helper to build and check HPROT values
  *
  * {{{
  * val p = Hprot.dataAccess.withUserAccess.withBufferable
  * assert(p is Hprot.Bufferable)
  * assert(!(p is Hprot.NonBufferable))
  * }}}
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Hprot {

  // Table 3-4 Protection signal encoding
  type Property = (Int, Boolean)
  val iDataOpcode = 0
  val iPriviledged = 1
  val iBufferable = 2
  val iCacheable = 3
  val OpcodeFetch = (iDataOpcode, false)
  val DataAccess = (iDataOpcode, true)
  val UserAccess = (iPriviledged, false)
  val PriviledgedAccess = (iPriviledged, true)
  val NonBufferable = (iBufferable, false)
  val Bufferable = (iBufferable, true)
  val NonCacheable = (iCacheable, false)
  val Cacheable = (iCacheable, true)

  // Section 3.7 Protection control, Note
  val defaultValue = 3
  private def build(what: Property): Hprot = new Hprot(mask(what)(defaultValue))

  /** Start creating an opcode fetch */
  def opcodeFetch: Hprot = build(OpcodeFetch)

  /** Start creating a data access */
  def dataAccess: Hprot = build(DataAccess)

  /** Check if hprot matches a property */
  def is(what: Property)(hprot: Int): Boolean = {
    val mask = 1 << what._1
    val value = if (what._2) hprot else ~hprot
    (value & mask) != 0
  }

  /** Add a property to an hprot
    *
    * @see
    *   opcodeFetch
    * @see
    *   dataAccess
    */
  def mask(what: Property)(hprot: Int): Int = {
    val mask = 1 << what._1
    if (what._2) hprot | mask
    else hprot & ~mask
  }

  /** Implicitly converts Hprot to its value
    *
    * @param hprot
    *   built Hprot
    * @return
    *   HPROT
    */
  implicit def hprot2oInt(hprot: Hprot): Int = hprot.value

}

/** Helper to build and check HPROT values
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
case class Hprot(value: Int) {
  import Hprot._

  /** Check if hprot matches a property */
  def is(what: Property): Boolean = Hprot.is(what)(this)

  def withUserAccess: Hprot = new Hprot(mask(UserAccess)(this))
  def withPriviledgedAccess: Hprot = new Hprot(mask(PriviledgedAccess)(this))
  def withNonBufferable: Hprot = new Hprot(mask(NonBufferable)(this))
  def withBufferable: Hprot = new Hprot(mask(Bufferable)(this))
  def withNonCacheable: Hprot = new Hprot(mask(NonCacheable)(this))
  def withCacheable: Hprot = new Hprot(mask(Cacheable)(this))
}
