package spinal.lib.bus.amba3.ahblite.sim

import scala.language.implicitConversions

/** Possible values of HMASTLOCK
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Hmastlock {
  /** Locked access */
  val HIGH = true

  /** Locked access */
  val high = Hmastlock(HIGH)

  /** Not locked access */
  val LOW = false

  /** Not locked access */
  val low = Hmastlock(LOW)

  /** Hburst can be implicitly converted to an integer: HBURST */
  implicit def hburst2boolean(hmastlock: Hmastlock): Boolean = hmastlock.value
}

case class Hmastlock(value: Boolean)
