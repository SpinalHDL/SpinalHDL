package spinal.lib.bus.regif

import spinal.core._

trait Secure{
  val signal: Bool = null
}

object Secure {
  object NS extends Secure
  object S  extends Secure
  case class CS(override val signal: Bool) extends Secure
}
