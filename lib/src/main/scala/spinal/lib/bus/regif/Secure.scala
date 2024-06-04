package spinal.lib.bus.regif

import spinal.core._

trait Secure{
  val wrbit: Bool = null
  val rdbit: Bool = null
}

/*
* AS: Always-Secure
* NS: Non-Secure
* CS: Config-Secure(which need signal_bit indicate security status)
*
* which all can be defined separately on write and read
*
* */
object Secure {
  case class MS(wrsec: Boolean,  rdsec: Boolean) extends Secure
  case class CS(override val wrbit: Bool, override val rdbit: Bool) extends Secure
  object AS{ def apply() = MS(true, true)}
  object NS{ def apply() = MS(false, false)}
  object CS{
    def apply(tzpc: Bool) = new CS(tzpc, tzpc)
  }
}
