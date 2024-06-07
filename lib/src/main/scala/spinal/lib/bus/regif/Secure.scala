package spinal.lib.bus.regif

import spinal.core._

trait Secure{
  val wrbit: Bool = null
  val rdbit: Bool = null
}

/*
* AS: Always-Secure(read and write all sec)
* NS: Non-Secure(read and write all non-sec)
* MS: Mix-Secure(read and write securetiy seperate)
* CS: Config-Secure(which need signal_bit indicate security status)
* */
object Secure {
  case class MS(wrsec: Boolean,  rdsec: Boolean) extends Secure
  case class CS(override val wrbit: Bool, override val rdbit: Bool) extends Secure
  object AS{ def apply() = MS(true, true)}
  object NS{ def apply() = MS(false, false)}
  object CS{
    /* read and write configurable Security bit use one*/
    def apply(tzpc: Bool) = new CS(tzpc, tzpc)
  }
}
