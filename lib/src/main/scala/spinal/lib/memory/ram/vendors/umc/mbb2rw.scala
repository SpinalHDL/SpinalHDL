package spinal.lib.memory.ram.vendors.umc

import spinal.lib.memory.ram._
import spinal.core._

class mbb2rw(wrap: Ram2rw) extends MemBlackBox {
  val mc = wrap.mc
  val memName = wrap.memName

  this.setDefinitionName(s"Mwrapper_srdp${mc.depth}x${mc.dw}m2b1w1")
  def Build() = {
    this
  }
  val mem = new Mem(Bits(mc.dw bits), mc.depth)
}
