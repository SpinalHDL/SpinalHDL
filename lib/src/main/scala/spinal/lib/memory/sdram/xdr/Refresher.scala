package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Refresher(cp : CoreParameter) {
  val io = new Bundle {
    val config = in(CoreConfig(cp))
    val cmd = master(Event)
  }

  val value = Reg(UInt(cp.timingWidth bits)) init(0)
  val hit = value === 0
  value := value - 1
  when(hit) {
    value := io.config.REF
  }

  val pending = RegInit(False) clearWhen(io.cmd.ready) setWhen(hit)
  io.cmd.valid := pending
}
