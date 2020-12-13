package spinal.lib.memory.ram.blackbox

import spinal.lib.memory.ram._
import spinal.core._

class rom_bb(wrap: Rom) extends MemBlackBox {
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_rom_${mc.depth}x${mc.dw}_${memName}_wrap")
  val io = new Bundle{
    val clk = in Bool
    val addr = in UInt(mc.aw bits)
    val ce_n = in Bool
    val rdata = out Bits(mc.dw bits)
  }

  def Build() = {
    this
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)
}
