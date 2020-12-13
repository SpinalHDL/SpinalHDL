package spinal.lib.memory.ram.vendors.umc

import spinal.lib.memory.ram._
import spinal.core._

class mbbrom(wrap: Rom) extends MemBlackBox {
  val mc = wrap.mc
  val initialContent = wrap.initialContent
  val memName = wrap.memName

  this.setDefinitionName(s"mem_rom_${mc.depth}x${mc.dw}_${memName}_wrap")
  val io = new Bundle{
    val CLK = in Bool
    val A   = in UInt(mc.aw bits)
    val CEN = in Bool
    val Q   = out Bits(mc.dw bits)
  }

  def Build() = {
    this.io.A    :=  wrap.io.addr
    this.io.CEN  := ~wrap.io.cs
    wrap.io.rdata  := this.io.Q
    this
  }
  noIoPrefix()
  mapClockDomain(clock = io.CLK)

  val mem =  Mem(Bits(mc.dw bits), initialContent.map(B(_, mc.dw bits)))
  io.Q := mem.readSync(io.A)
}
