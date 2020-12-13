package spinal.lib.memroy.ram.vendors.dji

import spinal.lib.memory.ram._
import spinal.core._

class mbbrom(wrap: Rom) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_rom_${mc.depth}x${mc.dw}_${memName}_wrap")
  val io = new Bundle {
    val clk      = in Bool
    val addr     = in UInt(mc.aw bits)
    val ce_n     = in Bool()
    val rdata    = out Bits(mc.dw bits)
    val ctrl_bus = flyWireFromTop(Bits(128 bits), "ctrl_bus")
  }
  noIoPrefix()

  val cd = ClockDomain(io.clk)

  def Build() = {
    wrap.clockDomain.setSyncWith(cd)
    this.io.clk   := wrap.clockDomain.readClockWire
    this.io.addr  := wrap.io.addr
    this.io.ce_n  := ~wrap.io.cs
    wrap.io.rdata := this.io.rdata
    this
  }

  val rom =  Mem(Bits(mc.dw bits), wrap.initialContent.map(B(_, mc.dw bits)))
  val ca  = new ClockingArea(cd) {
    io.rdata := rom.readSync(io.addr)
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)
}
