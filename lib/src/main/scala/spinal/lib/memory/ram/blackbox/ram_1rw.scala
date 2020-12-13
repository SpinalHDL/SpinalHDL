package spinal.lib.memory.ram.blackbox

import spinal.lib.memory.ram._
import spinal.core._


class ram_1rw(wrap: Ram1rw) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_1rw_${mc.depth}x${mc.dw}_${mc.maskName}_${memName}_wrap")

  val io = new Bundle {
    val clk      = in Bool
    val addr     = in UInt(mc.aw bits)
    val wdata    = in Bits(mc.dw bits)
    val ce       = in Bool()
    val we       = in Bool()
    val bwe      = mc.genBWE
    val rdata    = out Bits(mc.dw bits)
  }
  //  mapClockDomain(clock = io.clk)
  val cd  = ClockDomain(io.clk)

  def Build() = {
    wrap.clockDomain.setSyncWith(cd)
    this.io.clk    := wrap.clockDomain.readClockWire
    this.io.addr   := wrap.io.addr
    this.io.ce     := wrap.io.cs
    this.io.we     := wrap.io.wr
    if(mc.needBWE){
      this.io.bwe  := wrap.io.bwe
    }
    this.io.wdata  := wrap.io.wdata
    wrap.io.rdata  := this.io.rdata
    this
  }

  noIoPrefix()

  val mem = new Mem(Bits(mc.dw bits), mc.depth)
  val ca  = new ClockingArea(cd) {
    mem.write(
      address = io.addr,
      data = io.wdata,
      enable = io.we
    )

    io.rdata := mem.readSync(
      address = io.addr,
      enable = (io.ce && !io.we)
    )
  }
}
