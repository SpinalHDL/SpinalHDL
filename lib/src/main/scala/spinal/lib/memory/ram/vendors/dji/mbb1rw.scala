package spinal.lib.memory.ram.vendors.dji

import spinal.lib.memory.ram._
import spinal.core._

class mbb1rw(wrap: Ram1rw) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_1rw_${mc.depth}x${mc.dw}_${mc.maskName}_${memName}_wrap")

  val io = new Bundle {
    val clk     = in Bool
    val addr    = in UInt(mc.aw bits)
    val wdata   = in Bits(mc.dw bits)
    val ce_n    = in Bool()
    val we_n    = in Bool()
    val bwe_n   = mc.genBWE
    val rdata   = out Bits(mc.dw bits)
    val ctrl_bus = flyWireFromTop(Bits(128 bits), "ctrl_bus")
  }

  noIoPrefix()
  val cd  = ClockDomain(io.clk)

  def Build(): MemBlackBox = {
    wrap.clockDomain.setSyncWith(cd)
    this.io.clk     := wrap.clockDomain.readClockWire
    this.io.addr    :=  wrap.io.addr
    this.io.ce_n    := ~wrap.io.cs
    this.io.we_n    := ~wrap.io.wr
    if(mc.needBWE){
      this.io.bwe_n := ~wrap.io.bwe
    }
    this.io.wdata  := wrap.io.wdata
    wrap.io.rdata  := this.io.rdata
    this
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)

  val ca  = new ClockingArea(cd) {
    mem.write(
      address = io.addr,
      data = io.wdata,
      enable = ~io.we_n
    )

    io.rdata := mem.readSync(
      address = io.addr,
      enable = (!io.ce_n && io.we_n)
    )
  }

  override type RefOwnerType = this.type
}
