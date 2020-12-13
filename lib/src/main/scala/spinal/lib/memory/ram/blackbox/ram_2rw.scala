package spinal.lib.memory.ram.blackbox

import spinal.lib.memory.ram._
import spinal.core._

class ram_2rw(wrap: Ram2rw) extends MemBlackBox {
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_1rw_${mc.depth}x${mc.dw}_${mc.maskName}_${memName}_wrap")

  val io = new Bundle {
    val clk_a    = in Bool
    val addr_a   = in UInt(mc.aw bits)
    val wdata_a  = in Bits(mc.dw bits)
    val ce_n_a   = in Bool()
    val we_n_a   = in Bool()
    val bwe_n_a  = mc.genBWE
    val rdata_a  = out Bits(mc.dw bits)

    val clk_b    = in Bool
    val addr_b   = in UInt(mc.aw bits)
    val wdata_b  = in Bits(mc.dw bits)
    val ce_n_b   = in Bool()
    val we_n_b   = in Bool()
    val bwe_n_b  = mc.genBWE
    val rdata_b  = out Bits(mc.dw bits)
  }

  val cda = new ClockingArea(ClockDomain(io.clk_a))
  val cdb = new ClockingArea(ClockDomain(io.clk_b))

  def Build() = {
    this.io.clk_a    := wrap.io.Aclk
    this.io.addr_a   := wrap.io.Aaddr
    this.io.wdata_a  := wrap.io.Awdata
    this.io.ce_n_a   := ~wrap.io.Acs
    this.io.we_n_a   := ~wrap.io.Awr

    this.io.clk_b    := wrap.io.Bclk
    this.io.addr_b   := wrap.io.Baddr
    this.io.wdata_b  := wrap.io.Bwdata
    this.io.ce_n_b   := ~wrap.io.Bcs
    this.io.we_n_b   := ~wrap.io.Bwr

    if(mc.needBWE) {
      this.io.bwe_n_a := wrap.io.Abwe
      this.io.bwe_n_b := wrap.io.Bbwe
    }

    wrap.io.Ardata  := this.io.rdata_a
    wrap.io.Brdata  := this.io.rdata_b
    this
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)
}
