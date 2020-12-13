package spinal.lib.memory.ram

import spinal.core._
import spinal.lib.slave

case class Ram1r1w(mc: MemConfig, memName: String = "") extends Component with MemWrap{
  val io = new Bundle{
    val rd    = in Bool()
    val raddr = in UInt(mc.aw bits)
    val rdata = out Bits(mc.dw bits)

    val wr    = in Bool()
    val bwe   = mc.genBWE()
    val waddr = in UInt(mc.aw bits)
    val wdata = in Bits(mc.dw bits)
    val bist  = if(mc.withBist) slave(Bist(mc.bistaw, mc.dw)) else null
    val scan  = if(mc.withBist) slave(Scan(true, mc.withRepaire)) else null
  }

  val ram = mc.vendor.build(this)
}

