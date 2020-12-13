package spinal.lib.memory.ram

import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class Ram1rw(mc: MemConfig, memName: String = "") extends Component with MemWrap{
  val io = new Bundle{
    val cs, wr = in Bool()
    val bwe    = mc.genBWE()
    val addr   = in UInt(mc.aw bits)
    val wdata  = in Bits(mc.dw bits)
    val rdata  = out Bits(mc.dw bits)

    val bist = if(mc.withBist) slave(Bist(mc.bistaw, mc.dw)) else null
    val scan = if(mc.withScan) slave(Scan(false, mc.withRepaire)) else null
  }
   noIoPrefix()
  val ram = mc.vendor.build(this)
}

