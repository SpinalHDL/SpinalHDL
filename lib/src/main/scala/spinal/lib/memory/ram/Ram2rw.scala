package spinal.lib.memory.ram

import spinal.core._

case class Ram2rw(mc: MemConfig, memName: String = "") extends Component with MemWrap{
  val io = new Bundle{
    val Aclk  = in Bool()
    val Awr, Acs  = in Bool()
    val Abwe  = mc.genBWE
    val Aaddr = in UInt()
    val Awdata = in Bits()
    val Ardata = out Bits()

    val Bclk  = in Bool()
    val Bwr, Bcs = in Bool()
    val Bbwe  = mc.genBWE
    val Baddr = in UInt()
    val Bwdata = in Bits()
    val Brdata = out Bits()
  }

  noIoPrefix()
  val ram = mc.vendor.build(this)
}
