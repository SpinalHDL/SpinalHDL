package spinal.lib.memory.ram

import spinal.lib._
import spinal.core._

case class Rom(mc: MemConfig, initialContent: List[BigInt], memName: String = "") extends Component {
  val aw = log2Up(mc.depth)

  val io = new Bundle{
    val cs     = in Bool()
    val addr   = in UInt(mc.aw bits)
    val rdata  = out Bits(mc.dw bits)
  }

  val rom = mc.vendor.build(this)

  def genHex: Unit = {
    val name = s"rom_${mc.depth}x${mc.dw}_${memName}_init.dat"
    val content = initialContent.map(_.hexString(mc.dw))
    File.writeFile(name, content)
  }

  genHex
}
