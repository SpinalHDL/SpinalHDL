package spinal.lib.bus.pcie

import spinal.core._
import spinal.lib._

case class TlpConfig(
    dwCount: Int = 256/32,
    useData: Boolean = false,
    useStrb: Boolean = false,
    useSeq: Boolean = false,
    useError: Boolean = false,
    withBarFunc: Boolean = false
)

case class Tlp(config: TlpConfig) extends Bundle {
  val data = config.useData generate Bits(config.dwCount * 32 bits)
  val strb = config.useStrb generate Bits(config.dwCount bits)
  val hdr = Bits(128 bits)
  val seq = config.useSeq generate UInt(6 bits)
  val bar_id = config.withBarFunc generate UInt(3 bits)
  val func_num = config.withBarFunc generate UInt(8 bits)
  val error = config.useError generate UInt(4 bits)
}

