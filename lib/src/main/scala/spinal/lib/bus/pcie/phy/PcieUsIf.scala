package spinal.lib.bus.pcie.phy

import spinal.core._
import spinal.lib._

case class PcieUsAxisIfConfig(dataWidth: Int, userWidth: Int)

case class PcieUsAxisIf(config: PcieUsAxisIfConfig) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val keep = Bits(config.dataWidth/32 bits)
  val user = Bits(config.userWidth bits)
}

object PcieUsAxisIfSpecRenamer {
  def apply(axis: Stream[Fragment[PcieUsAxisIf]]) = {
    axis.payload.setPartialName("").fragment.setPartialName("")
    axis.data.setPartialName("tdata")
    axis.keep.setPartialName("tkeep")
    axis.user.setPartialName("tuser")
    axis.valid.setPartialName("tvalid")
    axis.ready.setPartialName("tready")
    axis.last.setPartialName("tlast")
    axis
  }
}