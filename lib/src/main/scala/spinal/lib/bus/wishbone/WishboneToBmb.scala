package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.wishbone._

object WishboneToBmb{
  def getBmbRequirements(c : WishboneConfig) = BmbAccessParameter(
    addressWidth = c.addressWidth + log2Up(c.dataWidth/8),
    dataWidth = c.dataWidth
  ).addSources(1, BmbSourceParameter(
    contextWidth = 0,
    lengthWidth = log2Up(c.dataWidth/8),
    alignment = BmbParameter.BurstAlignement.LENGTH
  ))
}

case class WishboneToBmb(c : WishboneConfig) extends Component{
  val p = WishboneToBmb.getBmbRequirements(c)
  val io = new Bundle{
    val input = slave(Wishbone(c))
    val output = master(Bmb(p))
  }

  io.output.cmd.address := io.input.ADR << log2Up(c.dataWidth/8)
  io.output.cmd.opcode := (io.input.WE ? B(Bmb.Cmd.Opcode.WRITE) | B(Bmb.Cmd.Opcode.READ))
  io.output.cmd.data := io.input.DAT_MOSI
  io.output.cmd.mask := (if(c.useSEL) io.input.SEL else B((1 << p.maskWidth)-1))
  io.output.cmd.length := c.dataWidth/8-1
  io.output.cmd.last := True
  if(c.useSTALL) {
    io.output.cmd.valid := io.input.CYC && io.input.STB
    io.input.STALL := io.output.cmd.isStall
  } else {
    val fired = RegInit(False) setWhen(io.output.cmd.fire) clearWhen(io.output.rsp.fire)
    io.output.cmd.valid := io.input.CYC && io.input.STB && !fired
  }

  io.input.ACK := io.output.rsp.fire
  io.input.DAT_MISO := io.output.rsp.data
  io.output.rsp.ready := True
}
