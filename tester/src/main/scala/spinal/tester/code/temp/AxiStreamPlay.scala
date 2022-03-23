package spinal.tester.code.temp

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axis.Axi4Stream._
import spinal.lib.bus.amba4.axis._


class AxiStreamComponentTop extends Component {
  val io = new Bundle {
    val axis_s_in = slave(new Axi4Stream(Axi4StreamConfig(dataBytes = 4, useStrb = true)))
    val axis_s_sel = slave(new Axi4Stream(Axi4StreamConfig(dataBytes = 1)))
    val axis_m_out = master(new Axi4Stream(Axi4StreamConfig(dataBytes = 4, useStrb = true)))
  }

  val selStream = io.axis_s_sel.toStream(Bits(4 bit)).map(_._1).stage()

  val inStaged = io.axis_s_in.stage()

  val joinedStream = StreamJoin(Seq(inStaged, selStream))
  val strobedStream = joinedStream.translateWith({
    val newPayload: Axi4StreamBundle = inStaged.payload.clone
    newPayload := inStaged.payload
    newPayload.tstrb.allowOverride
    newPayload.tstrb := inStaged.tstrb & selStream.payload.takeLow(inStaged.tstrb.getBitsWidth)
    newPayload
  })

  io.axis_m_out << strobedStream

  Axi4SpecRenamer(io.axis_s_in)
  Axi4SpecRenamer(io.axis_s_sel)
  Axi4SpecRenamer(io.axis_m_out)
}

object AxiStreamPlay {
  def main(args: Array[String]) {
    SpinalVhdl(new AxiStreamComponentTop())
  }
}