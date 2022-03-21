package spinal.tester.code.temp

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axis.Axi4Stream._
import spinal.lib.bus.amba4.axis._


class AxiStreamComponentTop extends Component {
  val io = new Bundle {
    val data_in = slave(new Stream(Bits(32 bit)))
    val data_out = master(new Axi4Stream(Axi4StreamConfig(dataBytes = 4, useStrb = true, useKeep = false)))
  }

  io.data_in.stage().toAxi4Stream() >> io.data_out

  Axi4SpecRenamer(io.data_out)
}

object AxiStreamPlay {
  def main(args: Array[String]) {
    SpinalVhdl(new AxiStreamComponentTop())
  }
}