package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

/**
 * Created by PIC on 30.05.2015.
 */
case class AxiLite4SimpleReadDmaCmd(axiLiteConfig: AxiLite4Config) extends Bundle{
  val offset = UInt(axiLiteConfig.addressWidth bit)
  val endAt = UInt(axiLiteConfig.addressWidth bit)
}

class AxiLite4SimpleReadDma(axiLiteConfig: AxiLite4Config) extends Component {
  val io = new Bundle {
    val run = slave Stream (AxiLite4SimpleReadDmaCmd(axiLiteConfig))
    val axi = master(AxiLite4(axiLiteConfig))
    val read = master Stream (Bits(axiLiteConfig.dataWidth bit))
  }

  val active = RegInit(False)
  val counter = Reg(UInt(axiLiteConfig.addressWidth bit))
  io.run.ready := False
  when(!active) {
    when(io.run.valid) {
      counter := io.run.offset
      active := True
    }
  } otherwise {
    when(io.axi.readCmd.ready) {
      counter := counter + axiLiteConfig.bytePerWord
      when(counter === io.run.endAt) {
        active := False
        io.run.ready := True
      }
    }
  }

  io.axi.readCmd.valid := active
  io.axi.readCmd.addr := counter
  io.axi.readCmd.setUnprivileged

  io.read.translateFrom(io.axi.readRsp)((to,from) => {
    to := from.data
  })
}
