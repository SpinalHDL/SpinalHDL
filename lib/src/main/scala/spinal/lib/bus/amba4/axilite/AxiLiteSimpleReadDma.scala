package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

/**
 * Created by PIC on 30.05.2015.
 */
case class AxiLiteSimpleReadDmaCmd(axiLiteConfig: AxiLiteConfig) extends Bundle{
  val offset = UInt(axiLiteConfig.addressWidth bit)
  val endAt = UInt(axiLiteConfig.addressWidth bit)
}

class AxiLiteSimpleReadDma(axiLiteConfig: AxiLiteConfig) extends Component {
  val io = new Bundle {
    val run = slave Stream (AxiLiteSimpleReadDmaCmd(axiLiteConfig))
    val axi = master(AxiLiteReadOnly(axiLiteConfig))
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
      counter := counter + axiLiteConfig.dataByteCount
      when(counter === io.run.endAt) {
        active := False
        io.run.ready := True
      }
    }
  }

  io.axi.readCmd.valid := active
  io.axi.readCmd.addr := counter
  io.axi.readCmd.setUnprivileged

  io.read.translateFrom(io.axi.readData)((to,from) => {
    to := from.data
  })
}
