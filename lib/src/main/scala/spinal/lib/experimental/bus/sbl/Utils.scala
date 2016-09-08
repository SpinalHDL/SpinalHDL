package spinal.lib.experimental.bus.sbl

import spinal.core._
import spinal.lib._
import spinal.lib.experimental.bus.sbl._


case class SblReadDmaCmd(config: SblConfig) extends Bundle{
  val offset = UInt(config.addressWidth bit)
  val endAt = UInt(config.addressWidth bit)
}

class SblReadDma(hblConfig: SblConfig) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (SblReadDmaCmd(hblConfig))
    val sblReadCmd = master Stream(SblReadCmd(hblConfig))
  }

  val active = RegInit(False)
  val counter = Reg(UInt(hblConfig.addressWidth bit))
  io.cmd.ready := False
  when(!active) {
    when(io.cmd.valid) {
      counter := io.cmd.offset
      active := True
    }
  } otherwise {
    when(io.sblReadCmd.ready) {
      counter := counter + 1
      when(counter === io.cmd.endAt) {
        active := False
        io.cmd.ready := True
      }
    }
  }

  io.sblReadCmd.valid := active
  io.sblReadCmd.address := counter
}
