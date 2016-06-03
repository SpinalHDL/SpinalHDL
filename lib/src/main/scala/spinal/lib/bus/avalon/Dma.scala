package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._


case class AvalonReadDmaCmd(c: AvalonReadDmaConfig) extends Bundle {
  val startAt = UInt(c.addressWidth bit)
  val endAt = UInt(c.addressWidth bit)
  val burstSize = UInt(c.burstCountWidth bit)
}

case class AvalonReadDmaConfig( addressWidth : Int,
                                dataWidth : Int,
                                burstCountWidth : Int,
                                fifoSize : Int,
                                pendingReadMax : Int){
  def getAvalonConfig = AvalonMMConfig.bursted(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    burstCountWidth = burstCountWidth
  ).copy(
    maximumPendingReadTransactions = pendingReadMax
  )
}

class AvalonReadDma[T <: Data](dataType : T,c: AvalonReadDmaConfig) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (AvalonReadDmaCmd(c))
    val mem = master(AvalonMM(c.getAvalonConfig))
    val rsp = master Stream(dataType)
  }

  val active = RegInit(False)
  val counter = Reg(UInt(c.addressWidth bit))
  io.cmd.ready := False
  when(!active) {
    when(io.cmd.valid) {
      counter := io.cmd.startAt
      active := True
    }
  } otherwise {
    when(io.mem.fire) {
      counter := counter + io.cmd.burstSize * io.mem.config.dataByteCount
      when(counter === io.cmd.endAt) {
        active := False
        io.cmd.ready := True
      }
    }
  }

  io.mem.read := active
  io.mem.address := counter


  val rsp = Stream(dataType)
  rsp.valid := io.mem.readDataValid
  rsp.payload.assignFromBits(io.mem.readData)
  io.rsp << rsp.queue(c.fifoSize)

  val pendingRead = Reg(UInt())

}
