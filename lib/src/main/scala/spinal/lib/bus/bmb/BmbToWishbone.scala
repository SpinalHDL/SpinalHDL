package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._
object BmbToWishbone{
  def getWishboneConfig(p : BmbAccessParameter): WishboneConfig = WishboneConfig(
    addressWidth = p.addressWidth-log2Up(p.byteCount),
    dataWidth = p.dataWidth,
    selWidth = p.byteCount,
    useERR = true,
    useBTE = true,
    useCTI = true
  )
}

case class BmbToWishbone(p : BmbParameter) extends Component{
  val io = new Bundle {
    val input = slave(Bmb(p))
    val output = master(Wishbone(BmbToWishbone.getWishboneConfig(p.access)))
  }

  val beatCounter = Reg(UInt(p.access.beatCounterWidth bits)) init(0)
  val beatLast = beatCounter === io.input.cmd.transferBeatCountMinusOne
  when(io.input.cmd.valid && io.output.ACK){
    beatCounter := beatCounter + 1
    when(io.input.cmd.ready && io.input.cmd.last){
      beatCounter := 0
    }
  }


  io.output.ADR := Bmb.addToAddress(io.input.cmd.address, beatCounter << log2Up(p.access.byteCount), p) >> log2Up(p.access.byteCount)
  io.output.CTI := io.input.cmd.last ? (io.input.cmd.first ? B"000" | B"111") | B"010"
  io.output.BTE :=  B"00"
  io.output.SEL := io.input.cmd.isWrite ? io.input.cmd.mask | io.output.SEL.getAllTrue
  io.output.WE  := io.input.cmd.isWrite
  io.output.DAT_MOSI := io.input.cmd.data

  io.input.cmd.ready := io.output.ACK && (io.input.cmd.isWrite || beatLast)
  io.output.CYC := io.input.cmd.valid
  io.output.STB := io.input.cmd.valid

  io.input.rsp.valid   := RegNext(io.input.cmd.valid && io.output.ACK && (io.input.cmd.isRead || beatLast)) init(False)
  io.input.rsp.data    := RegNext(io.output.DAT_MISO)
  io.input.rsp.source  := RegNext(io.input.cmd.source)
  io.input.rsp.context := RegNext(io.input.cmd.context)
  io.input.rsp.last    := RegNext(beatLast)
  io.input.rsp.setSuccess() //TODO
}


