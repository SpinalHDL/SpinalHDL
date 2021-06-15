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
  
  val inputCmd = io.input.cmd.halfPipe()

  val beatCounter = Reg(UInt(p.access.beatCounterWidth bits)) init(0)
  val beatLast = beatCounter === inputCmd.transferBeatCountMinusOne
  when(inputCmd.valid && io.output.ACK){
    beatCounter := beatCounter + 1
    when(inputCmd.ready && inputCmd.last){
      beatCounter := 0
    }
  }


  io.output.ADR := Bmb.addToAddress(inputCmd.address, beatCounter << log2Up(p.access.byteCount), p) >> log2Up(p.access.byteCount)
  io.output.CTI := inputCmd.last ? (inputCmd.first ? B"000" | B"111") | B"010"
  io.output.BTE :=  B"00"
  io.output.SEL := inputCmd.isWrite ? inputCmd.mask | io.output.SEL.getAllTrue
  io.output.WE  := inputCmd.isWrite
  io.output.DAT_MOSI := inputCmd.data

  inputCmd.ready := io.output.ACK && (inputCmd.isWrite || beatLast)
  io.output.CYC := inputCmd.valid
  io.output.STB := inputCmd.valid

  io.input.rsp.valid   := RegNext(inputCmd.valid && io.output.ACK && (inputCmd.isRead || beatLast)) init(False)
  io.input.rsp.data    := RegNext(io.output.DAT_MISO)
  io.input.rsp.source  := RegNext(inputCmd.source)
  io.input.rsp.context := RegNext(inputCmd.context)
  io.input.rsp.last    := RegNext(beatLast)
  io.input.rsp.setSuccess() //TODO
}


