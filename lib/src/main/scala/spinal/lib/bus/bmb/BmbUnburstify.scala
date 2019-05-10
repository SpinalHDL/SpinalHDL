package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

case class BmbUnburstify(p : BmbParameter) extends Component{
  val outputParameter = p.copy(lengthWidth = log2Up(p.byteCount), contextWidth = p.contextWidth + 2)
  val contextDropBit = p.contextWidth
  val contextLastBit = p.contextWidth+1

  val io = new Bundle {
    val input = Bmb(p)
    val output = Bmb(outputParameter)
  }

  val doResult = Bool
  val addrIncrRange = (Math.min(11, p.addressWidth - 1) downto 0)

  val buffer = new Area{
    val valid       = RegInit(False)
    val opcode      = Reg(Bits(1 bits))
    val source      = Reg(UInt(p.addressWidth bits))
    val address     = Reg(UInt(p.addressWidth bits))
    val context     = Reg(Bits(p.contextWidth bits))
    val beat        = Reg(UInt(p.beatCounterWidth bits))
    val last        = beat === 1

    when(io.output.cmd.fire) {
      beat := beat - 1
      address(addrIncrRange) := Bmb.incr(address = address, p = p)(addrIncrRange)
      when(last){
        valid := False
      }
    }
  }


  io.output.cmd.data := io.input.cmd.data
  io.output.cmd.mask := io.input.cmd.mask
  io.output.cmd.last := True

  //payload muxes
  when(buffer.valid && buffer.opcode === Bmb.Cmd.Opcode.READ) {
    io.output.cmd.source := buffer.source
    io.output.cmd.address := buffer.address
    io.output.cmd.opcode := Bmb.Cmd.Opcode.READ
    io.output.cmd.length := p.byteCount
    io.output.cmd.context(p.contextWidth-1 downto 0) := buffer.context
  } otherwise {
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.length := io.input.cmd.length.max(p.byteCount)
    io.output.cmd.context(p.contextWidth-1 downto 0) := io.input.cmd.context
  }



  io.input.cmd.ready := False
  when(buffer.valid){
    io.output.cmd.valid := !(buffer.opcode === Bmb.Cmd.Opcode.WRITE && !io.input.cmd.valid)
    io.input.cmd.ready  :=   buffer.opcode === Bmb.Cmd.Opcode.WRITE && io.output.cmd.ready
    io.output.cmd.context(contextLastBit) := buffer.last
    io.output.cmd.context(contextDropBit) := buffer.opcode === Bmb.Cmd.Opcode.WRITE
  }otherwise{
    io.input.cmd.ready  := io.output.cmd.ready
    io.output.cmd.valid := io.input.cmd.valid
    buffer.opcode := io.input.cmd.opcode
    buffer.source := io.input.cmd.source
    buffer.address := io.input.cmd.address
    buffer.context := io.input.cmd.context
    buffer.beat := io.input.cmd.length >> log2Up(p.byteCount)
    io.output.cmd.context(contextDropBit) := io.input.cmd.opcode === Bmb.Cmd.Opcode.WRITE

    val requireBuffer = io.input.cmd.length < p.byteCount
    io.output.cmd.context(contextLastBit) := !requireBuffer
    buffer.valid := requireBuffer && io.output.cmd.fire
  }

  io.input.rsp.valid := io.output.rsp.valid && (io.output.rsp.context(contextLastBit) || !io.output.rsp.context(contextDropBit))
  io.input.rsp.last := io.output.rsp.context(contextLastBit)
  io.input.rsp.source := io.output.rsp.source
  io.input.rsp.opcode := io.output.rsp.opcode
  io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := io.output.rsp.context.resized
  io.output.rsp.ready := io.input.rsp.ready
}
