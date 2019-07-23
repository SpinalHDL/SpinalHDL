package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbUnburstify{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(
    lengthWidth = log2Up(inputParameter.byteCount),
    contextWidth = inputParameter.contextWidth + 2
  )
}

//TODO check inputParameter requirements
case class BmbUnburstify(inputParameter : BmbParameter) extends Component{
  val outputParameter = BmbUnburstify.outputParameter(inputParameter)
  val contextDropBit = inputParameter.contextWidth
  val contextLastBit = inputParameter.contextWidth+1

  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val doResult = Bool
  val addrIncrRange = (Math.min(Bmb.boundaryWidth-1, inputParameter.addressWidth - 1) downto 0)

  val buffer = new Area{
    val valid       = RegInit(False)
    val opcode      = Reg(Bits(1 bits))
    val source      = Reg(UInt(inputParameter.sourceWidth bits))
    val address     = Reg(UInt(inputParameter.addressWidth bits))
    val context     = Reg(Bits(inputParameter.contextWidth bits))
    val beat        = Reg(UInt(inputParameter.beatCounterWidth bits))
    val last        = beat === 1
    val addressIncr = Bmb.incr(address = address, p = inputParameter)
    val isWrite = Bmb.Cmd.Opcode.isWrite(opcode)

    when(io.output.cmd.fire) {
      beat := beat - 1
      address(addrIncrRange) := addressIncr(addrIncrRange)
      when(last){
        valid := False
      }
    }
  }

  val cmdTransferBeatCount = io.input.cmd.transferBeatCountMinusOne
  val requireBuffer = cmdTransferBeatCount =/= 0

  io.output.cmd.data := io.input.cmd.data
  io.output.cmd.mask := io.input.cmd.mask
  io.output.cmd.last := True

  //payload muxes
  when(buffer.valid) {
    io.output.cmd.source := buffer.source
    io.output.cmd.address := buffer.addressIncr
    io.output.cmd.opcode := buffer.opcode
    io.output.cmd.length := inputParameter.byteCount-1
    io.output.cmd.context(inputParameter.contextWidth-1 downto 0) := buffer.context
  } otherwise {
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.opcode := io.input.cmd.opcode
    when(requireBuffer) {
      io.output.cmd.address(inputParameter.wordRange) := 0
      io.output.cmd.length := inputParameter.byteCount-1 // (inputParameter.byteCount - io.input.cmd.length(inputParameter.wordRange)).resized
    } otherwise {
      io.output.cmd.length := io.input.cmd.length.resized
    }
    io.output.cmd.context(inputParameter.contextWidth-1 downto 0) := io.input.cmd.context
  }



  io.input.cmd.ready := False
  when(buffer.valid){
    io.output.cmd.valid := !(buffer.isWrite && !io.input.cmd.valid)
    io.input.cmd.ready  :=   buffer.isWrite && io.output.cmd.ready
    io.output.cmd.context(contextLastBit) := buffer.last
    io.output.cmd.context(contextDropBit) := buffer.isWrite
  }otherwise{
    io.input.cmd.ready  := io.output.cmd.ready
    io.output.cmd.valid := io.input.cmd.valid
    buffer.opcode := io.input.cmd.opcode
    buffer.source := io.input.cmd.source
    buffer.address := io.input.cmd.address
    buffer.context := io.input.cmd.context
    buffer.beat    := cmdTransferBeatCount
    io.output.cmd.context(contextDropBit) := io.input.cmd.isWrite
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
