package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._


//Todo use less ressource depending parameters
case class BmbUpSizerBridge(inputParameter: BmbParameter,
                            outputParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val input  = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val ratio = outputParameter.dataWidth / inputParameter.dataWidth
  val selRange = (outputParameter.wordRangeLength - 1 downto inputParameter.wordRangeLength)
  assert(ratio > 1)

  case class OutputContext() extends Bundle {
    val context = Bits(inputParameter.contextWidth bits)
    val source = UInt(inputParameter.sourceWidth bits)
    val selStart = inputParameter.canRead generate UInt(log2Up(ratio) bits)
    val selEnd = inputParameter.canRead generate UInt(log2Up(ratio) bits)
  }

  val cmdArea = new Area {
    val context = OutputContext()
    context.context := io.input.cmd.context
    context.source := io.input.cmd.source
    if (inputParameter.canRead) {
      context.selStart := io.input.cmd.address(selRange)
      context.selEnd := (io.input.cmd.address(selRange) + io.input.cmd.transferBeatCountMinusOne).resized
      if (inputParameter.canWrite) when(io.input.cmd.isWrite) {
        context.selEnd := io.input.cmd.address(selRange)
      }
    }

    io.output.cmd.valid := io.input.cmd.valid
    io.output.cmd.last := io.input.cmd.last
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.length := io.input.cmd.length
    io.output.cmd.context := B(context)

    if (!inputParameter.canWrite) {
      io.input.cmd.ready := io.output.cmd.ready
    }

    val writeLogic = inputParameter.canWrite generate new Area {
      val dataRegs = Vec(Reg(Bits(inputParameter.dataWidth bits)), ratio - 1)
      val maskRegs = Vec(Reg(Bits(inputParameter.dataWidth / 8 bits)), ratio - 1)

      val sel = Reg(UInt(log2Up(ratio) bits))
      when(io.input.cmd.fire && io.input.cmd.first) {
        sel := io.input.cmd.address(selRange)
      }

      val outputData = io.output.cmd.data.subdivideIn(ratio slices)
      val outputMask = io.output.cmd.mask.subdivideIn(ratio slices)
      for (i <- 0 until ratio) {
        outputData(i) := io.input.cmd.data
        outputMask(i) := io.input.cmd.mask
        if (i != ratio - 1) when(!io.input.cmd.first && sel =/= i) {
          outputData(i) := dataRegs(i)
          outputMask(i) := maskRegs(i)
        } otherwise {
          dataRegs(i) := io.input.cmd.data
          maskRegs(i) := io.input.cmd.mask
        }
      }
      io.input.cmd.ready := io.output.cmd.ready
    }
  }

  val rspArea = new Area {
    val context = io.output.rsp.context.as(OutputContext())
    io.input.rsp.valid := io.output.rsp.valid
    io.input.rsp.last := io.output.rsp.last
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.source := context.source
    io.input.rsp.context := context.context
    io.output.rsp.ready := io.input.rsp.ready

    val readLogic = inputParameter.canRead generate new Area {
      val selReg = Reg(UInt(log2Up(ratio) bits))
      val sel = io.input.rsp.first ? context.selStart | selReg
      selReg := sel

      when(io.input.rsp.fire) {
        selReg := sel + 1
      }

      when(!io.input.rsp.last || sel =/= ratio - 1) {
        io.output.rsp.ready := False
      }

      when(context.selEnd =/= sel) {
        io.input.rsp.last := False
      }

      io.input.rsp.data := io.output.rsp.data.subdivideIn(ratio slices)(sel)
    }
  }
}