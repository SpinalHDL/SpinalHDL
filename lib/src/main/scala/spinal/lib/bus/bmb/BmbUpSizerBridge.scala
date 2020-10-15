package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._


object BmbUpSizerBridge{
  def outputParameterFrom( inputParameter : BmbAccessParameter,
                           outputDataWidth : Int): BmbAccessParameter = {
    val ratio = outputDataWidth / inputParameter.dataWidth
    import BmbParameter.BurstAlignement._
    val contextAddition = if(inputParameter.canRead) 2*log2Up(ratio) else 0
    inputParameter.copy(
      dataWidth = outputDataWidth
    ).sourcesTransform(s => s.copy(
      contextWidth = s.contextWidth + contextAddition,
      alignment = inputParameter.alignment match {
        case BYTE => BYTE
        case WORD => BYTE
        case LENGTH => LENGTH
      }
    ))
  }
}


//Todo use less ressource depending parameters
case class BmbUpSizerBridge(inputParameter: BmbParameter,
                            outputParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val input  = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val ratio = outputParameter.access.dataWidth / inputParameter.access.dataWidth
  val selRange = (outputParameter.access.wordRangeLength - 1 downto inputParameter.access.wordRangeLength)
  assert(ratio > 1)

  case class OutputContext() extends Bundle {
    val selStart = inputParameter.access.canRead generate UInt(log2Up(ratio) bits)
    val selEnd = inputParameter.access.canRead generate UInt(log2Up(ratio) bits)
    val context = Bits(inputParameter.access.contextWidth bits)
  }

  val cmdArea = new Area {
    val selStart = io.input.cmd.address(selRange)

    val context = OutputContext()
    context.context := io.input.cmd.context
    if (inputParameter.access.canRead) {
      context.selStart := selStart
      context.selEnd := (io.input.cmd.address(selRange) + io.input.cmd.transferBeatCountMinusOne).resized
      if (inputParameter.access.canWrite) when(io.input.cmd.isWrite) {
        context.selEnd := io.input.cmd.address(selRange)
      }
    }


    io.output.cmd.last := io.input.cmd.last
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.length := io.input.cmd.length
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.context := B(context)

    if (!inputParameter.access.canWrite) {
      io.output.cmd.valid := io.input.cmd.valid
      io.input.cmd.ready := io.output.cmd.ready
    }

    val writeLogic = inputParameter.access.canWrite generate new Area {
      val dataRegs = Vec(Reg(Bits(inputParameter.access.dataWidth bits)), ratio - 1)
      val maskRegs = Vec(Reg(Bits(inputParameter.access.dataWidth / 8 bits)) init(0), ratio - 1)

      val selReg = Reg(UInt(log2Up(ratio) bits))
      val sel = io.input.cmd.first ? selStart | selReg
      when(io.input.cmd.fire) {
        selReg := sel + 1
      }

      val outputData = io.output.cmd.data.subdivideIn(ratio slices)
      val outputMask = io.output.cmd.mask.subdivideIn(ratio slices)
      for (i <- 0 until ratio) {
        outputData(i) := io.input.cmd.data
        if (i != ratio - 1) when(!io.input.cmd.first && selReg =/= i) {
          outputData(i) := dataRegs(i)
        } otherwise {
          dataRegs(i) := io.input.cmd.data
        }

        if (i == ratio - 1){
          outputMask(i) := (sel === i) ? io.input.cmd.mask | 0
        } else {
          outputMask(i) := (sel === i) ? io.input.cmd.mask | maskRegs(i)
          when(io.input.cmd.valid && sel === i){
            maskRegs(i) := io.input.cmd.mask
          }
          when(io.output.cmd.fire){
            maskRegs(i) := 0
          }
        }
      }

      io.output.cmd.valid := io.input.cmd.valid && (sel === ratio-1 || io.input.cmd.last)
      io.input.cmd.ready := !io.output.cmd.isStall
    }
  }

  val rspArea = new Area {
    val context = io.output.rsp.context.as(OutputContext())
    io.input.rsp.valid := io.output.rsp.valid
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.source := io.output.rsp.source
    io.input.rsp.context := context.context

    if(!inputParameter.access.canRead) {
      io.input.rsp.last := io.output.rsp.last
      io.output.rsp.ready := io.input.rsp.ready
    }

    val readLogic = inputParameter.access.canRead generate new Area {
      val selReg = Reg(UInt(log2Up(ratio) bits))
      val sel = io.input.rsp.first ? context.selStart | selReg
      selReg := sel

      when(io.input.rsp.fire) {
        selReg := sel + 1
      }

      io.input.rsp.last := io.output.rsp.last && sel === context.selEnd
      io.output.rsp.ready := io.input.rsp.ready && (io.input.rsp.last || sel === ratio - 1)

      when(context.selEnd =/= sel) {
        io.input.rsp.last := False
      }

      io.input.rsp.data := io.output.rsp.data.subdivideIn(ratio slices)(sel)
    }
  }
}