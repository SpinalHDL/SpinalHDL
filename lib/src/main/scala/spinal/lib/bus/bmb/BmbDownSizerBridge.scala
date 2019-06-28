package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}


object BmbDownSizerBridge{
  def outputParameterFrom( inputParameter : BmbParameter,
                           outputDataWidth : Int): BmbParameter = {
    val ratio = inputParameter.dataWidth/outputDataWidth
    inputParameter.copy(
      dataWidth = outputDataWidth,
      sourceWidth = 0,
      contextWidth = inputParameter.contextWidth + inputParameter.sourceWidth + log2Up(ratio)
    )
  }
}

//Todo use less ressource depending parameters
case class BmbDownSizerBridge(inputParameter: BmbParameter,
                              outputParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val input  = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val ratio = inputParameter.dataWidth / outputParameter.dataWidth
  val selRange = (inputParameter.wordRangeLength - 1 downto outputParameter.wordRangeLength)
  assert(ratio > 1)

  case class OutputContext() extends Bundle {
    val context = Bits(inputParameter.contextWidth bits)
    val source = UInt(inputParameter.sourceWidth bits)
    val sel = UInt(log2Up(ratio) bits)
  }
  val cmdContext = OutputContext()
  cmdContext.context := io.input.cmd.context
  cmdContext.source := io.input.cmd.source
  cmdContext.sel := io.input.cmd.address(selRange)

  io.output.cmd.valid := io.input.cmd.valid
  io.output.cmd.opcode := io.input.cmd.opcode
  io.output.cmd.address := io.input.cmd.address
  io.output.cmd.length := io.input.cmd.length
  io.output.cmd.context := B(cmdContext)

  if(!inputParameter.canWrite){
    io.output.cmd.last := io.input.cmd.last
    io.input.cmd.ready := io.output.cmd.ready
  }

  val cmdLogic = inputParameter.canWrite generate new Area {
    val locked = RegNextWhen(!io.output.cmd.last, io.output.cmd.fire) init(False)
    val counter = Reg(UInt(log2Up(ratio) bits))
    val sel = locked ? counter | io.input.cmd.address(selRange)

    when(io.output.cmd.fire){
      counter := sel + 1
    }

    io.output.cmd.data := io.input.cmd.data.subdivideIn(ratio slices)(sel)
    io.output.cmd.mask := io.input.cmd.mask.subdivideIn(ratio slices)(sel)
    io.output.cmd.last := io.input.cmd.last && (io.input.cmd.isRead || sel === (io.input.cmd.address + io.input.cmd.length)(selRange))
    io.input.cmd.ready := io.output.cmd.ready && (sel === sel.maxValue || io.output.cmd.last)
  }



  val rspContext = io.output.rsp.context.as(OutputContext())
  io.input.rsp.last := io.output.rsp.last
  io.input.rsp.opcode := io.output.rsp.opcode
  io.input.rsp.source := rspContext.source
  io.input.rsp.context := rspContext.context
  io.output.rsp.ready := io.input.rsp.ready

  if(!inputParameter.canRead){
    io.input.rsp.valid  := io.output.rsp.valid
  }

  val rspLogic = inputParameter.canRead generate new Area{
    val locked = RegNextWhen(!io.output.rsp.last, io.output.rsp.fire) init(False)
    val counter = Reg(UInt(log2Up(ratio) bits))
    val sel = locked ? counter | rspContext.sel
    val buffers = Vec(Reg(Bits(outputParameter.dataWidth bits)), ratio - 1)
    val words = Vec(Bits(outputParameter.dataWidth bits), ratio)

    when(io.output.rsp.fire){
      counter := sel + 1
      for(i <- 0 until ratio - 1){
        when(sel === i){
          buffers(i) := io.output.rsp.data
        }
      }
    }

    //Buffer bypass to have zero latency rsp
    for(i <- 0 until ratio-1){
      words(i) := buffers(i)
      when(io.input.rsp.last && sel === i){
        words(i) := io.output.rsp.data
      }
    }
    words.last := io.output.rsp.data

    io.input.rsp.valid  := io.output.rsp.valid && (io.output.rsp.last || sel === sel.maxValue)
    io.input.rsp.data := B(words)
  }
}