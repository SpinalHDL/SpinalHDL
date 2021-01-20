package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}


object BmbDownSizerBridge{
  def outputParameterFrom( inputAccessParameter : BmbAccessParameter,
                           outputDataWidth : Int): BmbAccessParameter = {
    val ratio = inputAccessParameter.dataWidth/outputDataWidth
    inputAccessParameter.copy(
      dataWidth = outputDataWidth
    ).sourcesTransform(s => s.copy(
      contextWidth = s.contextWidth + log2Up(ratio)
    ))
  }
}

//Todo use less ressource depending parameters
case class BmbDownSizerBridge(inputParameter: BmbParameter,
                              outputParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val input  = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val ratio = inputParameter.access.dataWidth / outputParameter.access.dataWidth
  val selRange = (inputParameter.access.wordRangeLength - 1 downto outputParameter.access.wordRangeLength)
  assert(ratio > 1)

  case class OutputContext() extends Bundle {
//    val source = UInt(inputParameter.sourceWidth bits)
    val sel = UInt(log2Up(ratio) bits)
    val context = Bits(inputParameter.access.contextWidth bits)
  }

  val cmdArea = new Area {
    val context = OutputContext()
    context.context := io.input.cmd.context
    context.sel := io.input.cmd.address(selRange)

    io.output.cmd.valid := io.input.cmd.valid
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.length := io.input.cmd.length
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.context := B(context)
    if(inputParameter.access.canExclusive) io.output.cmd.exclusive := io.input.cmd.exclusive

    if (!inputParameter.access.canWrite) {
      io.output.cmd.last := io.input.cmd.last
      io.input.cmd.ready := io.output.cmd.ready
    }

    val writeLogic = inputParameter.access.canWrite generate new Area {
      val locked = RegNextWhen(!io.output.cmd.last, io.output.cmd.fire) init (False)
      val counter = Reg(UInt(log2Up(ratio) bits))
      val sel = locked ? counter | io.input.cmd.address(selRange)

      when(io.output.cmd.fire) {
        counter := sel + 1
      }

      io.output.cmd.data := io.input.cmd.data.subdivideIn(ratio slices)(sel)
      io.output.cmd.mask := io.input.cmd.mask.subdivideIn(ratio slices)(sel)
      io.output.cmd.last := io.input.cmd.last && (io.input.cmd.isRead || sel === (io.input.cmd.address + io.input.cmd.length) (selRange))
      io.input.cmd.ready := io.output.cmd.ready && (sel === sel.maxValue || io.output.cmd.last)
    }
  }


  val rspArea = new Area {
    val context = io.output.rsp.context.as(OutputContext())
    io.input.rsp.last := io.output.rsp.last
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.source := io.output.rsp.source
    io.input.rsp.context := context.context
    io.output.rsp.ready := io.input.rsp.ready

    if(inputParameter.access.canExclusive) io.input.rsp.exclusive := io.output.rsp.exclusive

    if (!inputParameter.access.canRead) {
      io.input.rsp.valid := io.output.rsp.valid
    }

    val readLogic = inputParameter.access.canRead generate new Area {
      val locked = RegNextWhen(!io.output.rsp.last, io.output.rsp.fire) init (False)
      val counter = Reg(UInt(log2Up(ratio) bits))
      val sel = locked ? counter | context.sel
      val buffers = Vec(Reg(Bits(outputParameter.access.dataWidth bits)), ratio - 1)
      val words = Vec(Bits(outputParameter.access.dataWidth bits), ratio)

      when(io.output.rsp.fire) {
        counter := sel + 1
        for (i <- 0 until ratio - 1) {
          when(sel === i) {
            buffers(i) := io.output.rsp.data
          }
        }
      }

      //Buffer bypass to have zero latency rsp
      for (i <- 0 until ratio - 1) {
        words(i) := buffers(i)
        when(io.input.rsp.last && sel === i) {
          words(i) := io.output.rsp.data
        }
      }
      words.last := io.output.rsp.data

      io.input.rsp.valid := io.output.rsp.valid && (io.output.rsp.last || sel === sel.maxValue)
      io.input.rsp.data := B(words)
      io.output.rsp.ready setWhen(!io.input.rsp.valid)
    }
  }

  if(inputParameter.invalidation.canInvalidate){
    io.input.inv << io.output.inv
    io.input.ack >> io.output.ack
  }

  if(inputParameter.invalidation.canSync){
    io.input.sync << io.output.sync
  }
}