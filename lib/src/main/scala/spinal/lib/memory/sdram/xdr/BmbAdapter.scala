package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAlignedSpliter, BmbAligner, BmbCmd, BmbLengthFixer}

object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyLayout) = CorePortParameter(
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(BmbAligner.outputParameter(pp.bmb.access, log2Up(pl.burstWidth/8)), log2Up(pl.burstWidth/8))
      converterBmb.contextWidth + converterBmb.sourceWidth
    },
    writeTockenInterfaceWidth = 1,
    writeTockenBufferSize = pp.dataBufferSize + 4,
    canRead = pp.bmb.access.canRead,
    canWrite = pp.bmb.access.canWrite
  )
}

case class BmbAdapter(pp : BmbPortParameter,
                      cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val cpp = BmbAdapter.corePortParameter(pp, cpa.pl)
  assert(pl.beatCount*4 <= pp.rspBufferSize, s"SDRAM rspBufferSize should be at least ${pl.beatCount*4}")
  assert(pl.beatCount <= pp.dataBufferSize, s"SDRAM dataBufferSize should be at least ${pl.beatCount}")

  val io = new Bundle{
    val refresh = in Bool()
    val input = slave(Bmb(pp.bmb))
    val output = master(CorePort(cpp, cpa))
  }

  val asyncCc = pp.clockDomain != ClockDomain.current
  val inputLogic = new ClockingArea(pp.clockDomain) {
    val aligner = BmbAligner(pp.bmb, log2Up(pl.burstWidth / 8))
    aligner.io.input << io.input


    val splitLength = Math.min(cpa.cp.bytePerTaskMax, 1 << pp.bmb.access.lengthWidth)
    assert(pp.rspBufferSize*cpa.pl.bytePerBeat >= splitLength)

    val spliter = BmbAlignedSpliter(aligner.io.output.p, splitLength)
    spliter.io.input << aligner.io.output

    val converter = BmbToCorePort(spliter.io.output.p, io.output.cpp, cpa, pp)
    converter.io.input << spliter.io.output.pipelined(cmdValid = true)
    converter.io.inputBurstLast := spliter.io.outputBurstLast
  }

  val cmdAddress = Stream(CoreCmd(cpp, cpa))
  val syncBuffer = if(!asyncCc) new Area{
    cmdAddress << inputLogic.converter.io.output.cmd.queueLowLatency(pp.cmdBufferSize, 1)
    inputLogic.converter.io.output.rsp << io.output.rsp.queueLowLatency(pp.rspBufferSize, 1)

    if(pp.bmb.access.canWrite) {
      io.output.writeData << inputLogic.converter.io.output.writeData.queueLowLatency(pp.dataBufferSize, 1)
      io.output.writeDataTocken := RegNext(U(inputLogic.converter.io.output.writeData.fire)) init (0)
    }
  }
  val asyncBuffer = if(asyncCc) new Area {
    cmdAddress << inputLogic.converter.io.output.cmd.queue(pp.cmdBufferSize, pp.clockDomain, ClockDomain.current)
    inputLogic.converter.io.output.rsp << io.output.rsp.queue(pp.rspBufferSize, ClockDomain.current, pp.clockDomain)

    val writeData = if (pp.bmb.access.canWrite) new Area {
      val fifo = new StreamFifoCC(inputLogic.converter.io.output.writeData.payloadType, pp.dataBufferSize, pp.clockDomain, ClockDomain.current)
      fifo.io.push << inputLogic.converter.io.output.writeData
      io.output.writeData << fifo.io.pop

      val pushCounter = fromGray(fifo.popCC.pushPtrGray.pull())
      val tockenCounter = Reg(UInt(fifo.ptrWidth bits)) init (0)
      val tockenIncrement = tockenCounter =/= pushCounter
      when(tockenIncrement) {
        tockenCounter := tockenCounter + 1
      }

      io.output.writeDataTocken := RegNext(U(tockenIncrement)) init (0)
    }
  }

  io.output.cmd << cmdAddress.m2sPipe().haltWhen(RegNext(io.refresh)) //No pipelining after the halt please, else refresh incoherency
  assert(!io.output.rsp.isStall, "SDRAM rsp buffer stalled !")
}
