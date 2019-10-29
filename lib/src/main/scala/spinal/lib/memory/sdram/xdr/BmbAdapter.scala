package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAligner, BmbLengthFixer}

object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyParameter) = CorePortParameter(
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(BmbAligner.outputParameter(pp.bmb, log2Up(pl.wordWidth/8)), log2Up(pl.wordWidth/8))
      converterBmb.contextWidth + converterBmb.sourceWidth
    }
  )
}

case class BmbAdapter(pp : BmbPortParameter,
                      cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle{
    val refresh = in Bool()
    val input = slave(Bmb(pp.bmb))
    val output = master(CorePort(BmbAdapter.corePortParameter(pp, cpa.pl), cpa))
  }

  val asyncCc = pp.clockDomain != ClockDomain.current
  val aligner = pp.clockDomain(BmbAligner(pp.bmb, log2Up(pl.wordWidth/8)))
  val cmdBuffer = asyncCc match {
    case false => new StreamFifoLowLatency(aligner.io.output.cmd.payload, pp.cmdBufferSize, 1).io
    case true  => new StreamFifoCC(aligner.io.output.cmd.payload, pp.cmdBufferSize, pp.clockDomain, ClockDomain.current).io
  }
  val rspBuffer = asyncCc match {
    case false => new StreamFifoLowLatency(aligner.io.output.rsp.payload, pp.rspBufferSize, 1).io
    case true  => new StreamFifoCC(aligner.io.output.rsp.payload, pp.rspBufferSize, ClockDomain.current, pp.clockDomain).io
  }
  val lengthFixer = BmbLengthFixer(cmdBuffer.pop.p, log2Up(pl.wordWidth/8))
  val converter = BmbToCorePort(lengthFixer.io.output.p, io.output.cpp, cpa)

  //trafic
  //notEnoughCmd assume that there is no tocken between cmdBuffer.pop and lengthFixer.io.output
  val notEnoughCmd = if(pl.beatCount == 1) False else lengthFixer.io.output.cmd.isWrite && lengthFixer.io.output.cmd.first && cmdBuffer.popOccupancy < pl.beatCount
  val inFlightRsp = Reg(UInt(log2Up(1 + pl.outputLatency + cp.readLatencies.max + pl.inputLatency + 6) bits)) init(0) //TODO secure
  val cmdToRspCount = (converter.io.output.cmd.fire && converter.io.output.cmd.last) ? (converter.io.output.cmd.write ? U(1) | U(pl.beatCount))| U(0)
  inFlightRsp := inFlightRsp - U(io.output.rsp.valid) + cmdToRspCount
  val toManyPendingRsp = RegNext(rspBuffer.pushOccupancy + inFlightRsp + pl.beatCount*2 >= pp.rspBufferSize) init(False) //TODO secure

  //CMD
  aligner.io.input.cmd << io.input.cmd
  cmdBuffer.push << aligner.io.output.cmd
  lengthFixer.io.input.cmd << cmdBuffer.pop
  converter.io.inputBurstLast := lengthFixer.io.outputBurstLast
  converter.io.input.cmd << lengthFixer.io.output.cmd.haltWhen(notEnoughCmd) //Avoid
  io.output.cmd << converter.io.output.cmd.haltWhen((toManyPendingRsp && converter.io.output.cmd.first) || (io.refresh && converter.io.output.cmd.isFirst)).s2mPipe().stage()

  //RSP
  converter.io.output.rsp << io.output.rsp.toFlow.toStream //Drop the ready, as the fifo will never be full
  lengthFixer.io.output.rsp << converter.io.input.rsp
  rspBuffer.push << lengthFixer.io.input.rsp
  aligner.io.output.rsp << rspBuffer.pop
  io.input.rsp << aligner.io.input.rsp
}
