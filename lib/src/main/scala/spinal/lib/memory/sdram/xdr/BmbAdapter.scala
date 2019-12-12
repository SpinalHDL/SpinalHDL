package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAligner, BmbCmd, BmbLengthFixer}

object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyLayout) = CorePortParameter(
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(BmbAligner.outputParameter(pp.bmb, log2Up(pl.burstWidth/8)), log2Up(pl.burstWidth/8))
      converterBmb.contextWidth + converterBmb.sourceWidth
    }
  )
}

case class BmbAdapter(pp : BmbPortParameter,
                      cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val corePortParameter = BmbAdapter.corePortParameter(pp, cpa.pl)
  assert(pl.beatCount*4 <= pp.rspBufferSize, s"SDRAM rspBufferSize should be at least ${pl.beatCount*4}")
  assert(pl.beatCount <= pp.dataBufferSize, s"SDRAM dataBufferSize should be at least ${pl.beatCount}")

  val io = new Bundle{
    val refresh = in Bool()
    val input = slave(Bmb(pp.bmb))
    val output = master(CorePort(corePortParameter, cpa))
  }

  val asyncCc = pp.clockDomain != ClockDomain.current
  val inputLogic = new ClockingArea(pp.clockDomain) {
    val aligner = pp.clockDomain(BmbAligner(pp.bmb, log2Up(pl.burstWidth / 8)))
    aligner.io.input << io.input

    val lengthFixer = BmbLengthFixer(aligner.io.output.p, log2Up(pl.burstWidth / 8))
    lengthFixer.io.input << aligner.io.output

    val converter = BmbToCorePort(lengthFixer.io.output.p, io.output.cpp, cpa)
    converter.io.input << lengthFixer.io.output
    converter.io.inputBurstLast := lengthFixer.io.outputBurstLast
  }

  val cmdAddressBuffer = asyncCc match {
    case false => new StreamFifoLowLatency(inputLogic.converter.io.output.cmd.payloadType, pp.cmdBufferSize, 1).io
    case true  => new StreamFifoCC(inputLogic.converter.io.output.cmd.payloadType, pp.cmdBufferSize, pp.clockDomain, ClockDomain.current).io
  }
  val cmdDataBuffer = asyncCc match {
    case false => new StreamFifoLowLatency(inputLogic.converter.io.output.writeData.payloadType, pp.dataBufferSize, 1).io
    case true  => new StreamFifoCC(inputLogic.converter.io.output.writeData.payloadType, pp.dataBufferSize, pp.clockDomain, ClockDomain.current).io
  }
  val rspBuffer = asyncCc match {
    case false => new StreamFifoLowLatency(inputLogic.converter.io.output.rsp.payloadType, pp.rspBufferSize, 1).io
    case true  => new StreamFifoCC(inputLogic.converter.io.output.rsp.payloadType, pp.rspBufferSize, ClockDomain.current, pp.clockDomain).io
  }
  assert(!rspBuffer.push.isStall, "SDRAM rsp buffer stalled !")

  val stateToArbitrationLatency = 2
  val cmdAddressBufferPop = cmdAddressBuffer.pop.s2mPipe()
  val cmdToRspCount = (cmdAddressBufferPop.fire) ? (cmdAddressBufferPop.write ? U(1) | U(pl.beatCount))| U(0)

  val inFlightRsp = Reg(UInt(log2Up(pp.rspBufferSize+1) bits)) init(0)
  inFlightRsp := inFlightRsp + RegNext(cmdToRspCount).init(0) - U(RegNext(rspBuffer.push.fire) init(False))

  val toManyPendingRsp = RegNext(rspBuffer.pushOccupancy + inFlightRsp > pp.rspBufferSize - pl.beatCount*(1+stateToArbitrationLatency)) init(False)

  //Do not need to check that write data are available for the given address pop, as the converter push write address on last beat
  cmdAddressBuffer.push << inputLogic.converter.io.output.cmd
  cmdAddressBufferPop.m2sPipe().haltWhen(toManyPendingRsp || RegNext(io.refresh)) >> io.output.cmd //No pipelining after the halt please
  cmdDataBuffer.push << inputLogic.converter.io.output.writeData
  cmdDataBuffer.pop >> io.output.writeData
  rspBuffer.push << io.output.rsp
  rspBuffer.pop >> inputLogic.converter.io.output.rsp
}
