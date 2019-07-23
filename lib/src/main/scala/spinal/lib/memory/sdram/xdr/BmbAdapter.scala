package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAligner, BmbLengthFixer}

object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyParameter) = CorePortParameter(
    contextWidth = BmbLengthFixer.outputParameter(BmbAligner.outputParameter(pp.bmb, log2Up(pl.wordWidth/8)), log2Up(pl.wordWidth/8)).contextWidth + pp.bmb.sourceWidth
  )
}

case class BmbAdapter(pp : BmbPortParameter,
                      cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle{
    val input = slave(Bmb(pp.bmb))
    val output = master(CorePort(BmbAdapter.corePortParameter(pp, cpa.pl), cpa))
  }

  val aligner = BmbAligner(pp.bmb, log2Up(pl.wordWidth/8))
  aligner.io.input << io.input

  val cmdBuffer = new StreamFifoLowLatency(aligner.io.output.cmd.payload, pp.cmdBufferSize, 0)
  val cmdHalt = if(pl.beatCount == 1) False else aligner.io.output.cmd.isWrite && cmdBuffer.io.occupancy < cpa.pl.beatCount
  cmdBuffer.io.push << aligner.io.output.cmd.haltWhen(cmdHalt)

  val lengthFixer = BmbLengthFixer(cmdBuffer.io.pop.p, log2Up(pl.wordWidth/8))
  lengthFixer.io.input.cmd << cmdBuffer.io.pop
  lengthFixer.io.input.rsp >> aligner.io.output.rsp

  val converter = BmbToCorePort(lengthFixer.io.output.p, io.output.cpp, cpa)
  converter.io.input.cmd << lengthFixer.io.output.cmd
  converter.io.input.rsp >> lengthFixer.io.output.rsp

  io.output.cmd << converter.io.output.cmd.stage()
  io.output.rsp >> converter.io.output.rsp
}
