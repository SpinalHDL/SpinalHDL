package spinal.lib.memory.sdram.Dfi.CtrlWithBmb

import spinal.lib._
import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbAlignedSpliter, BmbAligner, BmbLengthFixer}
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.memory.sdram.Dfi._

object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyConfig) = TaskPortParameter(
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
                      tpa : TaskParameterAggregate) extends Component{
  import tpa._
  val tpp = BmbAdapter.corePortParameter(pp, tpa.pl)
  assert(pl.beatCount*4 <= pp.rspBufferSize, s"SDRAM rspBufferSize should be at least ${pl.beatCount*4}")
  assert(pl.beatCount <= pp.dataBufferSize, s"SDRAM dataBufferSize should be at least ${pl.beatCount}")

  val io = new Bundle{
    val refresh = in Bool()
    val input = slave(Bmb(pp.bmb))
    val output = master(TaskCmdPort(tpp, tpa))
  }

  val asyncCc = pp.clockDomain != ClockDomain.current
  val inputLogic = new ClockingArea(pp.clockDomain) {
    val aligner = BmbAligner(pp.bmb, log2Up(pl.burstWidth / 8))
    aligner.io.input << io.input

    val splitLength = Math.min(tpa.tp.bytePerTaskMax, 1 << pp.bmb.access.lengthWidth)
    assert(pp.rspBufferSize*tpa.pl.bytePerBeat >= splitLength)

    val spliter = BmbAlignedSpliter(aligner.io.output.p, splitLength)
    spliter.io.input << aligner.io.output

    val converter = BmbToTaskCmdPort(spliter.io.output.p, io.output.tpp, tpa, pp)
    converter.io.input << spliter.io.output.pipelined(cmdValid = true)
    converter.io.inputBurstLast := spliter.io.outputBurstLast
  }

  val cmdAddress = Stream(TaskCmd(tpp, tpa))
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






case class BmbBridge(port : BmbPortParameter, tpa : TaskParameterAggregate) extends Component{
  import tpa._
  val io = new Bundle{
    val bmb      = slave(Bmb(port.bmb))
    val taskPort = master(TaskPort(tpp, tpa))
  }

  val bmbAdapter =  BmbAdapter(port, tpa)
  bmbAdapter.io.input <>  io.bmb
  bmbAdapter.io.output.writeData <> io.taskPort.writeData
  bmbAdapter.io.output.rsp <> io.taskPort.rsp

  val maketask = MakeTask(tpp,tpa)
  maketask.io.cmd << bmbAdapter.io.output.cmd
  maketask.io.writeDataTockens <> bmbAdapter.io.output.writeDataTocken
  maketask.io.output <> io.taskPort.tasks

  val refresher = Refresher(tpa)
  refresher.io.refresh.valid <> bmbAdapter.io.refresh
  refresher.io.refresh <> maketask.io.refresh
}
