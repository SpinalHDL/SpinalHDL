package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAlignedSpliter, BmbAligner, BmbLengthFixer, BmbParameter}
import spinal.lib.memory.sdram.dfi.CtrlWithBmb.BmbPortParameter
import spinal.lib.memory.sdram.dfi.Interface._


case class BmbToTaskCmdPort(ip : BmbParameter, tpp : TaskPortParameter, tpa : TaskParameterAggregate, pp : BmbPortParameter) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool()
    val output = master(PreTaskPort(tpp, tpa))
  }

  case class PackContext() extends Bundle{
    val source = UInt(ip.access.sourceWidth bits)
    val context = Bits(ip.access.contextWidth bits)
  }

  val cmdToRspCount = io.output.cmd.write ? U(0) | (io.output.cmd.length +^ 1) << log2Up(tpa.pl.beatCount)

  val rspPendingCounter = Reg(UInt(log2Up(pp.rspBufferSize + 1) bits)) init(0)
  rspPendingCounter := rspPendingCounter + (io.input.cmd.lastFire ? cmdToRspCount | U(0)) - U(io.output.rsp.fire)

  val toManyRsp = (U"0" @@ rspPendingCounter) + cmdToRspCount > pp.rspBufferSize //pp.rspBufferSize - pp.beatPerBurst*tpa.pl.beatCount //Pessimistic

  io.input.cmd.ready := io.output.cmd.ready && !toManyRsp
  if(ip.access.canWrite) io.input.cmd.ready clearWhen(!io.output.writeData.ready)

  val cmdContext = PackContext()
  cmdContext.context := io.input.cmd.context
  cmdContext.source := io.input.cmd.source

  io.output.cmd.valid := io.input.cmd.firstFire
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  assert(widthOf(io.output.cmd.length) >= widthOf(io.input.cmd.length) - log2Up(tpa.pl.bytePerBurst))
  io.output.cmd.length := (io.input.cmd.length >> log2Up(tpa.pl.bytePerBurst)).resized
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.burstLast := io.inputBurstLast

  if(ip.access.canWrite) {
    io.output.writeData.valid := io.input.cmd.fire && io.input.cmd.isWrite
    io.output.writeData.data := io.input.cmd.data
    io.output.writeData.mask := io.input.cmd.mask
  }

  val rspContext = io.output.rsp.context.as(PackContext())
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.last := io.output.rsp.last
  if(ip.access.canRead) io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.context
  io.input.rsp.source := rspContext.source
}


object BmbAdapter{
  def corePortParameter(pp : BmbPortParameter, pl : PhyConfig) = TaskPortParameter(
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(BmbAligner.outputParameter(pp.bmb.access, log2Up(pl.burstWidth/8)), log2Up(pl.burstWidth/8))
      converterBmb.contextWidth + converterBmb.sourceWidth
    },
    writeTokenInterfaceWidth = 1,
    writeTokenBufferSize = pp.dataBufferSize + 4,
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
    val output = master(PreTaskPort(tpp, tpa))
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

  val cmdAddress = Stream(TaskWrRdCmd(tpp, tpa))
  val writeDataToken = UInt(tpp.writeTokenInterfaceWidth bits)
  val syncBuffer = if(!asyncCc) new Area{
    cmdAddress << inputLogic.converter.io.output.cmd.queueLowLatency(pp.cmdBufferSize, 1)
    inputLogic.converter.io.output.rsp << io.output.rsp.queueLowLatency(pp.rspBufferSize, 1)

    if(pp.bmb.access.canWrite) {
      io.output.writeData << inputLogic.converter.io.output.writeData.queueLowLatency(pp.dataBufferSize, 1)
      writeDataToken := RegNext(U(inputLogic.converter.io.output.writeData.fire)) init (0)
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
      writeDataToken := RegNext(U(tockenIncrement)) init (0)
    }
  }

  val writeTokens =  new Area{
    val canWrite = tpp.canWrite
    val consume = io.output.writeDataToken.ready
    val counter = canWrite generate Reg(UInt(log2Up(tpp.writeTokenBufferSize + 1) bits)).init(0)
    if(canWrite) {
      counter := counter + writeDataToken - (U(consume) << log2Up(pl.beatCount))
      io.output.writeDataToken.valid := RegInit(False) setWhen(counter >= pl.beatCount) clearWhen(consume && counter < pl.beatCount*2)
    }
  }

  io.output.cmd << cmdAddress.m2sPipe().haltWhen(RegNext(io.refresh)) //No pipelining after the halt please, else refresh incoherency
  assert(!io.output.rsp.isStall, "SDRAM rsp buffer stalled !")
}


