package spinal.lib.memory.sdram.dfi.foundation

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.memory.sdram.dfi.interface._

case class BmbToPreTaskPort(ip: BmbParameter, tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool ()
    val output = master(PreTaskPort(tpp, tpa))
  }
  val cmdToRspCount = io.output.cmd.write ? U(0) | (io.output.cmd.length +^ 1) << log2Up(tpa.config.beatCount)
  val rspPendingCounter = Reg(UInt(log2Up(tp.rspBufferSize + 1) bits)) init (0)
  val toManyRsp =
    (U"0" @@ rspPendingCounter) + cmdToRspCount > tp.rspBufferSize // pp.rspBufferSize - pp.beatPerBurst*tpa.config.beatCount //Pessimistic
  rspPendingCounter := rspPendingCounter + (io.input.cmd.lastFire ? cmdToRspCount | U(0)) - U(io.output.rsp.fire)
  val cmdContext = PackContext()

  io.input.cmd.ready := io.output.cmd.ready && !toManyRsp
  if (ip.access.canWrite) io.input.cmd.ready clearWhen (!io.output.writeData.ready)
  val rspContext = io.output.rsp.context.as(PackContext())
  cmdContext.context := io.input.cmd.context
  cmdContext.source := io.input.cmd.source

  io.output.cmd.valid := io.input.cmd.firstFire
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  assert(widthOf(io.output.cmd.length) >= widthOf(io.input.cmd.length) - log2Up(tpa.config.bytePerBurst))
  io.output.cmd.length := (io.input.cmd.length >> log2Up(tpa.config.bytePerBurst)).resized
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.burstLast := io.inputBurstLast

  if (ip.access.canWrite) {
    io.output.writeData.valid := io.input.cmd.fire && io.input.cmd.isWrite
    io.output.writeData.data := io.input.cmd.data
    io.output.writeData.mask := io.input.cmd.mask
  }

  case class PackContext() extends Bundle {
    val source = UInt(ip.access.sourceWidth bits)
    val context = Bits(ip.access.contextWidth bits)
  }
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.last := io.output.rsp.last
  if (ip.access.canRead) io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.context
  io.input.rsp.source := rspContext.source
}

object BmbAdapter {
  def taskPortParameter(bmbp: BmbParameter, config: DfiConfig, tp: TaskParameter) = TaskPortParameter(
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(
        BmbAligner.outputParameter(bmbp.access, log2Up(config.burstWidth / 8)),
        log2Up(config.burstWidth / 8)
      )
      converterBmb.contextWidth + converterBmb.sourceWidth
    },
    writeTokenInterfaceWidth = 1,
    writeTokenBufferSize = tp.dataBufferSize + 4,
    canRead = bmbp.access.canRead,
    canWrite = bmbp.access.canWrite
  )
}

case class BmbAdapter(bmbp: BmbParameter, tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val tpp = BmbAdapter.taskPortParameter(bmbp, tpa.config, tp)
  assert(config.beatCount * 4 <= tp.rspBufferSize, s"SDRAM rspBufferSize should be at least ${config.beatCount * 4}")
  assert(config.beatCount <= tp.dataBufferSize, s"SDRAM dataBufferSize should be at least ${config.beatCount}")

  val io = new Bundle {
    val refresh = in Bool ()
    val input = slave(Bmb(bmbp))
    val output = master(PreTaskPort(tpp, tpa))
  }

  val inputLogic = new Area {
    val aligner = BmbAligner(bmbp, log2Up(config.burstWidth / 8))
    aligner.io.input << io.input

    val splitLength = Math.min(tpa.tp.bytePerTaskMax, 1 << bmbp.access.lengthWidth)
    assert(tp.rspBufferSize * tpa.config.bytePerBeat >= splitLength)

    val spliter = BmbAlignedSpliter(aligner.io.output.p, splitLength)
    spliter.io.input << aligner.io.output

    val converter = BmbToPreTaskPort(spliter.io.output.p, io.output.tpp, tpa)
    converter.io.input << spliter.io.output.pipelined(cmdValid = true)
    converter.io.inputBurstLast := spliter.io.outputBurstLast
  }

  val cmdAddress = Stream(TaskWrRdCmd(tpp, tpa))
  val writeDataToken = UInt(tpp.writeTokenInterfaceWidth bits)
  val syncBuffer = new Area {
    cmdAddress << inputLogic.converter.io.output.cmd.queueLowLatency(tp.cmdBufferSize, 1)
    inputLogic.converter.io.output.rsp << io.output.rsp.queueLowLatency(tp.rspBufferSize, 1)

    if (bmbp.access.canWrite) {
      io.output.writeData << inputLogic.converter.io.output.writeData.queueLowLatency(tp.dataBufferSize, 1)
      writeDataToken := RegNext(U(inputLogic.converter.io.output.writeData.fire)) init (0)
    }
  }

  val writeTokens = new Area {
    val canWrite = tpp.canWrite
    val consume = io.output.writeDataToken.ready
    val counter = canWrite generate Reg(UInt(log2Up(tpp.writeTokenBufferSize + 1) bits)).init(0)
    if (canWrite) {
      counter := counter + writeDataToken - (U(consume) << log2Up(config.beatCount))
      io.output.writeDataToken.valid := RegInit(
        False
      ) setWhen (counter >= config.beatCount) clearWhen (consume && counter < config.beatCount * 2)
    }
  }

  io.output.cmd << cmdAddress
    .m2sPipe()
    .haltWhen(RegNext(io.refresh)) // No pipelining after the halt please, else refresh incoherency
  assert(!io.output.rsp.isStall, "SDRAM rsp buffer stalled !")
}
