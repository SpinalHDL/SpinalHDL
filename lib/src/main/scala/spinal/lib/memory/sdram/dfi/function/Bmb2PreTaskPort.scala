package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

case class BmbToPreTaskPort(ip: BmbParameter, taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool ()
    val output = master(PreTaskPort(taskConfig, dfiConfig))
  }
  val cmdToRspCount = io.output.cmd.write ? U(1) | (io.output.cmd.length +^ 1) << log2Up(dfiConfig.beatCount)
  val rspPendingCounter = Reg(UInt(log2Up(taskConfig.taskParameter.rspBufferSize + 1) bits)) init (0)
  val toManyRsp =
    (U"0" @@ rspPendingCounter) + cmdToRspCount > taskConfig.taskParameter.rspBufferSize // taskParameter.rspBufferSize - taskParameter.beatPerBurst*dfiConfig.beatCount //Pessimistic
  rspPendingCounter := rspPendingCounter + (io.input.cmd.lastFire ? cmdToRspCount | U(0)) - U(io.output.rsp.fire)
  val cmdContext = PackContext()

  io.input.cmd.ready := io.output.cmd.ready && !toManyRsp
  if (ip.access.canWrite) io.input.cmd.ready clearWhen (!io.output.writeData.ready)
  val rspContext = io.output.rsp.context.as(PackContext())
  cmdContext.context := io.input.cmd.context
  cmdContext.source := io.input.cmd.source

  io.output.cmd.valid := io.input.cmd.lastFire
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  assert(widthOf(io.output.cmd.length) >= widthOf(io.input.cmd.length) - log2Up(dfiConfig.bytePerBurst))
  io.output.cmd.length := (io.input.cmd.length >> log2Up(dfiConfig.bytePerBurst)).resized
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
  def taskConfig(bmbp: BmbParameter, dfiConfig: DfiConfig, tp: TaskParameter) = TaskConfig(
    taskParameter = tp,
    contextWidth = {
      val converterBmb = BmbLengthFixer.outputParameter(
        BmbAligner.outputParameter(bmbp.access, log2Up(dfiConfig.burstWidth / 8)),
        log2Up(dfiConfig.burstWidth / 8)
      )
      converterBmb.contextWidth + converterBmb.sourceWidth
    },
    writeTokenInterfaceWidth = 1,
    writeTokenBufferSize = tp.dataBufferSize + 4,
    canRead = bmbp.access.canRead,
    canWrite = bmbp.access.canWrite
  )
}

case class BmbAdapter(bmbp: BmbParameter, taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  assert(
    dfiConfig.beatCount * 4 <= taskConfig.taskParameter.rspBufferSize,
    s"SDRAM rspBufferSize should be at least ${dfiConfig.beatCount * 4}"
  )
  assert(
    dfiConfig.beatCount <= taskConfig.taskParameter.dataBufferSize,
    s"SDRAM dataBufferSize should be at least ${dfiConfig.beatCount}"
  )

  val io = new Bundle {
    val halt = in Bool ()
    val input = slave(Bmb(bmbp))
    val output = master(PreTaskPort(taskConfig, dfiConfig))
  }

  val inputLogic = new Area {
    val aligner = BmbAligner(bmbp, log2Up(dfiConfig.burstWidth / 8))
    aligner.io.input << io.input

    val splitLength = Math.min(taskConfig.taskParameter.bytePerTaskMax, 1 << bmbp.access.lengthWidth)
    assert(taskConfig.taskParameter.rspBufferSize * dfiConfig.bytePerBeat >= splitLength)

    val spliter = BmbAlignedSpliter(aligner.io.output.p, splitLength)
    spliter.io.input << aligner.io.output.pipelined(cmdValid = true, cmdReady = true)

    val converter = BmbToPreTaskPort(spliter.io.output.p, taskConfig, dfiConfig)
    converter.io.input << spliter.io.output.pipelined(cmdValid = true)
    converter.io.inputBurstLast := spliter.io.outputBurstLast
  }

  val cmdAddress = Stream(TaskWrRdCmd(taskConfig, dfiConfig))
  val writeDataToken = UInt(taskConfig.writeTokenInterfaceWidth bits)
  val syncBuffer = new Area {
    cmdAddress << inputLogic.converter.io.output.cmd.queueLowLatency(taskConfig.taskParameter.cmdBufferSize, 1)
    inputLogic.converter.io.output.rsp << io.output.rsp.queueLowLatency(taskConfig.taskParameter.rspBufferSize, 1)

    if (bmbp.access.canWrite) {
      io.output.writeData << inputLogic.converter.io.output.writeData.queueLowLatency(taskConfig.taskParameter.dataBufferSize, 1)
      writeDataToken := RegNext(U(inputLogic.converter.io.output.writeData.fire)) init (0)
    }
  }

  val writeTokens = new Area {
    val canWrite = taskConfig.canWrite
    val consume = io.output.writeDataToken.ready
    val counter = canWrite generate Reg(UInt(log2Up(taskConfig.writeTokenBufferSize + 1) bits)).init(0)
    if (canWrite) {
      counter := counter + writeDataToken - (U(consume) << log2Up(dfiConfig.beatCount))
      io.output.writeDataToken.valid := RegInit(
        False
      ) setWhen (counter >= dfiConfig.beatCount) clearWhen (consume && counter < dfiConfig.beatCount * 2)
    }
  }

  io.output.cmd << cmdAddress
    .m2sPipe()
    .haltWhen(RegNext(io.halt)) // No pipelining after the halt please, else refresh incoherency
  assert(!io.output.rsp.isStall, "SDRAM rsp buffer stalled !")
}
