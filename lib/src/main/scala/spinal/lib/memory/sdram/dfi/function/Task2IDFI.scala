package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class CmdTxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val task = slave(OpTasks(taskConfig, dfiConfig))
    val cmd = Vec(master(Flow(DfiCmd(dfiConfig))), dfiConfig.frequencyRatio)
    val address = Vec(master(Flow(DfiAddr(dfiConfig))), dfiConfig.frequencyRatio)
  }
  def cmdphase(i: Int) = io.cmd(i)
  def addrphase(i: Int) = io.address(i)

  def ACTIVE: Bits = (~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber) ## B"b011").setName("ACTIVE")
  def WRITE: Bits = (~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber) ## B"b100").setName("WRITE")
  def READ: Bits = (~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber) ## B"b101").setName("READ")
  def PRECHARGE: Bits = (~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber) ## B"b010")
  def REFRESH: Bits = (~(B(1) << io.task.address.cs).resize(dfiConfig.chipSelectNumber) ## B"b001").setName("REFRESH")

  def AUTO_PRECHARGE_BIT = 10 // Disable auto precharge (auto close of row)
  def ALL_BANKS_BIT = 10 // Precharge all banks
  def COULMNRang = 0 until (dfiConfig.sdram.columnWidth)
  def BANK: Bits = io.task.address.bank.asBits
  def ROW: Bits = io.task.address.row.asBits
  def COLUMN: Bits = io.task.address.column.asBits

  def active = io.task.active
  def write = io.task.write
  def read = io.task.read
  def precharge = io.task.precharge
  def prechargeAll = io.task.prechargeAll
  def refresh = io.task.refresh

  io.cmd.foreach(_.valid.clear())
  io.cmd.foreach(_.payload.setAll())
  io.address.foreach(_.valid.clear())
  io.address.foreach(_.address.clearAll())
  io.address.foreach(_.bank.clearAll())

  when(active) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(ACTIVE)
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
    addrphase(dfiConfig.timeConfig.cmdPhase).address := ROW.resized
  }
  when(write) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(WRITE)
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
    addrphase(dfiConfig.timeConfig.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(read) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(READ)
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
    addrphase(dfiConfig.timeConfig.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(dfiConfig.timeConfig.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(precharge) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGE"))
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    addrphase(dfiConfig.timeConfig.cmdPhase).bank := BANK.resized
    addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).clear()
  }
  when(prechargeAll) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGEALL"))
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
    addrphase(dfiConfig.timeConfig.cmdPhase).address(ALL_BANKS_BIT).set()

  }
  when(refresh) {
    cmdphase(dfiConfig.timeConfig.cmdPhase).valid := True
    cmdphase(dfiConfig.timeConfig.cmdPhase).payload.assignFromBits(REFRESH)
    addrphase(dfiConfig.timeConfig.cmdPhase).valid.set()
  }
}

case class RdDataRxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {

  val frequencyRatio = dfiConfig.frequencyRatio
  val timeConfig = dfiConfig.timeConfig
  val io = new Bundle {
    val task = slave(OpTasks(taskConfig, dfiConfig))
    val input = Vec(slave(Stream(Fragment(DfiRdData(dfiConfig)))), frequencyRatio)
    val rden = out Vec (Bool(), frequencyRatio)
    val output = master(Flow(Fragment(TaskRsp(taskConfig, dfiConfig))))
  }
  val rspPipeline = new Area {
    val input = Flow(Fragment(Context()))
    assert(timeConfig.tPhyRdlat + timeConfig.tRddataEn >= 1)
    val cmd = input.toStream.queueLowLatency(
      1 << log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + dfiConfig.beatCount - 1) / dfiConfig.beatCount + 1),
      latency = 1
    )
    val rden = input.valid & ~input.write
    val rdensHistory =
      Vec(Vec(Bool(), (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 2), frequencyRatio)
    rdensHistory.foreach(
      _ := History(rden, 0 to (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 1, init = False)
    )

    val beatCounter = Counter(dfiConfig.beatCount, io.input.map(_.valid).orR)

    val delayCyc = timeConfig.tRddataEn / frequencyRatio
    val nextPhase = (dfiConfig.timeConfig.cmdPhase + timeConfig.tRddataEn) % frequencyRatio

    for (i <- 0 until (frequencyRatio)) {
      if (i >= nextPhase) {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc), 0 until (dfiConfig.beatCount), init = False).orR
      } else {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc + 1), 0 until (dfiConfig.beatCount), init = False).orR
      }
    }

    val output = Flow(Fragment(PipelineRsp()))
    output.valid.clear()
    output.valid.setWhen(cmd.valid && cmd.write && cmd.last || io.input.map(_.valid).orR)
    output.context := cmd.context
    output.last := cmd.write || beatCounter.willOverflowIfInc && cmd.last
    cmd.ready := cmd.write || beatCounter.willOverflow

    for ((outputData, phase) <- (output.data.subdivideIn(frequencyRatio slices).reverse, io.input).zipped) {
      outputData := B(phase.rdData)
    }
  }
  val rspPop = rspPipeline.output.stage()
  val ready = Vec(Reg(Bool()), frequencyRatio)

  rspPipeline.input.valid := False
  rspPipeline.input.write.clear()
  rspPipeline.input.last := io.task.last
  rspPipeline.input.context := io.task.context

  case class PipelineRsp() extends Bundle {
    val data = Bits(dfiConfig.beatWidth bits)
    val context = Bits(taskConfig.contextWidth bits)
  }
  io.output.valid := rspPop.valid
  io.output.last := rspPop.last
  if (io.output.taskConfig.canRead) io.output.data := rspPop.data
  io.output.context := rspPop.context.resized

  rspPipeline.input.valid.setWhen(io.task.read | io.task.write)
  rspPipeline.input.write.setWhen(io.task.write).clearWhen(io.task.read)

  case class Context() extends Bundle {
    val context = Bits(taskConfig.contextWidth bits)
    val write = Bool()
  }
  ready.foreach(_.init(False))
  for (i <- 0 until (frequencyRatio)) {
    ready(i).setWhen(io.task.read).clearWhen(io.task.write)
  }
  for ((outport, iready) <- (io.input, ready).zipped) {
    outport.ready := iready
  }
}

case class WrDataTxd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {

  val frequencyRatio = dfiConfig.frequencyRatio
  val timeConfig = dfiConfig.timeConfig
  val io = new Bundle {
    val write = in Bool ()
    val input = slave(Stream(TaskWriteData(dfiConfig)))
    val output = Vec(master(Flow(DfiWrData(dfiConfig))), frequencyRatio)
  }
  val delayCyc = timeConfig.tPhyWrLat / frequencyRatio
  val nextPhase = (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) % frequencyRatio
  val writeHistory = History(io.write, 0 until dfiConfig.beatCount, init = False)
  val write = writeHistory.orR
  val wrens = Vec(Bool(), frequencyRatio)
  val wrensHistory =
    Vec(Vec(Bool(), (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 2), frequencyRatio)

  def wrdataPhase(i: Int) = io.output(i)
  for (i <- 0 until (frequencyRatio)) {
    wrensHistory(i) := History(
      wrens(i),
      0 to (dfiConfig.timeConfig.cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 1,
      init = False
    )
  }
  wrens.foreach(_.clear())
  wrens.foreach(_.setWhen(write))
  io.input.ready.clear()
  io.input.ready.setWhen(wrensHistory(nextPhase)(delayCyc))
  assert(!(!io.input.valid && io.input.ready), "SDRAM write data stream starved !", ERROR)
  for (i <- 0 until (frequencyRatio)) {
    if (i >= nextPhase) {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc)
      wrdataPhase(i).wrData := Vec(io.input.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
        (t + nextPhase) % frequencyRatio
      )(i)
      wrdataPhase(i).wrDataMask := Vec(io.input.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
        (t + nextPhase) % frequencyRatio
      )(i)
    } else {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc + 1)
      wrdataPhase(i).wrData := RegNext(
        Vec(io.input.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
      wrdataPhase(i).wrDataMask := RegNext(
        Vec(io.input.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
    }
  }
}
