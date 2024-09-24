package spinal.lib.memory.sdram.dfi.foundation

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface._

case class CmdTxd(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val task = slave(OpTasks(tpa))
    val cmd = Vec(master(Flow(DfiCmd(config))), config.frequencyRatio)
    val address = Vec(master(Flow(DfiAddr(config))), config.frequencyRatio)
  }
  def cmdphase(i: Int) = io.cmd(i)
  def addrphase(i: Int) = io.address(i)

  def ACTIVE: Bits = (~(B(1) << io.task.address.cs).resize(config.chipSelectNumber) ## B"b011").setName("ACTIVE")
  def WRITE: Bits = (~(B(1) << io.task.address.cs).resize(config.chipSelectNumber) ## B"b100").setName("WRITE")
  def READ: Bits = (~(B(1) << io.task.address.cs).resize(config.chipSelectNumber) ## B"b101").setName("READ")
  def PRECHARGE: Bits = (~(B(1) << io.task.address.cs).resize(config.chipSelectNumber) ## B"b010")
  def REFRESH: Bits = (~(B(1) << io.task.address.cs).resize(config.chipSelectNumber) ## B"b001").setName("REFRESH")

  def AUTO_PRECHARGE_BIT = 10 // Disable auto precharge (auto close of row)
  def ALL_BANKS_BIT = 10 // Precharge all banks
  def COULMNRang = 0 until (config.sdram.columnWidth)
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
    cmdphase(config.cmdPhase).valid.set()
    cmdphase(config.cmdPhase).payload.assignFromBits(ACTIVE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address := ROW.resized
  }
  when(write) {
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(WRITE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(read) {
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(READ)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(precharge) {
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGE"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).clear()
  }
  when(prechargeAll) {
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGEALL"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).set()

  }
  when(refresh) {
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(REFRESH)
    addrphase(config.cmdPhase).valid.set()
  }
}

case class RdDataRxd(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  import tpa.config._
  val io = new Bundle {
    val task = slave(OpTasks(tpa))
    val idfiRdData = Vec(slave(Stream(Fragment(DfiRdData(config)))), config.frequencyRatio)
    val rden = out Vec (Bool(), frequencyRatio)
    val taskRdData = master(Flow(Fragment(TaskRsp(tpp, tpa))))
  }
  val rspPipeline = new Area {
    val input = Flow(Fragment(Context()))
    assert(timeConfig.tPhyRdlat + timeConfig.tRddataEn >= 1)
    val cmd = input.toStream.queueLowLatency(
      1 << log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + config.beatCount - 1) / config.beatCount + 1),
      latency = 1
    )

    val rdensHistory = Vec(Vec(Bool(), (cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 2), frequencyRatio)
    rdensHistory.foreach(_ := History(input.valid, 0 to (cmdPhase + timeConfig.tRddataEn) / frequencyRatio + 1))
    rdensHistory.foreach(_.tail.foreach(_ init (False)))

    val beatCounter = Counter(config.beatCount, io.idfiRdData.map(_.valid).orR)

    val delayCyc = timeConfig.tRddataEn / config.frequencyRatio
    val nextPhase = (cmdPhase + timeConfig.tRddataEn) % config.frequencyRatio

    for (i <- 0 until (frequencyRatio)) {
      if (i >= nextPhase) {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc), 0 until (config.beatCount)).orR
      } else {
        io.rden(i) := History(rdensHistory(nextPhase)(delayCyc + 1), 0 until (config.beatCount)).orR
      }
    }

    val output = Flow(Fragment(PipelineRsp()))
    output.valid.clear()
    output.valid.setWhen(io.idfiRdData.map(_.valid).orR)
    output.context := cmd.context
    output.last := beatCounter.willOverflowIfInc && cmd.last
    cmd.ready := beatCounter.willOverflow

    for ((outputData, phase) <- (output.data.subdivideIn(frequencyRatio slices).reverse, io.idfiRdData).zipped) {
      outputData := B(phase.rdData)
    }
  }
  val rspPop = rspPipeline.output.stage()
  val ready = Vec(Reg(Bool()), frequencyRatio)

  rspPipeline.input.valid := False
  rspPipeline.input.last := io.task.last
  rspPipeline.input.context := io.task.context

  case class PipelineRsp() extends Bundle {
    val data = Bits(config.beatWidth bits)
    val context = Bits(contextWidth bits)
  }
  io.taskRdData.valid := rspPop.valid
  io.taskRdData.last := rspPop.last
  if (io.taskRdData.tpp.canRead) io.taskRdData.data := rspPop.data
  io.taskRdData.context := rspPop.context.resized

  rspPipeline.input.valid.setWhen(io.task.read)

  case class Context() extends Bundle {
    val context = Bits(contextWidth bits)
  }
  ready.foreach(_.init(False))
  for (i <- 0 until (frequencyRatio)) {
    ready(i).setWhen(io.task.read).clearWhen(io.task.write)
  }
  for ((outport, iready) <- (io.idfiRdData, ready).zipped) {
    outport.ready := iready
  }
}

case class WrDataTxd(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  import tpa.config._
  val io = new Bundle {
    val write = in Bool ()
    val taskWrData = slave(Stream(TaskWriteData(tpp, tpa)))
    val idfiWrData = Vec(master(Flow(DfiWrData(config))), config.frequencyRatio)
  }
  val delayCyc = timeConfig.tPhyWrLat / config.frequencyRatio
  val nextPhase = (cmdPhase + timeConfig.tPhyWrLat) % config.frequencyRatio
  val writeHistory = History(io.write, 0 until config.beatCount)
  val write = writeHistory.orR
  val wrens = Vec(Bool(), frequencyRatio)
  val wrensHistory = Vec(Vec(Bool(), (cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 2), frequencyRatio)

  def wrdataPhase(i: Int) = io.idfiWrData(i)
  for (i <- 0 until (frequencyRatio)) {
    wrensHistory(i) := History(wrens(i), 0 to (cmdPhase + timeConfig.tPhyWrLat) / frequencyRatio + 1)
    wrensHistory(i).tail.foreach(_.init(False))
  }
  wrens.foreach(_.clear())
  wrens.foreach(_.setWhen(write))
  io.taskWrData.ready.clear()
  io.taskWrData.ready.setWhen(wrensHistory(nextPhase)(delayCyc))
  assert(!(!io.taskWrData.valid && io.taskWrData.ready), "SDRAM write data stream starved !", ERROR)
  for (i <- 0 until (frequencyRatio)) {
    if (i >= nextPhase) {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc)
      wrdataPhase(i).wrData := Vec(io.taskWrData.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
        (t + nextPhase) % frequencyRatio
      )(i)
      wrdataPhase(i).wrDataMask := Vec(io.taskWrData.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(
        t => (t + nextPhase) % frequencyRatio
      )(i)
    } else {
      wrdataPhase(i).valid := wrensHistory(nextPhase)(delayCyc + 1)
      wrdataPhase(i).wrData := RegNext(
        Vec(io.taskWrData.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
      wrdataPhase(i).wrDataMask := RegNext(
        Vec(io.taskWrData.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t =>
          (t + nextPhase) % frequencyRatio
        )(i)
      )
    }
  }
}
