package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class TaskTimingConfig(dfiConfig: DfiConfig) extends Bundle {

  def RAS = time(dfiConfig.sdram.tRAS)

  def RP = time(dfiConfig.sdram.tRP)

  def WR = time(dfiConfig.sdram.tWR)

  def RCD = time(dfiConfig.sdram.tRCD)

  def WTR = time(dfiConfig.sdram.tWTR)

  def RTP = time(dfiConfig.sdram.tRTP)

  def RRD = time(dfiConfig.sdram.tRRD)

  def RTW = time(dfiConfig.sdram.tRTW)

  def RFC = time(dfiConfig.sdram.tRFC)

  def ODT = 0

  def FAW = time(dfiConfig.sdram.tFAW)

  def time(tcyc: Int, phase: Int = dfiConfig.frequencyRatio) = (tcyc + phase - 1) / phase

  def REF = time(dfiConfig.sdram.tREF)

  def autoRefresh = True
}

case class SdramAddress(l: SdramConfig) extends Bundle {
  val byte = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank = UInt(l.bankWidth bits)
  val row = UInt(l.rowWidth bits)
}

case class BusAddress(dfiConfig: DfiConfig) extends Bundle {
  val byte = UInt(log2Up(dfiConfig.sdram.bytePerWord) bits)
  val column = UInt(dfiConfig.sdram.columnWidth bits)
  val bank = UInt(dfiConfig.sdram.bankWidth bits)
  val row = UInt(dfiConfig.sdram.rowWidth bits)
  val cs = UInt(log2Up(dfiConfig.chipSelectNumber) bits)
  def getRBCAddress(addr: UInt) = addr(log2Up(dfiConfig.sdram.bytePerWord), dfiConfig.sdram.wordAddressWidth bits)
  def getButeAddress(addr: UInt) = addr(0, log2Up(dfiConfig.sdram.bytePerWord) bits)
  def getCsAddress(addr: UInt) =
    addr(dfiConfig.sdram.byteAddressWidth, log2Up(dfiConfig.chipSelectNumber) bits)
}

case class TaskParameter(
    bytePerTaskMax: Int = 64,
    timingWidth: Int,
    refWidth: Int,
    cmdBufferSize: Int,
    dataBufferSize: Int,
    rspBufferSize: Int
) {
  assert(isPow2(bytePerTaskMax))
}

case class TaskConfig(
    taskParameter: TaskParameter,
    contextWidth: Int,
    writeTokenInterfaceWidth: Int,
    writeTokenBufferSize: Int,
    canRead: Boolean,
    canWrite: Boolean
) {}

case class TaskWrRdCmd(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Bundle {
  val write = Bool()
  val address = UInt(dfiConfig.taskAddressWidth bits)
  val context = Bits(taskConfig.contextWidth bits)
  val burstLast = Bool()
  val length = UInt(stationLengthWidth bits)

  def stationLengthWidth = log2Up(stationLengthMax)

  def stationLengthMax = taskConfig.taskParameter.bytePerTaskMax / dfiConfig.bytePerBurst
}

case class TaskWriteData(dfiConfig: DfiConfig) extends Bundle {
  val data = Bits(dfiConfig.beatWidth bits)
  val mask = Bits(dfiConfig.beatWidth / 8 bits)
}

case class TaskRsp(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Bundle {
  val data = taskConfig.canRead generate Bits(dfiConfig.beatWidth bits)
  val context = Bits(taskConfig.contextWidth bits)
}

case class PreTaskPort(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(TaskWrRdCmd(taskConfig, dfiConfig))
  val writeData = taskConfig.canWrite generate Stream(TaskWriteData(dfiConfig))
  val writeDataToken = taskConfig.canWrite generate Stream(Event)
  val rsp = Stream(Fragment(TaskRsp(taskConfig, dfiConfig)))

  override def asMaster(): Unit = {
    master(cmd, writeDataToken)
    masterWithNull(writeData)
    //    outWithNull(writeDataToken)
    slave(rsp)
  }
}

case class OpTasks(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Bundle with IMasterSlave {

  val read, write, active, precharge = Bool() // OH encoded
  val last = Bool()
  val address = BusAddress(dfiConfig)
  val context = Bits(taskConfig.contextWidth bits)
  val prechargeAll, refresh = Bool() // OH encoded

  def init(): OpTasks = {
    val ret = RegNext(this).setCompositeName(this, "init", true)
    ret.read init (False)
    ret.write init (False)
    ret.precharge init (False)
    ret.active init (False)
    ret.prechargeAll init (False)
    ret.refresh init (False)
    ret
  }

  override def asMaster(): Unit = out(this)
}

case class TaskPort(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Bundle with IMasterSlave {
  val tasks = OpTasks(taskConfig, dfiConfig)
  val writeData = taskConfig.canWrite generate Stream(TaskWriteData(dfiConfig))
  val rsp = Stream(Fragment(TaskRsp(taskConfig, dfiConfig)))

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    master(tasks)
  }
}
