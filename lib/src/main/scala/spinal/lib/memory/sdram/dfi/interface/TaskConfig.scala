package spinal.lib.memory.sdram.dfi.interface

import spinal.core._
import spinal.lib._

case class TaskTimingConfig(dfiConfig: DfiConfig) extends Bundle {

  import dfiConfig._

  def RAS = time(sdram.tRAS)

  def time(tcyc: Int, phase: Int = dfiConfig.frequencyRatio) = (tcyc + phase - 1) / phase

  def RP = time(sdram.tRP)

  def WR = time(sdram.tWR)

  def RCD = time(sdram.tRCD)

  def WTR = time(sdram.tWTR)

  def RTP = time(sdram.tRTP)

  def RRD = time(sdram.tRRD)

  def RTW = time(sdram.tRTW)

  def RFC = time(sdram.tRFC)

  def ODT = 0

  def FAW = time(sdram.tFAW)

  def REF = time(sdram.tREF)

  def autoRefresh = True
}

case class SdramAddress(l: SdramConfig) extends Bundle {
  val byte = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank = UInt(l.bankWidth bits)
  val row = UInt(l.rowWidth bits)
}

case class BusAddress(dfiConfig: DfiConfig) extends Bundle {
  import dfiConfig.sdram._
  val byte = UInt(log2Up(bytePerWord) bits)
  val column = UInt(columnWidth bits)
  val bank = UInt(bankWidth bits)
  val row = UInt(rowWidth bits)
  val cs = UInt(log2Up(dfiConfig.chipSelectNumber) bits)
  def getRBCAddress(addr: UInt) = addr(log2Up(bytePerWord), columnWidth + bankWidth + rowWidth bits)
  def getButeAddress(addr: UInt) = addr(0, log2Up(bytePerWord) bits)
  def getCsAddress(addr: UInt) =
    addr(log2Up(bytePerWord) + columnWidth + bankWidth + rowWidth, log2Up(dfiConfig.chipSelectNumber) bits)
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

  import dfiConfig._
  import taskConfig._
  val write = Bool()
  val address = UInt(taskAddressWidth bits)
  val context = Bits(contextWidth bits)
  val burstLast = Bool()
  val length = UInt(stationLengthWidth bits)

  def stationLengthWidth = log2Up(stationLengthMax)

  def stationLengthMax = taskParameter.bytePerTaskMax / dfiConfig.bytePerBurst
}

case class TaskWriteData(dfiConfig: DfiConfig) extends Bundle {

  import dfiConfig._

  val data = Bits(beatWidth bits)
  val mask = Bits(beatWidth / 8 bits)
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

  import taskConfig._

  val read, write, active, precharge = Bool() // OH encoded
  val last = Bool()
  val address = BusAddress(dfiConfig)
  val context = Bits(contextWidth bits)
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
