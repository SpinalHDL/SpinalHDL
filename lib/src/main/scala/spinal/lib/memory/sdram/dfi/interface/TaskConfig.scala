package spinal.lib.memory.sdram.dfi.interface

import spinal.core._
import spinal.lib._

case class TaskTimingConfig(dc: DfiConfig) extends Bundle {

  import dc._

  def RAS = time(sdram.tRAS)

  def RP = time(sdram.tRP)

  def WR = time(sdram.tWR)

  def RCD = time(sdram.tRCD)

  def WTR = time(sdram.tWTR)

  def time(tcyc: Int, phase: Int = dc.frequencyRatio) = (tcyc + phase - 1) / phase

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

case class BusAddress(dc: DfiConfig) extends Bundle {
  import dc.sdram._
  val byte = UInt(log2Up(bytePerWord) bits)
  val column = UInt(columnWidth bits)
  val bank = UInt(bankWidth bits)
  val row = UInt(rowWidth bits)
  val cs = UInt(log2Up(dc.chipSelectNumber) bits)
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

case class TaskWrRdCmd(tc: TaskConfig, dc: DfiConfig) extends Bundle {

  import dc._
  import tc._
  val write = Bool()
  val address = UInt(taskAddressWidth bits)
  val context = Bits(contextWidth bits)
  val burstLast = Bool()
  val length = UInt(stationLengthWidth bits)

  def stationLengthWidth = log2Up(stationLengthMax)

  def stationLengthMax = taskParameter.bytePerTaskMax / dc.bytePerBurst
}

case class TaskWriteData(dc: DfiConfig) extends Bundle {

  import dc._

  val data = Bits(beatWidth bits)
  val mask = Bits(beatWidth / 8 bits)
}

case class TaskRsp(tc: TaskConfig, dc: DfiConfig) extends Bundle {
  val data = tc.canRead generate Bits(dc.beatWidth bits)
  val context = Bits(tc.contextWidth bits)
}

case class PreTaskPort(tc: TaskConfig, dc: DfiConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(TaskWrRdCmd(tc, dc))
  val writeData = tc.canWrite generate Stream(TaskWriteData(dc))
  val writeDataToken = tc.canWrite generate Stream(Event)
  val rsp = Stream(Fragment(TaskRsp(tc, dc)))

  override def asMaster(): Unit = {
    master(cmd, writeDataToken)
    masterWithNull(writeData)
    //    outWithNull(writeDataToken)
    slave(rsp)
  }
}

case class OpTasks(tc: TaskConfig, dc: DfiConfig) extends Bundle with IMasterSlave {

  import tc._

  val read, write, active, precharge = Bool() // OH encoded
  val last = Bool()
  val address = BusAddress(dc)
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

case class TaskPort(tc: TaskConfig, dc: DfiConfig) extends Bundle with IMasterSlave {
  val tasks = OpTasks(tc, dc)
  val writeData = tc.canWrite generate Stream(TaskWriteData(dc))
  val rsp = Stream(Fragment(TaskRsp(tc, dc)))

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    master(tasks)
  }
}
