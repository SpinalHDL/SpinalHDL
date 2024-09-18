package spinal.lib.memory.sdram.dfi.interface

import spinal.core._
import spinal.lib._

case class TaskTimingConfig(tpa: TaskParameterAggregate) extends Bundle {

  import tpa._

  val sdram = tpa.pl.sdram

  def RAS = time(sdram.tRAS)

  def RP = time(sdram.tRP)

  def WR = time(sdram.tWR)

  def time(tcyc: Int, phase: Int = config.frequencyRatio) = (tcyc + phase - 1) / phase

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

case class BusAddress(l: SdramConfig, config: DfiConfig) extends Bundle {
  val byte = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank = UInt(l.bankWidth bits)
  val row = UInt(l.rowWidth bits)
  val cs = UInt(log2Up(config.chipSelectNumber) bits)
}

case class TaskParameter(bytePerTaskMax: Int = 64,
                         timingWidth: Int,
                         refWidth: Int) {
  assert(isPow2(bytePerTaskMax))
}

case class TaskPortParameter(contextWidth: Int,
                             writeTokenInterfaceWidth: Int,
                             writeTokenBufferSize: Int,
                             canRead: Boolean,
                             canWrite: Boolean)

case class TaskParameterAggregate(tp: TaskParameter, pl: PhyConfig, tpp: TaskPortParameter, config: DfiConfig) {
  def backendContextWidth = tpp.contextWidth

  def generation = pl.sdram.generation

  def stationLengthWidth = log2Up(stationLengthMax)

  def stationLengthMax = tp.bytePerTaskMax / pl.bytePerBurst

  def addressWidth = pl.sdram.byteAddressWidth + chipSelectWidth

  def chipSelectWidth = log2Up(config.chipSelectNumber)
}

case class TaskWrRdCmd(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Bundle {

  import tpa._

  val write = Bool()
  val address = UInt(addressWidth bits)
  val context = Bits(tpp.contextWidth bits)
  val burstLast = Bool()
  val length = UInt(tpa.stationLengthWidth bits)
}

case class TaskWriteData(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Bundle {

  import tpa._

  val data = Bits(pl.beatWidth bits)
  val mask = Bits(pl.beatWidth / 8 bits)
}

case class TaskRsp(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Bundle {
  val data = tpp.canRead generate Bits(tpa.pl.beatWidth bits)
  val context = Bits(tpp.contextWidth bits)
}

case class PreTaskPort(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Bundle with IMasterSlave {
  val cmd = Stream(TaskWrRdCmd(tpp, tpa))
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val writeDataToken = tpp.canWrite generate Stream(Event)
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  override def asMaster(): Unit = {
    master(cmd, writeDataToken)
    masterWithNull(writeData)
    //    outWithNull(writeDataToken)
    slave(rsp)
  }
}

case class OpTasks(tpa: TaskParameterAggregate) extends Bundle with IMasterSlave {

  import tpa._

  val read, write, active, precharge = Bool() //OH encoded
  val last = Bool()
  val address = BusAddress(pl.sdram, config)
  val context = Bits(backendContextWidth bits)
  val prechargeAll, refresh = Bool() //OH encoded

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

case class TaskPort(tpp: TaskPortParameter, tpa: TaskParameterAggregate) extends Bundle with IMasterSlave {
  val tasks = OpTasks(tpa)
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    master(tasks)
  }
}

case class PhyConfig(sdram: SdramConfig,
                     phaseCount: Int, //How many DRAM clock per core clock
                     dataRate: Int, //(SDR=1, DDR=2, QDR=4)
                     outputLatency: Int, //Max delay for a command on the phy to arrive on the sdram
                     readDelay: Int, //Max delay between readEnable and readValid
                     writeDelay: Int, //Delay between writeEnable and data/dm
                     cmdToDqDelayDelta: Int, //How many cycle extra the DQ need to be on the pin compared to CAS/RAS
                     transferPerBurst: Int) { //How many transfer per burst

  import sdram._

  def phyIoWidth = dataRate * dataWidth

  def beatCount = transferPerBurst / phaseCount / dataRate

  def bytePerDq = dataWidth / 8

  def bytePerBurst = burstWidth / 8

  def burstWidth = dataWidth * transferPerBurst

  def bytePerBeat = beatWidth / 8

  def beatWidth = phaseCount * dataRate * dataWidth
}