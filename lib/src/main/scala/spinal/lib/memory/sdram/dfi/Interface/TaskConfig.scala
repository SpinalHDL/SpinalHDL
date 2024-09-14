package spinal.lib.memory.sdram.dfi.Interface

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

case class TaskTimingConfig(tpa : TaskParameterAggregate) extends Bundle {
  import tpa._

  val sdram = tpa.pl.sdram
  def time(tcyc : Int, phase : Int=config.frequencyRatio) = (tcyc + phase - 1) / phase
  def RAS = time(sdram.tRAS)
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
//  val RAS, RP, WR, RCD, WTR, RTP, RRD, RTW = UInt(tp.timingWidth bits)
//  val RFC = UInt(tp.timingWidth+3 bits)
//  val ODT = generation.ODT generate UInt(tp.timingWidth bits)
//  val ODTend = generation.ODT generate Bits(pl.phaseCount bits)
//  val FAW = generation.FAW generate UInt(tp.timingWidth bits)
//  val REF = UInt(tp.refWidth bits)
//  val autoRefresh, noActive = Bool()

}
case class SdramAddress(l : SdramConfig) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val row    = UInt(l.rowWidth bits)
}
case class BusAddress(l : SdramConfig, config:DfiConfig) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val row    = UInt(l.rowWidth bits)
  val cs     = UInt(log2Up(config.chipSelectNumber) bits)
}


case class OpTask(tpa : TaskParameterAggregate) extends Bundle {
  import tpa._

  val read, write, active, precharge = Bool() //OH encoded
  val last = Bool()
  val address = BusAddress(pl.sdram,config)
  val context = Bits(backendContextWidth bits)
}

case class OpTasks(tpa : TaskParameterAggregate) extends Bundle with IMasterSlave {
  val task = OpTask(tpa)
  val prechargeAll, refresh = Bool() //OH encoded

  def init(): OpTasks ={
    val ret = RegNext(this).setCompositeName(this, "init", true)
    ret.task.read init(False)
    ret.task.write init(False)
    ret.task.precharge init(False)
    ret.task.active init(False)
    ret.prechargeAll init(False)
    ret.refresh init(False)
    ret
  }
  override def asMaster(): Unit = out(this)
}


case class TaskParameter(bytePerTaskMax : Int = 64,
                         timingWidth : Int,
                         refWidth : Int){
  assert(isPow2(bytePerTaskMax))
}

case class TaskPortParameter(contextWidth : Int,
                             writeTokenInterfaceWidth : Int,
                             writeTokenBufferSize : Int,
                             canRead : Boolean,
                             canWrite : Boolean)
case class TaskParameterAggregate(tp : TaskParameter, pl : PhyConfig, tpp : TaskPortParameter, config: DfiConfig){
  def backendContextWidth = tpp.contextWidth
  def generation = pl.sdram.generation
  def stationLengthWidth = log2Up(stationLengthMax)
  def stationLengthMax = tp.bytePerTaskMax/pl.bytePerBurst
  def chipSelectWidth = log2Up(config.chipSelectNumber)
  def addressWidth = pl.sdram.byteAddressWidth+chipSelectWidth
}

case class TaskPort(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle with IMasterSlave{
  val tasks = OpTasks(tpa)
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    master(tasks)
  }
}

case class TaskCmdPort(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Stream(TaskCmd(tpp, tpa))
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val writeDataToken = tpp.canWrite generate Stream(Event)
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  override def asMaster(): Unit = {
    master(cmd,writeDataToken)
    masterWithNull(writeData)
//    outWithNull(writeDataToken)
    slave(rsp)
  }
}



case class TaskCmd(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle{
  import tpa._
  val write = Bool()
  val address = UInt(addressWidth bits)
  val context = Bits(tpp.contextWidth bits)
  val burstLast = Bool()
  val length = UInt(tpa.stationLengthWidth bits)
}
case class TaskWriteData(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle{
  import tpa._
  val data = Bits(pl.beatWidth bits)
  val mask = Bits(pl.beatWidth/8 bits)
}
case class TaskRsp(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle{
  val data = tpp.canRead generate Bits(tpa.pl.beatWidth bits)
  val context = Bits(tpp.contextWidth bits)
}

case class PhyConfig(sdram : SdramConfig,
                     phaseCount : Int, //How many DRAM clock per core clock
                     dataRate : Int, //(SDR=1, DDR=2, QDR=4)
                     outputLatency : Int, //Max delay for a command on the phy to arrive on the sdram
                     readDelay : Int, //Max delay between readEnable and readValid
                     writeDelay : Int, //Delay between writeEnable and data/dm
                     cmdToDqDelayDelta : Int, //How many cycle extra the DQ need to be on the pin compared to CAS/RAS
                     transferPerBurst : Int){ //How many transfer per burst
  import sdram._
  def beatWidth = phaseCount * dataRate * dataWidth
  def phyIoWidth =  dataRate * dataWidth
  def beatCount = transferPerBurst / phaseCount / dataRate
  def burstWidth = dataWidth*transferPerBurst
  def bytePerDq = dataWidth/8
  def bytePerBurst = burstWidth/8
  def bytePerBeat = beatWidth/8
}