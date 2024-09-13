package spinal.lib.memory.sdram.Dfi.Interface

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
  val row    = UInt(l.rowWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val csAddr = UInt(log2Up(config.chipSelectNumber) bits)
}


case class PortTask(tpa : TaskParameterAggregate) extends Bundle {
  import tpa._

  val read, write, active, precharge = Bool() //OH encoded
  val last = Bool()
  val address = BusAddress(pl.sdram,config)
  val context = Bits(backendContextWidth bits)
}

case class PortTasks(tpa : TaskParameterAggregate) extends Bundle with IMasterSlave {
  val task = PortTask(tpa)
  val prechargeAll, refresh = Bool() //OH encoded

  def init(): PortTasks ={
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

case class TaskParameterAggregate(tp : TaskParameter, pl : PhyConfig, tpp : TaskPortParameter, config: DfiConfig){
  def backendContextWidth = tpp.contextWidth
  def generation = pl.sdram.generation
  def stationLengthWidth = log2Up(stationLengthMax)
  def stationLengthMax = tp.bytePerTaskMax/pl.bytePerBurst
  def chipSelectWidth = log2Up(config.chipSelectNumber)
  def addressWidth = pl.sdram.byteAddressWidth+chipSelectWidth
}
case class TaskParameter(bytePerTaskMax : Int = 64,
                         timingWidth : Int,
                         refWidth : Int){
  assert(isPow2(bytePerTaskMax))
}

case class TaskPortParameter(contextWidth : Int,
                             writeTockenInterfaceWidth : Int,
                             writeTockenBufferSize : Int,
                             canRead : Boolean,
                             canWrite : Boolean)

case class TaskCmdPort(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Stream(TaskCmd(tpp, tpa))
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val writeDataTocken = tpp.canWrite generate (out UInt(tpp.writeTockenInterfaceWidth bits))
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  val writeDataAdded = UInt(tpp.writeTockenInterfaceWidth bits)

  override def asMaster(): Unit = {
    master(cmd)
    masterWithNull(writeData)
    out(writeDataAdded)
    outWithNull(writeDataTocken)
    slave(rsp)
  }
}

case class TaskPort(tpp : TaskPortParameter, tpa : TaskParameterAggregate) extends Bundle with IMasterSlave{
  val tasks = PortTasks(tpa)
  val writeData = tpp.canWrite generate Stream(TaskWriteData(tpp, tpa))
  val rsp = Stream(Fragment(TaskRsp(tpp, tpa)))

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    master(tasks)
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


case class InitCmd(config: DfiConfig) extends Bundle{
  val ADDR  = Bits(config.addressWidth bits)
  val BA    = Bits(config.bankWidth bits)
  val CASn  = Bool()
  val CSn   = Bits(config.chipSelectNumber bits)
  val RASn  = Bool()
  val WEn   = Bool()
}
case class InitBus(config: DfiConfig) extends Bundle with IMasterSlave{
  val cmd = Flow(InitCmd(config))
  val CKE = Bool()
  val RESETn = config.useResetN generate Bool()

  def driveFrom(mapper : BusSlaveFactory): Unit ={
    val valid = RegNext(mapper.isWriting(0x00)) init(False)
    cmd.valid := valid
    mapper.drive(
      address = 0x04,
      4 -> cmd.CSn,
      3 -> cmd.RASn,
      2 -> cmd.CASn,
      1 -> cmd.WEn
    )
    mapper.drive(cmd.ADDR, 0x08)
    mapper.drive(cmd.BA, 0x0C)

    if(RESETn != null) mapper.drive(RESETn, 0x10, 0) init(False)
    mapper.drive(CKE, 0x10, 1) init(False)
  }

  override def asMaster(): Unit = {
    master(cmd)
    out(CKE)
    outWithNull(RESETn)
  }
}