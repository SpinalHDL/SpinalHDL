package spinal.lib.memory.sdram.xdr.Dfi.Interface

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
//import spinal.lib.memory.sdram.xdr.InitCmd
//import spinal.lib.memory.sdram.SdramLayout
//import spinal.lib.memory.sdram.xdr.{PhyLayout}
case class CoreConfig(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

//  val writeLatency = UInt(log2Up(cp.writeLatencies.size) bits)
//  val readLatency = UInt(log2Up(cp.readLatencies.size) bits)
  val sdram = cpa.pl.sdram
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
//  val RAS, RP, WR, RCD, WTR, RTP, RRD, RTW = UInt(cp.timingWidth bits)
//  val RFC = UInt(cp.timingWidth+3 bits)
//  val ODT = generation.ODT generate UInt(cp.timingWidth bits)
//  val ODTend = generation.ODT generate Bits(pl.phaseCount bits)
//  val FAW = generation.FAW generate UInt(cp.timingWidth bits)
//  val REF = UInt(cp.refWidth bits)
//  val autoRefresh, noActive = Bool()
//
//  val phase = new Bundle {
//    val active = UInt(log2Up(cpa.pl.phaseCount) bits)
//    val precharge = UInt(log2Up(cpa.pl.phaseCount) bits)
//    val read = UInt(log2Up(cpa.pl.phaseCount) bits)
//    val write = UInt(log2Up(cpa.pl.phaseCount) bits)
//  }
}
case class SdramAddress(l : SdramLayout) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val row    = UInt(l.rowWidth bits)
}
case class BusAddress(l : SdramLayout, config:DfiConfig) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val row    = UInt(l.rowWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val csAddr = UInt(log2Up(config.chipSelectNumber) bits)
}





case class CoreTask(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

  //  val kind = FrontendCmdOutputKind()
  val read, write, active, precharge = Bool() //OH encoded
  val last = Bool()
  val address = BusAddress(pl.sdram,config)
  //  val portId = UInt(log2Up(cpp.size) bits)
  val context = Bits(backendContextWidth bits)
}

case class CoreTasks(cpa : CoreParameterAggregate) extends Bundle with IMasterSlave {
  val task = CoreTask(cpa)
  val prechargeAll, refresh = Bool() //OH encoded

  def init(): CoreTasks ={
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

case class CoreParameterAggregate(cp : CoreParameter, pl : PhyLayout, cpp : CorePortParameter, config: DfiConfig){
  def backendContextWidth = cpp.contextWidth
//  def portCount = cpp.size
  def generation = pl.sdram.generation
  def stationLengthWidth = log2Up(stationLengthMax)
  def stationLengthMax = cp.bytePerTaskMax/pl.bytePerBurst
  def chipSelectWidth = log2Up(config.chipSelectNumber)
  def addressWidth = pl.sdram.byteAddressWidth+chipSelectWidth
}
case class CoreParameter(
//                          stationCount  : Int = 2,
                          bytePerTaskMax : Int = 64,
                          timingWidth : Int,
                          refWidth : Int
//                          writeLatencies : List[Int],
//                          readLatencies : List[Int]){
                        ){
  assert(isPow2(bytePerTaskMax))
  //  assert(isPow2(frustrationMax))
  //  def frustrationWidth = log2Up(frustrationMax + 1)
}

case class CorePortParameter(contextWidth : Int,
                             writeTockenInterfaceWidth : Int,
                             writeTockenBufferSize : Int,
                             canRead : Boolean,
                             canWrite : Boolean)

case class CorePort(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreCmd(cpp, cpa))
  val writeData = cpp.canWrite generate Stream(CoreWriteData(cpp, cpa))
  val writeDataTocken = cpp.canWrite generate (out UInt(cpp.writeTockenInterfaceWidth bits))
  val rsp = Stream(Fragment(CoreRsp(cpp, cpa)))

  val writeDataAdded = UInt(cpp.writeTockenInterfaceWidth bits)

  override def asMaster(): Unit = {
    master(cmd)
    masterWithNull(writeData)
    out(writeDataAdded)
    outWithNull(writeDataTocken)
    slave(rsp)
  }
}

case class TaskPort(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle with IMasterSlave{
  val tasks = CoreTasks(cpa)
  val writeData = cpp.canWrite generate Stream(CoreWriteData(cpp, cpa))
  val rsp = Stream(Fragment(CoreRsp(cpp, cpa)))

  val writeDataAdded = UInt(cpp.writeTockenInterfaceWidth bits)

  override def asMaster(): Unit = {
    masterWithNull(writeData)
    slave(rsp)
    out(tasks)
  }
}

case class CoreCmd(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  import cpa._
  val write = Bool()
  val address = UInt(addressWidth bits)
  val context = Bits(cpp.contextWidth bits)
  val burstLast = Bool()
  val length = UInt(cpa.stationLengthWidth bits)
}
case class CoreWriteData(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  import cpa._
  val data = Bits(pl.beatWidth bits)
  val mask = Bits(pl.beatWidth/8 bits)
}
case class CoreRsp(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  val data = cpp.canRead generate Bits(cpa.pl.beatWidth bits)
  val context = Bits(cpp.contextWidth bits)
}

case class PhyLayout(sdram : SdramLayout,
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