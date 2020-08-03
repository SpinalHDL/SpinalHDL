package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriState
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface


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
  def beatCount = transferPerBurst / phaseCount / dataRate
  def burstWidth = dataWidth*transferPerBurst
  def bytePerDq = dataWidth/8
  def bytePerBurst = burstWidth/8
  def bytePerBeat = beatWidth/8
}
case class Timing()
case class Timings(      bootRefreshCount : Int, // Number of refresh command done in the boot sequence
                         tPOW  : TimeNumber,     // Powerup time
                         tREF  : TimeNumber,     // Refresh Cycle Time (that cover all row)
//                         tRC   : TimeNumber,     // Command Period (ACT to ACT)   Per bank
                         tRFC  : TimeNumber,     // Command Period (REF to REF)   Per bank
                         tRAS  : TimeNumber,     // Command Period (ACT to PRE)   Per bank
                         tRP   : TimeNumber,     // Command Period (PRE to ACT) Per bank
                         tRCD  : TimeNumber,     // ACT To READ / WRITE Command Delay Time per bank
                         cMRD  : Int,            // Mode Register Program Time
                         tWR   : TimeNumber,     // WRITE recovery time (WRITE to PRE) per bank
                         cWR   : Int)            // WRITE recovery cycle (WRITE to PRE) per bank
//tWTR //WRITE to READ cross bank
//tCCD //CAS to CAS cross bank
//tRRD //Active to Active cross bank
//tFAW //Four ACTIVATE windows
//RTP READ to PRE

//RFC, RAS, RP, WR, RCD, WTR, CCD, RTP


//TODO tFAW
//TODO ctrl lock

//case class Ddr3(l : MemoryLayout) extends Bundle with IMasterSlave{
//  val ADDR  = Bits(l.chipAddressWidth bits)
//  val BA    = Bits(l.bankWidth bits)
//  val DQ    = TriState(Bits(l.dataWidth bits))
//  val DQS   = TriState(Bits(l.bytePerDq bits))
//  val DM    = Bits(l.bytePerDq bits)
//  val CASn  = Bool
//  val CKE   = Bool
//  val CSn   = Bool
//  val RASn  = Bool
//  val WEn   = Bool
//  val ODT   = Bool
//  val RESETn   = Bool
//
//  override def asMaster(): Unit = {
//    out(ADDR,BA,CASn,CKE,CSn,DM,RASn,WEn,ODT,RESETn)
//    master(DQ)
//  }
//}
//

//case class Sdr(sl : Sdraplayout) extends Bundle with IMasterSlave{
//  val ADDR  = Bits(pl.chipAddressWidth bits)
//  val BA    = Bits(pl.bankWidth bits)
//  val DQ    = TriState(Bits(pl.dataWidth bits))
//  val DQM   = Bits(pl.bytePerWord bits)
//  val CASn  = Bool
//  val CKE   = Bool
//  val CSn   = Bool
//  val RASn  = Bool
//  val WEn   = Bool
//
//  override def asMaster(): Unit = {
//    out(ADDR,BA,CASn,CKE,CSn,DQM,RASn,WEn)
//    master(DQ)
//  }
//}

case class SdramXdrPhyCtrlPhase(pl : PhyLayout) extends Bundle with IMasterSlave{
  val CASn  = Bool()
  val CKE   = Bool()
  val CSn   = Bool()
  val RASn  = Bool()
  val WEn   = Bool()
  val RESETn = pl.sdram.generation.RESETn generate Bool()
  val ODT = pl.sdram.generation.ODT generate Bool()

  val DM       = Vec(Bits(pl.bytePerDq bits), pl.dataRate)
  val DQw, DQr = Vec(Bits(pl.sdram.dataWidth bits), pl.dataRate)

  override def asMaster(): Unit = {
    out(CASn,CKE,CSn,DM,RASn,WEn)
    out(DQw)
    outWithNull(RESETn, ODT)
    in(DQr)
  }
}

case class SdramXdrPhyCtrl(pl : PhyLayout) extends Bundle with IMasterSlave{
  val phases = Vec(SdramXdrPhyCtrlPhase(pl), pl.phaseCount)
  val ADDR  = Bits(pl.sdram.chipAddressWidth bits)
  val BA    = Bits(pl.sdram.bankWidth bits)
  val DQS = pl.sdram.generation.DQS generate new Bundle {
    val preamble = Bool()
    val active = Bool()
    val postamble = Bool()
  }
  val writeEnable = Bool()
  val readEnable = Bool()
  val readValid = Bool()
  override def asMaster(): Unit = {
    phases.foreach(master(_))
    out(ADDR,BA)
    out(readEnable, writeEnable)
    in(readValid)
    if(pl.sdram.generation.DQS) out(DQS)
  }
}




case class SdramXdrIo(g : SdramLayout) extends Bundle with IMasterSlave {
  val ADDR  = Bits(g.chipAddressWidth bits)
  val BA    = Bits(g.bankWidth bits)
  val CASn  = Bool()
  val CKE   = Bool()
  val CSn   = Bool()
  val RASn  = Bool()
  val WEn   = Bool()

  val CK, CKn = out Bool()
  val ODT = out Bool()
  val RESETn = g.generation.RESETn generate(out Bool())

  val DM   = Bits(g.bytePerWord bits)
  val DQ    = Analog(Bits(g.dataWidth bits))
  val DQS, DQSn = Analog(Bits((g.dataWidth + 7) / 8 bits))

  override def asMaster(): Unit = {
    outWithNull(ADDR,BA,CASn,CKE,CSn,DM,RASn,WEn,CK,CKn,ODT,RESETn)
    inout(DQ, DQS, DQSn)
  }
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

case class CoreCmd(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  import cpa._
  val write = Bool()
  val address = UInt(pl.sdram.byteAddressWidth bits)
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

case class SdramAddress(l : SdramLayout) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val row    = UInt(l.rowWidth bits)
}

object SdramTiming{
  val SDR = 0
  val DDR1 = 1
  val DDR2 = 2
  val DDR3 = 3
}
case class SdramTiming(generation : Int,
                       RFC : Int, // Command Period (REF to ACT)
                       RAS : Int, // Command Period (ACT to PRE)   Per bank
                       RP  : Int, // Command Period (PRE to ACT)
                       RCD : Int, // Active Command To Read / Write Command Delay Time
                       WTR : Int, // WRITE to READ
                       WTP : Int, // WRITE to PRE (WRITE recovery time)
                       RTP : Int, // READ to PRE
                       RRD : Int, // ACT to ACT cross bank
                       REF : Int, // Refresh Cycle Time (single row)
                       FAW : Int) // Four ACTIVATE windows

//object SoftConfig{
//  def apply(timing : SdramTiming,
//            frequancy : HertzNumber,
//            cpa : CoreParameterAggregate,
//            phyClockRatio : Int): SoftConfig = {
//    implicit def toCycle(spec : (TimeNumber, Int)) = Math.max(0, Math.max((spec._1 * frequancy).toDouble.ceil.toInt, (spec._2+phyClockRatio-1)/phyClockRatio))
//    SoftConfig(
//      RFC = timing.RFC,
//      RAS = timing.RAS,
//      RP  = timing.RP ,
//      WTP = timing.WTP ,
//      RCD = timing.RCD,
//      RTW = timing.RTW,
//      WTR = timing.WTR,
//      RTP = timing.RTP,
//      RRD = timing.RRD,
//      REF = timing.REF,
//      FAW = timing.FAW
//    )
//  }
//}
case class SoftConfig(RFC: Int,
                      RAS: Int,
                      RP: Int,
                      WTP: Int,
                      RCD: Int,
                      WTR: Int,
                      RTW: Int,
                      RTP: Int,
                      RRD: Int,
                      REF : Int,
                      FAW : Int)

case class CoreConfig(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

  val writeLatency = UInt(log2Up(cp.writeLatencies.size) bits)
  val readLatency = UInt(log2Up(cp.readLatencies.size) bits)
  val RAS, RP, WR, RCD, WTR, RTP, RRD, RTW = UInt(cp.timingWidth bits)
  val RFC = UInt(cp.timingWidth+3 bits)
  val ODT = generation.ODT generate UInt(cp.timingWidth bits)
  val ODTend = generation.ODT generate Bits(pl.phaseCount bits)
  val FAW = generation.FAW generate UInt(cp.timingWidth bits)
  val REF = UInt(cp.refWidth bits)
  val autoRefresh, noActive = Bool()

  val phase = new Bundle {
    val active = UInt(log2Up(cpa.pl.phaseCount) bits)
    val precharge = UInt(log2Up(cpa.pl.phaseCount) bits)
    val read = UInt(log2Up(cpa.pl.phaseCount) bits)
    val write = UInt(log2Up(cpa.pl.phaseCount) bits)
  }

  def driveFrom(mapper : BusSlaveFactory) = new Area {
    mapper.drive(autoRefresh,  0x00,  0) init(False)
    mapper.drive(noActive,     0x00,  1) init(False)
    mapper.drive(phase.write, 0x04,  0)
    mapper.drive(phase.read, 0x04,  8)
    mapper.drive(phase.active, 0x04,  16)
    mapper.drive(phase.precharge, 0x04,  24)
    mapper.drive(writeLatency, 0x08, 0) randBoot()
    mapper.drive(readLatency,  0x0C, 0) randBoot()

    mapper.drive(REF, 0x10,  0) randBoot()

    mapper.drive(RAS, 0x20,  0) randBoot()
    mapper.drive(RP , 0x20,  8) randBoot()
    mapper.drive(RFC, 0x20, 16) randBoot()
    mapper.drive(RRD, 0x20, 24) randBoot()

    mapper.drive(RCD, 0x24,  0) randBoot()

    mapper.drive(RTW, 0x28,  0) randBoot()
    mapper.drive(RTP, 0x28,  8) randBoot()
    mapper.drive(WTR, 0x28, 16) randBoot()
    mapper.drive(WR , 0x28, 24) randBoot()


    if(generation.FAW) mapper.drive(FAW, 0x30,  0) randBoot()
    if(generation.ODT) {
      mapper.drive(ODT, 0x34,  0) randBoot()
      mapper.drive(ODTend, 0x34,  8) randBoot()
    }
  }
}

case class CoreParameter(portTockenMin : Int,
                         portTockenMax : Int,
                         stationCount  : Int = 2,
                         bytePerTaskMax : Int = 64,
                         frustrationMax : Int = 8,
                         timingWidth : Int,
                         refWidth : Int,
                         writeLatencies : List[Int],
                         readLatencies : List[Int]){
  assert(isPow2(bytePerTaskMax))
  assert(isPow2(frustrationMax))
  def frustrationWidth = log2Up(frustrationMax + 1)
}

object FrontendCmdOutputKind extends SpinalEnum{
  val READ, WRITE, ACTIVE, PRECHARGE, REFRESH = newElement()
}
case class CoreTask(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

//  val kind = FrontendCmdOutputKind()
  val read, write, active, precharge = Bool() //OH encoded
  val last = Bool()
  val address = SdramAddress(pl.sdram)
  val portId = UInt(log2Up(cpp.size) bits)
  val context = Bits(backendContextWidth bits)
}

case class CoreTasks(cpa : CoreParameterAggregate) extends Bundle with IMasterSlave {
  val ports = Vec(CoreTask(cpa), cpa.cp.stationCount)
  val prechargeAll, refresh = Bool() //OH encoded

  def stage(): CoreTasks ={
    val ret = RegNext(this).setCompositeName(this, "stage", true)

    for(port <- ret.ports) {
      port.read init(False)
      port.write init(False)
      port.precharge init(False)
      port.active init(False)
    }
    ret.prechargeAll init(False)
    ret.refresh init(False)

    ret
  }
  override def asMaster(): Unit = out(this)
}


case class InitCmd(cpa : CoreParameterAggregate) extends Bundle{
  val ADDR  = Bits(cpa.pl.sdram.chipAddressWidth bits)
  val BA    = Bits(cpa.pl.sdram.bankWidth bits)
  val CASn  = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool
}

case class SoftBus(cpa : CoreParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Flow(InitCmd(cpa))
  val CKE = Bool()
  val RESETn = cpa.pl.sdram.generation.RESETn generate Bool()

  def driveFrom(mapper : BusSlaveFactory): Unit ={
    val valid = RegNext(mapper.isWriting(0x00)) init(False)
    cmd.valid := valid
    mapper.drive(
      address = 0x04,
      1 -> cmd.CSn,
      2 -> cmd.RASn,
      3 -> cmd.CASn,
      4 -> cmd.WEn
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

case class BmbToCorePort(ip : BmbParameter, cpp : CorePortParameter, cpa : CoreParameterAggregate, pp : BmbPortParameter) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool()
    val output = master(CorePort(cpp, cpa))
  }

  case class Context() extends Bundle{
    val source = UInt(ip.access.sourceWidth bits)
    val input = Bits(ip.access.contextWidth bits)
  }

  val cmdToRspCount = io.output.cmd.write ? U(1) | (io.output.cmd.length +^ 1) << log2Up(cpa.pl.beatCount)

  val rspPendingCounter = Reg(UInt(log2Up(pp.rspBufferSize + 1) bits)) init(0)
  rspPendingCounter := rspPendingCounter + (io.input.cmd.lastFire ? cmdToRspCount | U(0)) - U(io.output.rsp.fire)

  val toManyRsp = (U"0" @@ rspPendingCounter) + cmdToRspCount > pp.rspBufferSize //pp.rspBufferSize - pp.beatPerBurst*cpa.pl.beatCount //Pessimistic

  io.input.cmd.ready := io.output.cmd.ready && !toManyRsp
  if(ip.access.canWrite) io.input.cmd.ready clearWhen(!io.output.writeData.ready)

  val cmdContext = Context()
  cmdContext.input := io.input.cmd.context
  cmdContext.source := io.input.cmd.source

  io.output.cmd.valid := io.input.cmd.firstFire
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  assert(widthOf(io.output.cmd.length) >= widthOf(io.input.cmd.length) - log2Up(cpa.pl.bytePerBurst))
  io.output.cmd.length := (io.input.cmd.length >> log2Up(cpa.pl.bytePerBurst)).resized
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.burstLast := io.inputBurstLast

  if(ip.access.canWrite) {
    io.output.writeData.valid := io.input.cmd.fire && io.input.cmd.isWrite
    io.output.writeData.data := io.input.cmd.data
    io.output.writeData.mask := io.input.cmd.mask
  }

  val rspContext = io.output.rsp.context.as(Context())
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.last := io.output.rsp.last
  if(ip.access.canRead) io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.input
  io.input.rsp.source := rspContext.source
}

//case class BmbToCorePort(ip : BmbParameter, cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Component{
//  val io = new Bundle{
//    val input = slave(Bmb(ip))
//    val inputBurstLast = in Bool()
//    val output = master(CorePort(cpp, cpa))
//  }
//
//  val (cmdFork, writeFork) = StreamFork2(io.input.cmd)
//
//  case class Context() extends Bundle{
//    val input = Bits(ip.contextWidth bits)
//    val source = UInt(ip.sourceWidth bits)
//  }
//
//  val cmdContext = Context()
//  cmdContext.input := cmdFork.context
//  cmdContext.source := cmdFork.source
//
//  io.output.cmd.arbitrationFrom(cmdFork.takeWhen(cmdFork.last))
//  io.output.cmd.write := cmdFork.isWrite
//  io.output.cmd.address := cmdFork.address
//  io.output.cmd.context := B(cmdContext)
//  io.output.cmd.burstLast := io.inputBurstLast
//
//
//  io.output.writeData.arbitrationFrom(writeFork.takeWhen(writeFork.isWrite))
//  io.output.writeData.data := writeFork.data
//  io.output.writeData.mask := writeFork.mask
//
//  val rspContext = io.output.rsp.context.as(Context())
//  io.input.rsp.arbitrationFrom(io.output.rsp)
//  io.input.rsp.setSuccess()
//  io.input.rsp.last := io.output.rsp.last
//  io.input.rsp.data := io.output.rsp.data
//  io.input.rsp.context := rspContext.input
//  io.input.rsp.source := rspContext.source
//}