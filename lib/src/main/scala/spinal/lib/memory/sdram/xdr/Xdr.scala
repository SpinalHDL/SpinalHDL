package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.io.TriState
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface


case class PhyParameter(sdram : SdramLayout,
                        phaseCount : Int,
                        dataRatio : Int,
                        outputLatency : Int,
                        inputLatency : Int,
                        burstLength : Int,
                        CCD : Int){
  import sdram._
  def beatWidth = phaseCount * dataRatio * dataWidth
  def beatCount = burstLength / phaseCount
  def wordWidth = dataWidth*burstLength
  def bytePerDq = dataWidth/8
  def bytePerWord = wordWidth/8
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerDq)
  def chipAddressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
  def columnSize = 1 << columnWidth
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

case class SdramXdrPhyCtrlPhase(pl : PhyParameter) extends Bundle with IMasterSlave{
  val CASn  = Bool()
  val CKE   = Bool()
  val CSn   = Bool()
  val RASn  = Bool()
  val WEn   = Bool()
  val RESETn = pl.sdram.generation.RESETn generate Bool()
  val ODT = pl.sdram.generation.ODT generate Bool()

  val DM    = Vec(Bits(pl.bytePerDq bits), pl.dataRatio)
  val DQw, DQr = Vec(Bits(pl.sdram.dataWidth bits), pl.dataRatio)

  override def asMaster(): Unit = {
    out(CASn,CKE,CSn,DM,RASn,WEn)
    out(DQw)
    outWithNull(RESETn, ODT)
    in(DQr)
  }
}

case class SdramXdrPhyCtrl(pl : PhyParameter) extends Bundle with IMasterSlave{
  val phases = Vec(SdramXdrPhyCtrlPhase(pl), pl.phaseCount)
  val ADDR  = Bits(pl.chipAddressWidth bits)
  val BA    = Bits(pl.sdram.bankWidth bits)
  val DQe = Bool()
  val DQS = pl.sdram.generation.DQS generate new Bundle {
    val preamble = Bool()
    val active = Bool()
    val postamble = Bool()
  }
  override def asMaster(): Unit = {
    phases.foreach(master(_))
    out(ADDR,BA,DQe)
    if(pl.sdram.generation.DQS) out(DQS)
  }
}

abstract class Phy[T <: Data with IMasterSlave](val pl : PhyParameter) extends Component{
  def MemoryBus() : T
  def driveFrom(mapper : BusSlaveFactory) : Unit

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val memory = master(MemoryBus())
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
  val RESETn = out Bool()

  val DM   = Bits(g.bytePerWord bits)
  val DQ    = Analog(Bits(g.dataWidth bits))
  val DQS, DQSn = Analog(Bits((g.dataWidth + 7) / 8 bits))

  override def asMaster(): Unit = {
    out(ADDR,BA,CASn,CKE,CSn,DM,RASn,WEn,CK,CKn,ODT,RESETn)
    inout(DQ, DQS, DQSn)
  }
}







case class CorePortParameter( contextWidth : Int)

case class CorePort(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Stream(Fragment(CoreCmd(cpp, cpa)))
  val rsp = Stream(Fragment(CoreRsp(cpp, cpa)))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

case class CoreCmd(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  import cpa._
  val write = Bool()
  val address = UInt(pl.byteAddressWidth bits)
  val data = Bits(pl.beatWidth bits)
  val mask = Bits(pl.beatWidth/8 bits)
  val context = Bits(cpp.contextWidth bits)
  val burstLast = Bool()
}
case class CoreRsp(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Bundle{
  val data = Bits(cpa.pl.beatWidth bits)
  val context = Bits(cpp.contextWidth bits)
}

case class SdramAddress(l : SdramLayout) extends Bundle {
  val byte   = UInt(log2Up(l.bytePerWord) bits)
  val column = UInt(l.columnWidth bits)
  val bank   = UInt(l.bankWidth bits)
  val row    = UInt(l.rowWidth bits)
}



                          //max(Time, cycle)
case class SdramTiming(RFC : (TimeNumber, Int), // Command Period (REF to ACT)
                       RAS : (TimeNumber, Int), // Command Period (ACT to PRE)   Per bank
                       RP  : (TimeNumber, Int), // Command Period (PRE to ACT)
                       RCD : (TimeNumber, Int), // Active Command To Read / Write Command Delay Time
                       WTR : (TimeNumber, Int), // WRITE to READ
                       WR  : (TimeNumber, Int), // WRITE to PRE (WRITE recovery time)
//                       RTW : (TimeNumber, Int), // READ to WRITE
                       RTP : (TimeNumber, Int), // READ to PRE
                       RRD : (TimeNumber, Int), // ACT to ACT cross bank
                       REF : (TimeNumber, Int)) // Refresh Cycle Time (that cover all row)
//                       FAW : (TimeNumber, Int)) // Four ACTIVATE windows

object SoftConfig{
  def apply(timing : SdramTiming, frequancy : HertzNumber, cpa : CoreParameterAggregate): SoftConfig = {
    implicit def toCycle(spec : (TimeNumber, Int)) = Math.max(0, Math.max((spec._1 * frequancy).toDouble.ceil.toInt, spec._2)-1)
    SoftConfig(
      RFC = timing.RFC,
      RAS = timing.RAS,
      RP  = timing.RP ,
      WR  = timing.WR ,
      RCD = timing.RCD,
      WTR = timing.WTR,
      RTP = timing.RTP,
      RRD = timing.RRD,
      REF = timing.REF / cpa.pl.sdram.rowSize
    )
  }
}
case class SoftConfig(RFC: Int,
                      RAS: Int,
                      RP: Int,
                      WR: Int,
                      RCD: Int,
                      WTR: Int,
                      RTP: Int,
                      RRD: Int,
                      REF : Int)

case class CoreConfig(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

  val commandPhase = UInt(log2Up(pl.phaseCount) bits)
  val writeLatency = UInt(log2Up(cp.writeLatencies.size) bits)
  val readLatency = UInt(log2Up(cp.readLatencies.size) bits)
  val RFC, RAS, RP, WR, RCD, WTR, RTP, RRD, RTW = UInt(cp.timingWidth bits)
  val ODT = generation.ODT generate UInt(cp.timingWidth bits)
  val ODTend = generation.ODT generate Bits(pl.phaseCount bits)
  val FAW = generation.FAW generate UInt(cp.timingWidth bits)
  val REF = UInt(cp.refWidth bits)
  val autoRefresh = Bool()

  def driveFrom(mapper : BusSlaveFactory) = new Area {
    mapper.drive(commandPhase, 0x00, 0)
    mapper.drive(writeLatency, 0x00, 16)
    mapper.drive(readLatency,  0x00, 24)
    mapper.drive(autoRefresh,  0x04,  0) init(False)

    mapper.drive(REF, 0x10,  0)

    mapper.drive(RAS, 0x20,  0)
    mapper.drive(RP , 0x20,  8)
    mapper.drive(RFC, 0x20, 16)
    mapper.drive(RRD, 0x20, 24)

    mapper.drive(RCD, 0x24,  0)

    mapper.drive(RTW, 0x28,  0)
    mapper.drive(RTP, 0x28,  8)
    mapper.drive(WTR, 0x28, 16)
    mapper.drive(WR , 0x28, 24)


    if(generation.FAW) mapper.drive(FAW, 0x30,  0)
    if(generation.ODT) {
      mapper.drive(ODT, 0x34,  0)
      mapper.drive(ODTend, 0x34,  8)
    }
  }
}

case class CoreParameter(portTockenMin : Int,
                         portTockenMax : Int,
                         rspFifoSize : Int,
                         timingWidth : Int,
                         refWidth : Int,
                         writeLatencies : List[Int],
                         readLatencies : List[Int])

object FrontendCmdOutputKind extends SpinalEnum{
  val READ, WRITE, ACTIVE, PRECHARGE, REFRESH = newElement()
}
case class CoreTask(cpa : CoreParameterAggregate) extends Bundle {
  import cpa._

  val kind = FrontendCmdOutputKind()
  val all = Bool()
  val address = SdramAddress(pl.sdram)
  val data = Bits(pl.beatWidth bits)
  val mask = Bits(pl.beatWidth/8 bits)
  val source = UInt(log2Up(cpp.size) bits)
  val context = Bits(backendContextWidth bits)
}



case class InitCmd(cpa : CoreParameterAggregate) extends Bundle{
  val ADDR  = Bits(cpa.pl.chipAddressWidth bits)
  val BA    = Bits(cpa.pl.sdram.bankWidth bits)
  val CASn  = Bool
  val CKE   = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool
}

case class SoftBus(cpa : CoreParameterAggregate) extends Bundle with IMasterSlave{
  val cmd = Flow(InitCmd(cpa))
  val CKE = Bool()
  val RESETn = cpa.pl.sdram.generation.RESETn generate Bool()

  def driveFrom(mapper : BusSlaveFactory): Unit ={
    val valid = RegNext(mapper.isWriting(0x00))
    cmd.valid := valid
    mapper.drive(
      address = 0x04,
      0 -> cmd.CKE,
      1 -> cmd.CSn,
      2 -> cmd.RASn,
      3 -> cmd.CASn,
      4 -> cmd.WEn
    )
    mapper.drive(cmd.ADDR, 0x08)
    mapper.drive(cmd.BA, 0x0C)

    mapper.drive(RESETn, 0x10, 0)
    mapper.drive(CKE, 0x10, 1)
  }

  override def asMaster(): Unit = {
    master(cmd)
    out(CKE)
    outWithNull(RESETn)
  }
}



case class BmbToCorePort(ip : BmbParameter, cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(ip))
    val inputBurstLast = in Bool()
    val output = master(CorePort(cpp, cpa))
  }

  case class Context() extends Bundle{
    val input = Bits(ip.contextWidth bits)
    val source = UInt(ip.sourceWidth bits)
  }

  val cmdContext = Context()
  cmdContext.input := io.input.cmd.context
  cmdContext.source := io.input.cmd.source


  io.output.cmd.arbitrationFrom(io.input.cmd)
  io.output.cmd.last := io.input.cmd.last
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  io.output.cmd.data := io.input.cmd.data
  io.output.cmd.mask := io.input.cmd.mask
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.burstLast := io.inputBurstLast


  val rspContext =io.output.rsp.context.as(Context())

  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.last := io.output.rsp.last
  io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.input
  io.input.rsp.source := rspContext.source
}