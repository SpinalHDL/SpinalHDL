package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class MemoryLayout(bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        wordWidth : Int,
                        dataWidth : Int,
                        withDqs : Boolean,
                        burstLength : Int){
  def bytePerDq = dataWidth/8
  def bytePerWord = wordWidth/8
  def wordAddressWidth = bankWidth + columnWidth + rowWidth
  def byteAddressWidth = bankWidth + columnWidth + rowWidth + log2Up(bytePerWord)
  def chipAddressWidth = Math.max(columnWidth,rowWidth)
  def bankCount = 1 << bankWidth
  def capacity = BigInt(1) << byteAddressWidth
  def columnSize = 1 << columnWidth
}


case class PhyLayout(phaseCount : Int,
                     outputLatency : Int,
                     inputLatency : Int,
                     ml : MemoryLayout){
  def beatWidth = phaseCount * ml.dataWidth
  def beatCount = ml.burstLength / phaseCount
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



case class Ddr3(l : MemoryLayout) extends Bundle with IMasterSlave{
  val ADDR  = Bits(l.chipAddressWidth bits)
  val BA    = Bits(l.bankWidth bits)
  val DQ    = TriState(Bits(l.dataWidth bits))
  val DQS   = TriState(Bits(l.bytePerDq bits))
  val DM    = Bits(l.bytePerDq bits)
  val CASn  = Bool
  val CKE   = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool
  val ODT   = Bool
  val RESETn   = Bool

  override def asMaster(): Unit = {
    out(ADDR,BA,CASn,CKE,CSn,DM,RASn,WEn,ODT,RESETn)
    master(DQ)
  }
}


case class Sdr(ml : MemoryLayout) extends Bundle with IMasterSlave{
  val ADDR  = Bits(ml.chipAddressWidth bits)
  val BA    = Bits(ml.bankWidth bits)
  val DQ    = TriState(Bits(ml.dataWidth bits))
  val DQM   = Bits(ml.bytePerWord bits)
  val CASn  = Bool
  val CKE   = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool

  override def asMaster(): Unit = {
    out(ADDR,BA,CASn,CKE,CSn,DQM,RASn,WEn)
    master(DQ)
  }
}

case class SdramXdrPhyCtrlPhase(pl : PhyLayout) extends Bundle with IMasterSlave{
  val CASn  = Bool()
  val CKE   = Bool()
  val CSn   = Bool()
  val RASn  = Bool()
  val WEn   = Bool()

  val DM    = Bits(pl.ml.bytePerDq bits)
  val DQw, DQr = Bits(pl.ml.dataWidth bits)

  override def asMaster(): Unit = {
    out(CASn,CKE,CSn,DM,RASn,WEn)
    out(DQw)
    in(DQr)
  }
}

case class SdramXdrPhyCtrl(pl : PhyLayout) extends Bundle with IMasterSlave{
  val phases = Vec(SdramXdrPhyCtrlPhase(pl), pl.phaseCount)
  val ADDR  = Bits(pl.ml.chipAddressWidth bits)
  val BA    = Bits(pl.ml.bankWidth bits)
  val DQe = Bool()
  val DQS = pl.ml.withDqs generate new Bundle {
    val preamble = Bool()
    val active = Bool()
    val postamble = Bool()
  }
  override def asMaster(): Unit = {
    phases.foreach(master(_))
    out(ADDR,BA,DQe)
    if(pl.ml.withDqs) out(DQS)
  }
}


case class SdrInferedPhy(pl : PhyLayout) extends Component{
  require(pl.phaseCount == 1)
  require(!pl.ml.withDqs)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val memory = master(Sdr(pl.ml))
  }

  io.memory.ADDR  := RegNext(io.ctrl.ADDR)
  io.memory.BA    := RegNext(io.ctrl.BA  )
  io.memory.DQM   := RegNext(io.ctrl.phases(0).DM  )
  io.memory.CASn  := RegNext(io.ctrl.phases(0).CASn)
  io.memory.CKE   := RegNext(io.ctrl.phases(0).CKE )
  io.memory.CSn   := RegNext(io.ctrl.phases(0).CSn )
  io.memory.RASn  := RegNext(io.ctrl.phases(0).RASn)
  io.memory.WEn   := RegNext(io.ctrl.phases(0).WEn )

  io.memory.DQ.writeEnable  := RegNext(io.ctrl.DQe)
  io.memory.DQ.write        := RegNext(io.ctrl.phases(0).DQw )
  io.ctrl.phases(0).DQr     := RegNext(io.memory.DQ.read )
}

case class CorePort(cp : CoreParameter) extends Bundle with IMasterSlave{
  val cmd = Stream(Fragment(CoreCmd(cp)))
  val rsp = Stream(Fragment(CoreRsp(cp)))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

case class CoreCmd(cp : CoreParameter) extends Bundle{
  val write = Bool()
  val address = UInt(cp.pl.ml.byteAddressWidth bits)
  val data = Bits(cp.pl.beatWidth bits)
  val context = Bits(cp.contextWidth bits)
}
case class CoreRsp(cp : CoreParameter) extends Bundle{
  val data = Bits(cp.pl.beatWidth bits)
  val context = Bits(cp.contextWidth bits)
}

case class SdramAddress(ml : MemoryLayout) extends Bundle {
  val column = UInt(ml.columnWidth bits)
  val bank   = UInt(ml.bankWidth bits)
  val row    = UInt(ml.rowWidth bits)
}

case class CoreConfig(cp : CoreParameter) extends Bundle {
  val commandPhase = UInt(log2Up(cp.pl.phaseCount) bits)
  val writeLatency = UInt(log2Up(cp.writeLatencies.size) bits)
  val readLatency = UInt(log2Up(cp.readLatencies.size) bits)
  val RFC, RAS, RP, WR, RCD, WTR, CCD, RTP = UInt(cp.timingWidth bits)
  val REF = UInt(cp.refWidth bits)
}

case class CoreParameter(pl : PhyLayout,
                         portCount : Int,
                         contextWidth : Int,
                         timingWidth : Int,
                         refWidth : Int,
                         writeLatencies : List[Int],
                         readLatencies : List[Int])
object FrontendCmdOutputKind extends SpinalEnum{
  val READ, WRITE, ACTIVE, PRECHARGE, REFRESH = newElement()
}
case class FrontendCmdOutput(cp : CoreParameter) extends Bundle {
  val kind = FrontendCmdOutputKind()
  val all = Bool()
  val address = SdramAddress(cp.pl.ml)
  val data = Bits(cp.pl.beatWidth bits)
  val mask = Bits(cp.pl.beatWidth/8 bits)
}



case class InitCmd(cp : CoreParameter) extends Bundle{
  val ADDR  = Bits(cp.pl.ml.chipAddressWidth bits)
  val BA    = Bits(cp.pl.ml.bankWidth bits)
  val CASn  = Bool
  val CKE   = Bool
  val CSn   = Bool
  val RASn  = Bool
  val WEn   = Bool
}

case class InitBus(cp : CoreParameter) extends Bundle with IMasterSlave{
  val cmd = Flow(InitCmd(cp))

  override def asMaster(): Unit = {
    master(cmd)
  }
}