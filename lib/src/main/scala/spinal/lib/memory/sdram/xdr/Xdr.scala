package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.io.TriState

case class MemoryLayout(bankWidth : Int,
                        columnWidth : Int,
                        rowWidth : Int,
                        dataWidth : Int,
                        withDqs : Boolean,
                        burstLength : Int){
  def wordWidth = dataWidth*burstLength
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

//TODO tFAW
//TODO ctrl lock

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

object SdrInferedPhy{
  def memoryLayoutToPhyLayout(ml : MemoryLayout) = PhyLayout(
    phaseCount = 1,
    outputLatency = 1,
    inputLatency = 1,
    ml = ml
  )
}

case class SdrInferedPhy(ml : MemoryLayout) extends Component{
  require(!ml.withDqs)
  val pl = SdrInferedPhy.memoryLayoutToPhyLayout(ml)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val memory = master(Sdr(ml))
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
  val mask = Bits(cp.pl.beatWidth/8 bits)
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
  val source = UInt(log2Up(cp.portCount) bits)
  val context = Bits(cp.contextWidth bits)
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




//Extend access to have them behing a length of 2^alignmentWidth
case class BmbAligner(ip : BmbParameter, op : BmbParameter, alignmentWidth : Int) extends Component{
  val io = new Bundle{
    val input = Bmb(ip)
    val output = Bmb(op)
  }

  val beatCount = (1 << alignmentWidth) / ip.byteCount
  val transferCount = op.transferBeatCount

  case class Context() extends Bundle{
    val input = Bits(ip.contextWidth bits)
    val write = Bool()
    val paddings = UInt(log2Up(beatCount) bits)
    val transfers = UInt(log2Up(transferCount) bits)
  }

  val cmdLogic = new Area {
    val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init(0)
    beatCounter := beatCounter + U(io.output.cmd.fire && io.input.cmd.isWrite)

    val context = Context()
    context.input := io.input.cmd.context
    context.paddings := io.input.cmd.address(alignmentWidth-1 downto log2Up(beatCount))
    context.transfers := io.input.cmd.transferBeatCountMinusOne
    context.write := io.input.cmd.isWrite

    val padding = False
    when(beatCounter === beatCount-1) {
      io.input.cmd.ready := io.output.cmd.ready
      io.output.cmd.last := io.input.cmd.last
    } otherwise {
      padding := io.input.cmd.isWrite && (io.input.cmd.last || io.input.cmd.first && beatCounter < context.paddings)
      io.input.cmd.ready := io.output.cmd.ready && (io.input.cmd.isRead || !io.input.cmd.last)
      io.output.cmd.last := io.input.cmd.isRead
    }

    io.output.cmd.valid := io.input.cmd.valid
    io.output.cmd.address := io.input.cmd.address(ip.addressWidth-1 downto alignmentWidth) << alignmentWidth
    io.output.cmd.context := B(context)
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.length := io.input.cmd.length + U((1 << alignmentWidth)-1, op.lengthWidth bits)
    io.output.cmd.length(alignmentWidth-1 downto 0) := 0
    io.output.cmd.data := io.input.cmd.data
    io.output.cmd.mask := (!padding ? io.input.cmd.mask | 0)
    io.input.cmd.ready := io.output.cmd.ready
  }

  val rspLogic = new Area{
    val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init(0)
    when(io.output.rsp.fire){
      beatCounter := beatCounter + 1
    }

    val transferCounter = Reg(UInt(log2Up(transferCount) bits)) init(0)
    when(io.input.rsp.fire){
      beatCounter := beatCounter + 1
      when(io.input.rsp.last){
        beatCounter := 0
      }
    }

    val context = io.output.rsp.context.as(Context())
    val drop = !context.write && (io.input.rsp.first && beatCounter(context.paddings.range) < context.paddings || transferCounter > context.transfers)

    io.input.rsp.arbitrationFrom(io.output.rsp.throwWhen(drop))
    io.input.rsp.last := context.write || transferCounter === context.transfers
    io.input.rsp.source := io.output.rsp.source
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.data := io.output.rsp.data
    io.input.rsp.context := io.output.rsp.context.resized
  }
}

//Subdivide a burst into smaller fixed length bursts
//Require the input to be fixedLength aligned
//Warning, Can fire output cmd before input cmd on reads
case class BmbLengthFixer(ip : BmbParameter, op : BmbParameter, fixedWidth : Int) extends Component{
  val io = new Bundle{
    val input = Bmb(ip)
    val output = Bmb(op)
  }

  val beatCount = (1 << fixedWidth) / ip.byteCount
  val splitCount = op.transferBeatCount / (1 << fixedWidth)

  case class Context() extends Bundle{
    val input = Bits(ip.contextWidth bits)
    val last = Bool()
  }

  val cmdLogic = new Area {
    val fixedAddress = io.input.cmd.address(ip.addressWidth - 1 downto ip.lengthWidth)
    val baseAddress = io.input.cmd.address(ip.lengthWidth - 1 downto op.lengthWidth)
    val beatAddress = io.input.cmd.address(op.lengthWidth - 1 downto log2Up(op.byteCount))

    val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
    val splitCounter = Reg(UInt(log2Up(splitCount) bits)) init (0)

    val context = Context()
    context.input := io.input.cmd.context
    context.last := io.input.cmd.last

    io.output.cmd.arbitrationFrom(io.input.cmd)
    io.output.cmd.last := io.input.cmd.last || beatCounter === beatCount-1
    io.output.cmd.address := (fixedAddress @@ (baseAddress + splitCounter)) << op.lengthWidth
    io.output.cmd.context := B(context)
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.length := (1 << fixedWidth) - 1
    io.output.cmd.data := io.input.cmd.data
    io.output.cmd.mask := io.input.cmd.mask

    when(io.input.cmd.fire){
      beatCounter := beatCounter + 1
      when(io.output.cmd.last){
        splitCounter := splitCounter + 1
      }
      when(io.input.cmd.last){
        splitCounter := 0
      }
    }
  }

  val rspLogic = new Area{
    val context = io.output.rsp.context.as(Context())
    io.input.rsp.arbitrationFrom(io.output.rsp)
    io.input.rsp.last := io.output.rsp.last && context.last
    io.input.rsp.source := io.output.rsp.source
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.data := io.output.rsp.data
    io.input.rsp.context := io.output.rsp.context.resized
  }
}

case class BmbToCorePort(ip : BmbParameter, op : CoreParameter) extends Component{
  val io = new Bundle{
    val input = Bmb(ip)
    val output = CorePort(op)
  }

  case class Context() extends Bundle{
    val input = Bits(ip.contextWidth bits)
    val source = UInt(ip.sourceWidth bits)
  }

  val cmdContext = Context()
  cmdContext.input := io.input.cmd.context
  cmdContext.source := io.input.cmd.source


  io.input.cmd.arbitrationFrom(io.output.cmd)
  io.output.cmd.write := io.input.cmd.isWrite
  io.output.cmd.address := io.input.cmd.address
  io.output.cmd.data := io.input.cmd.data
  io.output.cmd.mask := io.input.cmd.mask
  io.output.cmd.context := B(cmdContext)


  val rspContext = Context()
  rspContext.input := io.input.cmd.context
  rspContext.source := io.input.cmd.source

  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.setSuccess()
  io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.input
  io.input.rsp.source := rspContext.source
}