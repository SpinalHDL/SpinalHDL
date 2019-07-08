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

case class Core(cp : CoreParameter) extends Component{
  def pl = cp.pl
  def ml = pl.ml


  val io = new Bundle {
    val config = in(CoreConfig(cp))
    val ports = Vec(slave(CorePort(cp)), cp.portCount)
    val phy = master(SdramXdrPhyCtrl(pl))
  }
  object FrontendCmdOutputKind extends SpinalEnum{
    val READ, WRITE, ACTIVE, PRECHARGE, REFRESH = newElement()
  }
  case class FrontendCmdOutput() extends Bundle {
    val kind = FrontendCmdOutputKind()
    val all = Bool()
    val address = SdramAddress(pl.ml)
    val data = Bits(pl.beatWidth bits)
    val mask = Bits(pl.beatWidth/8 bits)
  }

  def Timing(loadValid : Bool, loadValue : UInt) = new Area{
    val value = Reg(UInt(cp.timingWidth bits)) init(0)
    val busy = value =/= 0
    value := value - busy.asUInt
    when(loadValid) { value := loadValue }
  }


  val refresher = new Area{
    val value = Reg(UInt(cp.timingWidth bits)) init(0)
    value := value - 1
    val pending = RegInit(False)
    when(value === 0) {
      value := io.config.REF
      pending := True
    }
  }

  val frontend = new Area {
    val banks = for(bankId <- 0 until ml.bankWidth) yield Reg(new Bundle {
      val active = Bool()
      val row = UInt(ml.rowWidth bits)
    })

    val gates = for (port <- io.ports) yield new Area {
      val address = port.cmd.address.as(SdramAddress(ml))
      val bank = banks.read(address.bank)
      val blocked = False
      val cmdOutputPayload = Fragment(FrontendCmdOutput())
      cmdOutputPayload.last := port.cmd.last
      cmdOutputPayload.fragment.address := address
      cmdOutputPayload.fragment.data := port.cmd.data
//      cmdOutputPayload.active = !bank.active
//      cmdOutputPayload.precharge = bank.active && bank.row =/= address.row
//      cmdOutputPayload.refresh = False
      cmdOutputPayload.fragment.all := False
      cmdOutputPayload.fragment.kind := (port.cmd.write ? FrontendCmdOutputKind.WRITE | FrontendCmdOutputKind.READ)
      when(!bank.active){
        cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.ACTIVE
      } elsewhen(bank.row =/= address.row){
        cmdOutputPayload.fragment.kind := FrontendCmdOutputKind.PRECHARGE
      }
      val output = port.cmd.toEvent().haltWhen(blocked).translateWith(cmdOutputPayload)
    }

    val output = StreamArbiterFactory.transactionLock.lowerFirst.on(gates.map(_.output))
    val outputBank = Vec(banks)(output.address.bank)
    when(output.fire){
      outputBank.row := output.address.row
      switch(output.kind) {
        is(FrontendCmdOutputKind.ACTIVE) {
          outputBank.active := True
        }
        is(FrontendCmdOutputKind.PRECHARGE) {
          outputBank.active := False
          when(output.all){
            banks.foreach(_.active := False)
          }
        }
      }
    }
  }

  val refreshInjector = new Area{

  }

  val scheduler = new Area{
    val input = frontend.output.stage()
//    class BusyCounterData(width : Int) extends Bundle{
//      val value = UInt(busyCounterWidth bits)
//      val busy = Bool
//    }
//    val banks = for(bankId <- 0 until ml.bankWidth) yield new Bundle {
//      val general = BusyCounterData(p.timingWidth)
//      val precharge = BusyCounterData(p.timingWidth)
//    }

//    case class BusyCounterData(width : Int) extends Area{
//      val value = Reg(UInt(busyCounterWidth bits)) init(0)
//      val busy = value =/= 0
//      value := value - busy.asUInt
//      def load(cycles : UInt) = value := cycles
//      def loader(cmd : Flow[UInt]) = {when(cmd.valid) value := cmd.payload; this}
//    }

    //Request to load timings counters
    val trigger = new Area{
      val WR,RAS,RP,RCD,WTR,CCD,RFC,RTP = False
    }

    //Banks timing counters
    val timing = new Area {
      val WTR = Timing(trigger.WTR, io.config.WTR)
      val CCD = Timing(trigger.CCD, io.config.CCD)
      val RFC = Timing(trigger.RFC, io.config.RFC)
      val banks = for (bankId <- 0 until ml.bankWidth) yield new Area {
        val hit = input.address.bank === bankId
        val WR = Timing(hit && trigger.WR, io.config.WR)
        val RAS = Timing(hit && trigger.RAS, io.config.RAS)
        val RP = Timing(hit && trigger.RP, io.config.RP)
        val RCD = Timing(hit && trigger.RCD, io.config.RCD)
        val RTP = Timing(hit && trigger.RTP, io.config.RTP)
      }
      val WR = banks.map(_.WR.busy).read(input.address.bank)
      val RAS = banks.map(_.RAS.busy).read(input.address.bank)
      val RP = banks.map(_.RP.busy).read(input.address.bank)
      val RCD = banks.map(_.RCD.busy).read(input.address.bank)
      val RTP = banks.map(_.RTP.busy).read(input.address.bank)
    }

    val timingIssue = False
    timingIssue.setWhen(timing.RFC.busy)
    switch(input.kind) {
      is(FrontendCmdOutputKind.READ) {
        timingIssue.setWhen(timing.RCD || timing.CCD.busy || timing.WTR.busy)
      }
      is(FrontendCmdOutputKind.WRITE) {
        timingIssue.setWhen(timing.RCD || timing.CCD.busy || timing.RTP)
      }
      is(FrontendCmdOutputKind.ACTIVE) {
        timingIssue.setWhen(timing.RP)
      }
      is(FrontendCmdOutputKind.PRECHARGE) {
        timingIssue.setWhen(timing.WR || timing.RAS)
      }
      is(FrontendCmdOutputKind.REFRESH) {
        timingIssue.setWhen(timing.RP)
      }
    }

    when(input.fire){
      switch(input.kind) {
        is(FrontendCmdOutputKind.READ) {
          trigger.CCD := True
          trigger.RTP := True
        }
        is(FrontendCmdOutputKind.WRITE) {
          trigger.CCD := True
          trigger.WTR := True
          trigger.WR := True
        }
        is(FrontendCmdOutputKind.ACTIVE) {
          trigger.RAS := True
          trigger.RCD := True
        }
        is(FrontendCmdOutputKind.PRECHARGE) {
          trigger.RP := True
        }
        is(FrontendCmdOutputKind.REFRESH) {
          trigger.RFC := True
        }
      }
    }

    val output = input.haltWhen(timingIssue)
  }


  val backend = new Area{
    val input = scheduler.output.toFlow.stage()

    val command = new Area{
      val CSn  = False
      val RASn = True
      val CASn = True
      val WEn  = True
      val CKE  = True
    }

    io.phy.DQe := False
    for((phase, id) <- io.phy.phases.zipWithIndex){
      when(io.config.commandPhase =/= id) {
        phase.CSn := False
        phase.RASn := True
        phase.CASn := True
        phase.WEn := True
        phase.CKE := True
      } otherwise {
        phase.CSn := command.CSn
        phase.RASn := command.RASn
        phase.CASn := command.CASn
        phase.WEn := command.WEn
        phase.CKE := command.CKE
      }
      phase.DQw.assignDontCare()
    }

    val writePipeline = new Area{
      case class Cmd() extends Bundle {
        val data = Bits()
        val mask = Bits()
      }
      val input = Flow(Cmd())
      val histories = History(input, 0 to cp.writeLatencies.max)
      histories.tail.foreach(_.valid init(False))

      switch(io.config.writeLatency) {
        for (i <- 0 until cp.writeLatencies.size) {
          is(i){
            when(histories(cp.writeLatencies(i)).valid){
              io.phy.DQe := True
              for((phase, dq, dm) <- (io.phy.phases, histories(i).data.subdivideIn(pl.phaseCount slices), histories(i).mask.subdivideIn(pl.phaseCount slices)).zipped){
                phase.DQw := dq
                phase.DM := ~dm
              }
            }
          }
        }
      }
    }

    val rspPipeline = new Area{
      case class Cmd() extends Bundle{
        val write = Bool
        val context = Bits(cp.contextWidth bits)
      }
      case class Rsp() extends Bundle{
        val data = Bits(pl.beatWidth bits)
        val context = Bits(cp.contextWidth bits)
      }
      val input = Flow(Cmd())

      val histories = History(input, 0 to cp.readLatencies.max + pl.outputLatency + pl.inputLatency - 1)
      histories.tail.foreach(_.valid init(False))

      val buffer = Reg(Flow(Cmd()))
      val bufferCounter = Reg(UInt(log2Up(pl.beatCount) bits)) init(0)
      bufferCounter := bufferCounter + U(buffer.valid)
      buffer.valid init(False)
      buffer.valid clearWhen(bufferCounter === pl.beatCount)

      switch(io.config.readLatency) {
        for (i <- 0 until cp.readLatencies.size) {
          is(i){
            val history = histories(cp.readLatencies(i))
            when(history.valid){
              buffer.valid := True
              buffer.payload := history.payload
            }
          }
        }
      }


      val output = Flow(Rsp())
      output.valid := buffer.valid
      output.context := buffer.context
      for((outputData, phase) <- (output.data.subdivideIn(pl.phaseCount slices), io.phy.phases).zipped){
        outputData := phase.DQw
      }
    }

    writePipeline.input.valid := input.valid && input.kind === FrontendCmdOutputKind.WRITE
    writePipeline.input.data := input.data
    writePipeline.input.mask := input.mask

    when(input.valid) {
      when(input.first) {
        switch(input.kind) {
          is(FrontendCmdOutputKind.PRECHARGE) {
            io.phy.ADDR := input.address.row.asBits
            io.phy.ADDR(10) := input.all
            io.phy.BA := input.address.bank.asBits
            command.CSn := False
            command.RASn := False
            command.CASn := True
            command.WEn := False
          }
          is(FrontendCmdOutputKind.REFRESH) {
            command.CSn := False
            command.RASn := False
            command.CASn := False
            command.WEn := True
          }
          is(FrontendCmdOutputKind.ACTIVE) {
            io.phy.ADDR := input.address.row.asBits
            io.phy.BA := input.address.bank.asBits
            command.CSn := False
            command.RASn := False
            command.CASn := True
            command.WEn := True
          }
          is(FrontendCmdOutputKind.WRITE) {
            io.phy.ADDR := input.address.column.asBits
            io.phy.ADDR(10) := False
            io.phy.BA := input.address.bank.asBits
            command.CSn := False
            command.RASn := True
            command.CASn := False
            command.WEn := False
          }
          is(FrontendCmdOutputKind.READ) {
            io.phy.ADDR := input.address.column.asBits
            io.phy.ADDR(10) := False
            io.phy.BA := input.address.bank.asBits
            command.CSn := False
            command.RASn := True
            command.CASn := False
            command.WEn := True
          }
        }
      }
    }
  }
}