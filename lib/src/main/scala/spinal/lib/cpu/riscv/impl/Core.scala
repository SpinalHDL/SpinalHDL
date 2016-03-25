package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.mm._
import spinal.lib.bus.avalon._
import spinal.lib.bus.amba4.axi._
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib.cpu.riscv.impl.extension.CoreExtension
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer




trait BranchPrediction
object disable extends BranchPrediction
object static extends BranchPrediction
object dynamic extends BranchPrediction

trait RegFileReadKind
object async extends RegFileReadKind
object sync extends RegFileReadKind

trait InstructionBusKind
trait DataBusKind
object cmdStream_rspStream extends InstructionBusKind with DataBusKind
object cmdStream_rspFlow extends InstructionBusKind with DataBusKind
object cmdStream_rspFlow_oneCycle extends InstructionBusKind  //Usefull for fast and light cache design



case class CoreParm(val pcWidth : Int = 32,
                    val addrWidth : Int = 32,
                    val startAddress : Int = 0,
                    val bypassExecute0 : Boolean = true,
                    val bypassExecute1 : Boolean = true,
                    val bypassWriteBack0 : Boolean = true,
                    val bypassWriteBack1 : Boolean = true,
                    val collapseBubble : Boolean = true,
                    val branchPrediction : BranchPrediction = static,
                    val regFileReadyKind : RegFileReadKind = sync,
                    val fastFetchCmdPcCalculation : Boolean = true,
                    val instructionBusKind : InstructionBusKind = cmdStream_rspFlow,
                    val dataBusKind : DataBusKind = cmdStream_rspFlow,
                    val dynamicBranchPredictorCacheSizeLog2 : Int = 4,
                    val branchPredictorHistoryWidth : Int = 2,
                    val invalidInstructionIrqId : Int = 0,
                    val unalignedMemoryAccessIrqId : Int = 1
                     ) {
  val extensions = ArrayBuffer[CoreExtension]()
  def add(that : CoreExtension) : this.type = {
    extensions += that
    this
  }
}

case class CoreInstructionCmd(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.addrWidth bit)
}

case class CoreInstructionRsp(implicit p : CoreParm) extends Bundle{
  val instruction = Bits(32 bit)
}

case class CoreInstructionBus(implicit p : CoreParm) extends Bundle with IMasterSlave{
  val cmd = Stream (CoreInstructionCmd())
  val rsp = Stream (CoreInstructionRsp())

  override def asMaster(): CoreInstructionBus.this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): CoreInstructionBus.this.type = asMaster.flip()

  def toAxiRead(): AxiReadOnly ={
    val axiParameters = AxiConfig(
      addressWidth = 32,
      dataWidth = 32
    )
    val axi = new AxiReadOnly(axiParameters)

    axi.readCmd.translateFrom(cmd)((to,from) => {
      to.addr := from.pc
      to.prot := 0
    })
    rsp.translateFrom(axi.readRsp)((to,from) => {
      to.instruction := from.data
    })

    axi
  }

  def toAvalon(): AvalonMMBus = {
    assert(p.instructionBusKind == cmdStream_rspFlow)
    val avalonConfig = AvalonMMConfig.pipelined(
      addressWidth = 32,
      dataWidth = 32).getReadOnlyConfig
    val mm = AvalonMMBus(avalonConfig)
    mm.read := cmd.valid
    mm.waitRequestn := cmd.ready
    mm.address := cmd.pc
    mm.readDataValid := rsp.valid
    mm.readData := rsp.instruction
    mm
  }
}



case class CoreDataBus(implicit p : CoreParm) extends Bundle with IMasterSlave{
  val cmd = Stream (CoreDataCmd())
  val rsp = Stream (Bits(32 bit))

  override def asMaster(): this.type = {
    master(cmd)
    slave(rsp)
    this
  }

  override def asSlave(): this.type = asMaster.flip()

  def toAvalon(): AvalonMMBus = {
    assert(p.dataBusKind == cmdStream_rspFlow)
    val avalonConfig = AvalonMMConfig.pipelined(
      addressWidth = 32,
      dataWidth = 32).copy(
      maximumPendingReadTransactions = 2
      )
    val mm = AvalonMMBus(avalonConfig)
    mm.read := cmd.valid
    mm.waitRequestn := cmd.ready
    mm.address := cmd.address
    mm.readDataValid := rsp.valid
    mm.readData := rsp.payload
    mm
  }
}

case class CoreDataCmd(implicit p : CoreParm) extends Bundle{
  val wr = Bool
  val address = UInt(p.addrWidth bit)
  val data = Bits(32 bit)
  val size = UInt(2 bit)
}

case class BranchPredictorLine(implicit p : CoreParm)  extends Bundle{
  val pc = UInt(p.pcWidth-p.dynamicBranchPredictorCacheSizeLog2-2 bit)
  val history = SInt(2 bit)
}

case class CoreFetchCmdOutput(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
}

case class CoreFetchOutput(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val branchCacheLine = BranchPredictorLine()
}

case class CoreDecodeOutput(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val src0 = Bits(32 bit)
  val src1 = Bits(32 bit)
  val alu_op0 = Bits(32 bit)
  val alu_op1 = Bits(32 bit)
  val doSub = Bool
  val predictorHasBranch = Bool
  val branchHistory = Flow(SInt(p.branchPredictorHistoryWidth bit))
}

case class CoreExecute0Output(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val br = new Bundle {
    val eq, ltx = Bool
  }
  val src1 = Bits(32 bit)
  val result = Bits(32 bit)
  val adder = UInt(32 bit)
  val predictorHasBranch = Bool
  val branchHistory = Flow(SInt(p.branchPredictorHistoryWidth bit))
  val pcPlus4 = UInt(32 bit)
  val pc_sel = PC()
  val unalignedMemoryAccessException = Bool
}

case class CoreExecute1Output(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val result = Bits(32 bit)
  val regFileAddress = UInt(5 bit)
  val pcPlus4 = UInt(32 bit)
  val unalignedMemoryAccessException = Bool
}

case class CoreWriteBack0Output(implicit p : CoreParm) extends Bundle{
  val addr = UInt(5 bit)
  val data = Bits(32 bit)
}

class Core(implicit p : CoreParm) extends Component{
  import p._
  val io = new Bundle{
    val i = master(CoreInstructionBus())
    val d = master(CoreDataBus())
  }
  val irqUsages = mutable.HashMap[Int,IrqUsage]()
  if(invalidInstructionIrqId != 0) irqUsages(invalidInstructionIrqId) = IrqUsage(true)
  if(unalignedMemoryAccessIrqId != 0) irqUsages(unalignedMemoryAccessIrqId) = IrqUsage(true)
  for(extension <- extensions){
    for((id,usage) <- extension.getIrqUsage){
      irqUsages(id) = usage
    }
  }
  val irqWidth = irqUsages.foldLeft(0)((max,e) => Math.max(max,e._1)) + 1
  val irqExceptionMask = irqUsages.foldLeft(0)((mask,e) => if(e._2.isException) mask + 1 << e._1 else mask)
  val regFile = Mem(Bits(32 bit),32)
  val brancheCache = Mem(BranchPredictorLine(), 1<<dynamicBranchPredictorCacheSizeLog2)

  //Send instruction request to io.i.cmd
  val fetchCmd = new Area {
    val pc = Reg(UInt(pcWidth bit)) init(U(startAddress,pcWidth bit))
    val inc = RegInit(False) //when io.i.cmd is stalled, it's used as a token to continue the request the next cycle
    val redo = False
    val pcNext = if(fastFetchCmdPcCalculation){
      val pcPlus4 = (pc + U(4)).add(new AttributeFlag("keep"))
      Mux(inc && !redo,pcPlus4,pc)
    }else{
      pc + Mux(inc && !redo,U(4),U(0))
    }
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.payload
    }

    val outInst = Stream(CoreFetchCmdOutput())

    instructionBusKind match {
      case `cmdStream_rspFlow` =>
        io.i.cmd.valid := outInst.ready
      case _ =>
        io.i.cmd.valid := True
    }
    io.i.cmd.pc := pcNext
    when(io.i.cmd.fire || pcLoad.fire){
      pc := pcNext
    }

    when(io.i.cmd.fire){
      inc := True
    }.elsewhen(pcLoad.valid || redo){
      inc := False
    }

    outInst.valid := io.i.cmd.fire
    outInst.pc := pcNext
  }

  //Join fetchCmd.outInst with io.i.rsp
  val fetch = new Area {
    val inContext = fetchCmd.outInst.m2sPipe()
    val outInst = Stream(CoreFetchOutput())
    val throwIt = False
    val flush = False
    when(flush){
      throwIt := True
    }

    instructionBusKind match {
      case `cmdStream_rspFlow_oneCycle` => {
        inContext.ready := True
        io.i.rsp.ready := True
        when(inContext.valid && (!io.i.rsp.valid || !outInst.ready)) {
          fetchCmd.redo := True
        }
        outInst.valid := io.i.rsp.valid && !throwIt
        outInst.pc := inContext.pc
        outInst.instruction := io.i.rsp.instruction
        outInst.branchCacheLine := brancheCache.readSync(fetchCmd.outInst.pc(2, dynamicBranchPredictorCacheSizeLog2 bit))
      }
      case _ => {
        val iRsp = if (instructionBusKind == cmdStream_rspFlow) {
          val backupFifoIn = Stream(CoreInstructionRsp())
          backupFifoIn.valid := io.i.rsp.valid
          backupFifoIn.payload := io.i.rsp.payload
          backupFifoIn.s2mPipe() //1 depth of backup fifo, zero latency
        } else {
          io.i.rsp
        }

        val throwNextIRsp = RegInit(False)
        when(throwIt && inContext.valid && !iRsp.valid) {
          throwNextIRsp := True
        }
        when(throwNextIRsp && iRsp.valid) {
          throwNextIRsp := False
        }

        inContext.ready := outInst.fire || throwIt
        io.i.rsp.ready := True
        outInst.arbitrationFrom(iRsp.throwWhen(throwIt || throwNextIRsp))
        outInst.pc := inContext.pc
        outInst.instruction := iRsp.instruction
        outInst.branchCacheLine := brancheCache.readSync(Mux(inContext.isStall, inContext.pc, fetchCmd.outInst.pc)(2, dynamicBranchPredictorCacheSizeLog2 bit))
      }
    }
  }

  val decode = new Area{
    val inInst = fetch.outInst.m2sPipe()
    val ctrl = getInstructionCtrl(inInst.instruction)
    val hazard = Bool //Used to stall decode phase because of register file hazard
    val throwIt = False
    val halt = False
    when(hazard){
      halt := True
    }
    val addr0 = inInst.instruction(src0Range).asUInt
    val addr1 = inInst.instruction(src1Range).asUInt

    //read register file
    val (src0,src1) = regFileReadyKind match{
      case `async` => (regFile.readAsync(inInst.instruction(src0Range).asUInt),regFile.readAsync(inInst.instruction(src1Range).asUInt))
      case `sync` =>  {
        val srcInstruction = Mux(inInst.isStall,inInst.instruction,fetch.outInst.instruction)
        (regFile.readSync(srcInstruction(src0Range).asUInt),regFile.readSync(srcInstruction(src1Range).asUInt))
      }
    }
    val imm = IMM(inInst.instruction)

    // calculate branch target
    val brjmpImm = Mux(ctrl.jmp, imm.j_sext, imm.b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt

    // branch prediction
    val branchCacheHit = inInst.branchCacheLine.pc === inInst.pc(pcWidth-1 downto 2 + dynamicBranchPredictorCacheSizeLog2)
    val staticBranchPrediction = brjmpImm.msb || ctrl.br === BR.J
    val shouldTakeBranch = Bool
    branchPrediction match{
      case `disable` =>
        shouldTakeBranch := False
      case `static` =>
        shouldTakeBranch := staticBranchPrediction
      case `dynamic` =>
        shouldTakeBranch := staticBranchPrediction
        when(branchCacheHit) {
          shouldTakeBranch := inInst.branchCacheLine.history.msb
        }
    }


    val outInst = Stream(CoreDecodeOutput())

    // branch interface
    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := inInst.valid && !hazard && outInst.ready && (ctrl.br =/= BR.JR && ctrl.br =/= BR.N) && ctrl.instVal && shouldTakeBranch
    pcLoad.payload := brJumpPc

    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.ctrl := ctrl
    outInst.doSub := outInst.ctrl.alu =/= ALU.ADD
    outInst.src0 := Mux(addr0 =/= 0, src0, B(0, 32 bit))
    outInst.src1 := Mux(addr1 =/= 0, src1, B(0, 32 bit))
    outInst.alu_op0 := outInst.ctrl.op1.map(
      default -> outInst.src0,
      OP1.IMU -> imm.u.resized,
      OP1.IMZ -> imm.z.resized,
      OP1.IMJB -> Mux(ctrl.jmp, imm.j_sext, imm.b_sext).resized
    )
    outInst.alu_op1 := outInst.ctrl.op2.map(
      default -> outInst.src1,
      OP2.IMI -> imm.i_sext.resized,
      OP2.IMS -> imm.s_sext.resized,
      OP2.PC1 -> inInst.pc.asBits
    )
    outInst.predictorHasBranch := pcLoad.valid
    outInst.branchHistory.valid := branchCacheHit
    outInst.branchHistory.payload := inInst.branchCacheLine.history


    val flush = False
    when(flush){
      fetch.throwIt := True
      throwIt := True
    }
  }

  val execute0 = new Area {
    val inInst = decode.outInst.m2sPipe(collapseBubble)
    val throwIt = False
    val halt = inInst.valid && inInst.ctrl.men && !io.d.cmd.ready

    val ctrl = inInst.ctrl
    val addr0 = inInst.instruction(19 downto 15).asUInt
    val addr1 = inInst.instruction(24 downto 20).asUInt

    val imm = IMM(inInst.instruction)

    //conditional branch calculation
    val br = new Area {
      val signed = BR.isSignedComp(ctrl.br)
      val src0Ext = (inInst.src0.msb && signed) ## inInst.src0
      val src1Ext = (inInst.src1.msb && signed) ## inInst.src1
      val ltx =  (src0Ext.asUInt-src1Ext.asUInt).msb
      val eq = inInst.src0 === inInst.src1


      val pc_sel = inInst.ctrl.br.map[PC.T](
        default -> PC.INC,
        BR.NE -> Mux(!eq, PC.BR1, PC.INC),
        BR.EQ -> Mux(eq, PC.BR1, PC.INC),
        (BR.GE , BR.GEU) -> Mux(!ltx, PC.BR1, PC.INC),
        (BR.LT , BR.LTU)  -> Mux(ltx, PC.BR1, PC.INC),
        BR.J -> PC.J,
        BR.JR -> PC.JR
      )
    }

    val alu = new Alu
    alu.io.func := inInst.ctrl.alu
    alu.io.doSub := inInst.doSub
    alu.io.src0 := inInst.alu_op0
    alu.io.src1 := inInst.alu_op1

    val outInst = Stream(CoreExecute0Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.predictorHasBranch := inInst.predictorHasBranch
    outInst.branchHistory.valid   := inInst.branchHistory.valid
    outInst.branchHistory.payload := inInst.branchHistory.payload
    outInst.ctrl := ctrl
    outInst.br.eq := br.eq
    outInst.br.ltx := br.ltx
    outInst.pc_sel := br.pc_sel
    outInst.src1 := inInst.src1
    outInst.result := alu.io.result
    outInst.adder := alu.io.adder
    outInst.pcPlus4 := inInst.pc + 4

    // Send memory read/write requests
    outInst.unalignedMemoryAccessException := inInst.ctrl.men && outInst.ctrl.msk.map(
      default-> False,
      MSK.H -> io.d.cmd.address(0),
      MSK.W -> (io.d.cmd.address(0) || io.d.cmd.address(1))
    )

    io.d.cmd.valid := outInst.fire && inInst.ctrl.men && !outInst.unalignedMemoryAccessException
    io.d.cmd.wr := inInst.ctrl.m === M.XWR
    io.d.cmd.address := outInst.adder
    io.d.cmd.payload.data := inInst.src1
    io.d.cmd.size := inInst.ctrl.msk.map(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.H -> U(1)
    )
    val pendingDataCmd = new Area{
      val pendingDataMax = 2
      val readCount = Reg(UInt(log2Up(pendingDataMax) bit)) init(0)
      val readCountInc = io.d.cmd.fire && !io.d.cmd.wr
      val readCountDec = io.d.rsp.fire

      when(readCountInc =/= readCountDec){
        readCount := readCount + Mux(readCountInc,U(1),U(readCount.maxValue))
      }
      when(inInst.valid && inInst.ctrl.men && inInst.ctrl.m === M.XRD && readCount === pendingDataMax){
        halt := True
      }
    }

    val flush = False
    when(flush){
      fetch.throwIt := True
      decode.throwIt := True
      throwIt := True
    }
  }

  val execute1 = new Area {
    val inInst = execute0.outInst.m2sPipe(collapseBubble)
    val halt = False
    val throwIt = False

    val pc_sel = inInst.pc_sel


    // branche interface
    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := inInst.fire && pc_sel.map(
      PC.INC -> inInst.predictorHasBranch,
      default -> !inInst.predictorHasBranch
    )

    pcLoad.payload := pc_sel.map(
      PC.INC -> inInst.pcPlus4,
      default -> inInst.adder
    )

    // dynamic branch predictor history update
    when(inInst.fire && inInst.ctrl.br =/= BR.JR && inInst.ctrl.br =/= BR.N && inInst.ctrl.br =/= BR.J){
      val line = BranchPredictorLine()
      val newHistory = inInst.branchHistory.payload.resize(branchPredictorHistoryWidth + 1) + Mux(pc_sel === PC.INC,S(1),S(-1))
      line.pc := inInst.pc(pcWidth-1 downto 2 + dynamicBranchPredictorCacheSizeLog2)
      when(inInst.branchHistory.valid){
        line.history := newHistory.resized
      }otherwise {
        line.history := (pc_sel =/= PC.INC).asSInt.resized
      }
      when(newHistory(newHistory.high downto newHistory.high - 1) =/= S"10") { //no history overflow
        brancheCache(inInst.pc(2, dynamicBranchPredictorCacheSizeLog2 bit)) := line
      }
    }

    val outInst = Stream(CoreExecute1Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.pc := inInst.pc
    outInst.result := inInst.result
    outInst.regFileAddress := inInst.instruction(dstRange).asUInt
    outInst.ctrl := inInst.ctrl
    outInst.instruction := inInst.instruction
    outInst.pcPlus4 := inInst.pcPlus4
    outInst.unalignedMemoryAccessException := inInst.unalignedMemoryAccessException

    val flush = False
    when(flush){
      fetch.throwIt := True
      decode.throwIt := True
      execute0.throwIt := True
      throwIt := True
    }
  }

  val writeBack0 = new Area{
    val inInst = execute1.outInst.m2sPipe(collapseBubble)
    val throwIt = !inInst.ctrl.rfen
    val halt = False

    val irq = new Area {
      val sources = B(0,irqWidth bit)
      val mask = Reg(Bits(irqWidth bit)) init(0)
      val masked = sources & mask
      when(((sources & ~mask) & irqExceptionMask) =/= 0){
        halt := True
      }

      when(inInst.valid) {
        if(invalidInstructionIrqId != -1){
          when(!inInst.ctrl.instVal) {
            sources(invalidInstructionIrqId) := True
          }
        }
        if(unalignedMemoryAccessIrqId != -1){
          when(inInst.unalignedMemoryAccessException) {
            sources(unalignedMemoryAccessIrqId) := True
          }
        }
      }
    }


    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := False
    pcLoad.payload.assignDontCare()

    val needMemoryResponse = inInst.ctrl.wb === WB.MEM && inInst.ctrl.m === M.XRD
    val flushMemoryResponse = RegInit(False)
    io.d.rsp.ready := False
    when(inInst.valid && needMemoryResponse){
      when(!io.d.rsp.valid) {
        halt := True
      }
      io.d.rsp.ready := !halt
    }

    when(execute0.pendingDataCmd.readCount === 0){
      flushMemoryResponse := False
    }
    when(flushMemoryResponse){
      io.d.rsp.ready := True
      halt := True
    }

    val dataRspFormated = inInst.ctrl.msk.map(
      default -> io.d.rsp.payload, //W
      MSK.B   -> B(default -> (io.d.rsp.payload(7) && ! inInst.instruction(14)),(7 downto 0) -> io.d.rsp.payload(7 downto 0)),
      MSK.H   -> B(default -> (io.d.rsp.payload(15) && ! inInst.instruction(14)),(15 downto 0) -> io.d.rsp.payload(15 downto 0))
    )

    val regFileData = inInst.ctrl.wb.map (
      default -> B(0,32 bit), //CSR1
      WB.ALU1 -> inInst.result,
      WB.MEM  -> dataRspFormated,
      WB.PC4  -> (inInst.pcPlus4).asBits.resized
    )


    val outInst = Stream(CoreWriteBack0Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.addr := inInst.regFileAddress
    outInst.data := regFileData

    when(outInst.fire) {
      regFile(outInst.addr) := regFileData
    }

  }

  //This stage is only about keep a trace of last writeBack0, trace used later to avoid read during write hazard on register file
  val writeBack1 = new Area{
    val inInst = writeBack0.outInst.m2sPipe(collapseBubble)
    inInst.ready := True
  }



  // apply decode/execute1 pcLoad interfaces to fetchCmd.pcLoad
  val branchArbiter = new Area {
    branchPrediction match{
      case `disable` =>
        fetchCmd.pcLoad.valid := execute1.pcLoad.valid
        fetchCmd.pcLoad.payload := execute1.pcLoad.payload
        when(execute1.pcLoad.valid) {
          execute0.flush := True
        }
      case `static` | `dynamic` =>
        fetchCmd.pcLoad.valid := decode.pcLoad.valid
        fetchCmd.pcLoad.payload := decode.pcLoad.payload
        when(decode.pcLoad.valid){
          fetch.flush := True
        }
        when(execute1.pcLoad.valid){
          execute0.flush :=  True
          fetchCmd.pcLoad.valid := True
          fetchCmd.pcLoad.payload := execute1.pcLoad.payload
        }
    }

    when(writeBack0.pcLoad.valid){
      execute1.flush :=  True
      fetchCmd.pcLoad.valid := True
      fetchCmd.pcLoad.payload := writeBack0.pcLoad.payload
    }

    val loadCounter = Counter(1<<30,execute1.pcLoad.valid).value.keep()
    //val flushCounter = Counter(1<<30,io.fetch.).value.keep()
  }



  // Check hazard and apply bypass logic
  val hazardTracker = new  Area {
    val addr0Check = decode.addr0 =/= 0
    val addr1Check = decode.addr1 =/= 0
    val src0Hazard = False
    val src1Hazard = False
    decode.hazard := src0Hazard || src1Hazard

    // write back bypass and hazard
    if(bypassWriteBack1) {
      when(writeBack1.inInst.valid) {
        when(addr0Check && writeBack1.inInst.addr === decode.addr0) {
          decode.src0 := writeBack1.inInst.data
        }
        when(addr1Check && writeBack1.inInst.addr === decode.addr1) {
          decode.src1 := writeBack1.inInst.data
        }
      }
    }else{
      when(writeBack1.inInst.valid) {
        when(decode.addr0 === writeBack1.inInst.addr) {
          src0Hazard := True
        }
        when(decode.addr1 === writeBack1.inInst.addr) {
          src1Hazard := True
        }
      }
    }

    // memory access bypass and hazard
    val A = new Area{
      val addr0Match = writeBack0.outInst.addr === decode.addr0
      val addr1Match = writeBack0.outInst.addr === decode.addr1
      when(writeBack0.inInst.ctrl.rfen){
        if(bypassWriteBack0) {
          when(writeBack0.outInst.valid) {
            when(addr0Check && addr0Match) {
              decode.src0 := writeBack0.regFileData
            }
            when(addr1Check && addr1Match) {
              decode.src1 := writeBack0.regFileData
            }
          }
        }
        when(writeBack0.inInst.valid && writeBack0.inInst.ctrl.rfen && (Bool(!bypassWriteBack0) || !writeBack0.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    // execute1 bypass and hazard
    val E1 = new Area{
      val addr0Match = execute1.outInst.instruction(dstRange).asUInt === decode.addr0
      val addr1Match = execute1.outInst.instruction(dstRange).asUInt === decode.addr1
      when(execute1.outInst.ctrl.rfen) {
        if (bypassExecute1) {
          when(execute1.outInst.ctrl.execute1AluBypass && execute1.outInst.ctrl.rfen && execute1.outInst.valid) {
            when(addr0Check && addr0Match) {
              decode.src0 := execute1.outInst.result
            }
            when(addr1Check && addr1Match) {
              decode.src1 := execute1.outInst.result
            }
          }
        }
        when(execute1.inInst.valid && execute1.inInst.ctrl.rfen && (Bool(!bypassExecute1) || !execute1.inInst.ctrl.execute1AluBypass || !execute1.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    // execute0 bypass and hazard
    val E0 = new Area {
      val addr0Match = execute0.outInst.instruction(dstRange).asUInt === decode.addr0
      val addr1Match = execute0.outInst.instruction(dstRange).asUInt === decode.addr1
      when(execute0.outInst.ctrl.rfen) {
        if (bypassExecute0) {
          when(execute0.outInst.ctrl.execute0AluBypass && execute0.outInst.ctrl.rfen && execute0.outInst.valid) {
            when(addr0Check && addr0Match) {
              decode.src0 := execute0.outInst.result
            }
            when(addr1Check && addr1Match) {
              decode.src1 := execute0.outInst.result
            }
          }
        }
        when(execute0.inInst.valid && execute0.inInst.ctrl.rfen && (Bool(!bypassExecute0) || !execute0.inInst.ctrl.execute0AluBypass || !execute0.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    when(!addr0Check){
      src0Hazard := False
    }
    when(!addr1Check){
      src1Hazard := False
    }
  }

  val noDataRspStallLogic = if(dataBusKind == cmdStream_rspFlow) new Area{
    when(execute0.inInst.valid && execute0.inInst.ctrl.men && execute0.inInst.ctrl.m === M.XRD){
      when(execute1.inInst.valid && execute1.inInst.ctrl.canInternalyStallWriteBack0){
        execute0.halt := True
      }
      when(writeBack0.inInst.isStall && writeBack0.inInst.ctrl.canInternalyStallWriteBack0){
        execute0.halt := True
      }
    }
  } else null


  for(extension <- extensions){
    val area = extension.applyIt(this)
    area.setName(extension.getName)
  }
  def getInstructionCtrl(instruction : Bits) = {
    applyExtensionTags
    val ctrl = InstructionCtrl(instruction)
    for(extension <- extensions){
      extension.instructionCtrlExtension(instruction,ctrl)
    }
    ctrl
  }
  lazy val applyExtensionTags ={
    var tagCounter = 0
    for(extension <- extensions){
      if(extension.needTag){
        tagCounter += 1
        extension.tag = tagCounter
      }
    }
  }
}





object CoreMain{
  import extension._
  def main(args: Array[String]) {
    SpinalVhdl({
      val interrupt = Bool

      val p = CoreParm(
        pcWidth = 32,
        addrWidth = 32,
        startAddress = 0x200,
        regFileReadyKind = sync,
        branchPrediction = dynamic,
        bypassExecute0 = true,
        bypassExecute1 = true,
        bypassWriteBack0 = true,
        bypassWriteBack1 = true,
        collapseBubble = true,
        instructionBusKind = cmdStream_rspFlow,
        dataBusKind = cmdStream_rspFlow,
        fastFetchCmdPcCalculation = true,
        dynamicBranchPredictorCacheSizeLog2 = 7
      )
      p.add(new MulExtension)
      p.add(new DivExtension)
      p.add(new BarrelShifterFullExtension)
      p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=interrupt,IrqUsage(isException=false),name="io_interrupt"))
//      p.add(new BarrelShifterLightExtension)
      new Core()(p)
      }
    ,_.setLibrary("riscv"))
  }
}
