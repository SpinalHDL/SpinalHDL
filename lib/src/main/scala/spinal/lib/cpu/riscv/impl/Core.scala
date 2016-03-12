package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils._


case class CoreParm(val pcWidth : Int = 32,
                    val addrWidth : Int = 32,
                    val startAddress : Int = 0,
                    val bypassExecute0 : Boolean = true,
                    val bypassExecute1 : Boolean = true,
                    val bypassWriteBack : Boolean = true,
                    val pendingI : Int = 1,
                    val enableDynamicBranchPrediction : Boolean = false,
                    val branchPredictorSizeLog2 : Int = 4,
                    val branchPredictorHistoryWidth : Int = 2
                     )

case class CoreInstCmd(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.addrWidth bit)
}
case class CoreInstRsp(implicit p : CoreParm) extends Bundle{
  val instruction = Bits(32 bit)
}


case class CoreDataCmd(implicit p : CoreParm) extends Bundle{
  val wr = Bool
  val address = UInt(p.addrWidth bit)
  val data = Bits(32 bit)
  val size = UInt(2 bit)
}

case class BranchPredictorLine(implicit p : CoreParm)  extends Bundle{
  val valid = Bool
  val pc = UInt(p.pcWidth-p.branchPredictorSizeLog2-2 bit)
  val history = SInt(2 bit)
}
// assert(latencyAnalysis(io.iCmd.data,io.iRsp.data) == 1)
class Core(implicit p : CoreParm) extends Component{
  import p._
  assert(pendingI == 1)
  val io = new Bundle{
    val i = new Bundle {
      val flush = out Bool
      val cmd = master Stream (CoreInstCmd())
      val rsp = slave Stream (CoreInstRsp())
    }
    val d = new Bundle {
      val cmd = master Stream (CoreDataCmd())
      val rsp = slave Flow (Bits(32 bit))
    }
  }
  io.i.flush := False
  val regFile = Mem(Bits(32 bit),32)
  val brancheCache = Mem(BranchPredictorLine(), 1<<branchPredictorSizeLog2) // TODO init it + manage valid

  val fetchCmd = new Area {
    val pc = Reg(UInt(pcWidth bit)) init(U(startAddress,pcWidth bit))
    val inc = RegInit(False)
    val pcNext = pc + Mux(inc,U(4),U(0))
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.payload
    }

    io.i.cmd.valid := True
    io.i.cmd.pc := pcNext
    when(io.i.cmd.fire || pcLoad.fire){
      pc := pcNext
    }

    when(io.i.cmd.fire){
      inc := True
    }.elsewhen(pcLoad.valid){
      inc := False
    }

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
    }))
    outInst.valid := io.i.cmd.fire
    outInst.pc := pcNext
  }
  val fetch = new Area {
    val inContext = fetchCmd.outInst.throwWhen(io.i.flush).m2sPipe()
    inContext.ready := io.i.rsp.ready

    val outInst = Stream(wrap(new Bundle {
      val pc = UInt(pcWidth bit)
      val instruction = Bits(32 bit)
      val branchCacheLine = BranchPredictorLine()
    }))
    outInst.arbitrationFrom(io.i.rsp)
    outInst.pc := inContext.pc
    outInst.instruction := io.i.rsp.instruction
    outInst.branchCacheLine := brancheCache.readSync(Mux(inContext.isStall,inContext.pc,fetchCmd.outInst.pc)(2, branchPredictorSizeLog2 bit))
  }
  val decode = new Area{
    
    val inInst = fetch.outInst.throwWhen(io.i.flush).m2sPipe()
    val ctrl = InstructionCtrl(inInst.instruction)
    val hazard = False
    
    val addr0 = inInst.instruction(src0Range).asUInt
    val addr1 = inInst.instruction(src1Range).asUInt

    val srcInstruction = Mux(inInst.isStall,inInst.instruction,fetch.outInst.instruction)
    val src0 = regFile.readSync(srcInstruction(src0Range).asUInt)  //Overridden by the write back stage
    val src1 = regFile.readSync(srcInstruction(src1Range).asUInt)

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val instruction = Bits(32 bit)
      val ctrl = InstructionCtrl()
      val src0 = Bits(32 bit)
      val src1 = Bits(32 bit)
      val alu_op0 = Bits(32 bit)
      val alu_op1 = Bits(32 bit)
      val doSub = Bool
      val predictorHasBranch = Bool
      val branchHistory = Flow(SInt(branchPredictorHistoryWidth bit))
    }))

    val imm = IMM(inInst.instruction)

    val branchCacheHit = inInst.branchCacheLine.pc === inInst.pc(pcWidth-1 downto 2 + branchPredictorSizeLog2)

    val brjmpImm = Mux(ctrl.jmp, imm.j_sext, imm.b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt
    val staticBranchPrediction = brjmpImm.msb || ctrl.br === BR.J
    val shouldTakeBranch = Bool
    shouldTakeBranch := staticBranchPrediction
    if(enableDynamicBranchPrediction) {
      when(branchCacheHit) {
        shouldTakeBranch := inInst.branchCacheLine.history.msb
      }
    }

    val pcLoad = Flow(UInt(pcWidth bit))  //give token to pcload.valid and remove hazard flag
    pcLoad.valid := inInst.valid && !hazard && outInst.ready && (ctrl.br =/= BR.JR && ctrl.br =/= BR.N) && ctrl.instVal && shouldTakeBranch
    pcLoad.payload := brJumpPc

    outInst.arbitrationFrom(inInst.haltWhen(hazard))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.ctrl := ctrl
    outInst.doSub := outInst.ctrl.alu =/= ALU.ADD
    outInst.src0 := Mux(addr0 =/= 0, src0, B(0, 32 bit))
    outInst.src1 := Mux(addr1 =/= 0, src1, B(0, 32 bit))
    outInst.alu_op0 := outInst.ctrl.op1.map(
      default -> outInst.src0,
      OP1.IMU -> imm.u.resized,
      OP1.IMZ -> imm.z.resized
    )
    outInst.alu_op1 := outInst.ctrl.op2.map(
      default -> outInst.src1,
      OP2.IMI -> imm.i_sext.resized,
      OP2.IMS -> imm.s_sext.resized,
      OP2.PC1 -> inInst.pc.asBits.resized
    )
    outInst.predictorHasBranch := pcLoad.valid
    outInst.branchHistory.valid := branchCacheHit
    outInst.branchHistory.payload := inInst.branchCacheLine.history

    val flush = False
    when(flush){
      io.i.flush := True
      inInst.ready := True
    }
  }


  val execute0 = new Area {
    val inInst = decode.outInst.m2sPipe()
    val ctrl = inInst.ctrl
    val addr0 = inInst.instruction(19 downto 15).asUInt
    val addr1 = inInst.instruction(24 downto 20).asUInt

    val imm = IMM(inInst.instruction)

    val br = new Area {
      val eq = (inInst.src0 === inInst.src1)      //TODO opt
      val lt = (inInst.src0.asSInt < inInst.src1.asSInt)
      val ltu = (inInst.src0.asUInt < inInst.src1.asUInt)
    }



    val brjmpImm = Mux(ctrl.jmp, imm.j_sext, imm.b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt //TODO opt

    val alu = new Alu
    alu.io.func := inInst.ctrl.alu
    alu.io.doSub := inInst.doSub
    alu.io.src0 := inInst.alu_op0
    alu.io.src1 := inInst.alu_op1

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val ctrl = InstructionCtrl()
      val br = new Bundle {
        val eq, lt, ltu = Bool
      }
      val src1 = Bits(32 bit)
      val brJumpPc = UInt(32 bit)
      val instruction = Bits(32 bit)
      val alu = Bits(32 bit)
      val adder = UInt(32 bit)
      val shift = Bits(32 bit)
      val predictorHasBranch = Bool
      val branchHistory = Flow(SInt(branchPredictorHistoryWidth bit))
      val pcPlus4 = UInt(32 bit)
    }))
    outInst.arbitrationFrom(inInst)
    outInst.pc := inInst.pc
    outInst.ctrl := ctrl
    outInst.br.eq := br.eq
    outInst.br.lt := br.lt
    outInst.br.ltu := br.ltu
    outInst.src1 := inInst.src1
    outInst.brJumpPc := brJumpPc
    outInst.instruction := inInst.instruction
    outInst.alu := alu.io.result
    outInst.adder := alu.io.adder
    outInst.shift := alu.io.shift
    outInst.predictorHasBranch := inInst.predictorHasBranch
    outInst.branchHistory.valid   := inInst.branchHistory.valid
    outInst.branchHistory.payload := inInst.branchHistory.payload
    outInst.pcPlus4 := inInst.pc + 4


    val flush = False
    when(flush){
      io.i.flush := True
      decode.inInst.ready := True
      decode.outInst.valid := False
      inInst.ready := True
      outInst.valid := False
    }

    def doBypass() : Unit = {
      when(outInst.ctrl.aluBypass && outInst.ctrl.rfen && inInst.valid) {
        when(outInst.instruction(dstRange).asUInt === decode.addr0) {
          decode.src0 := outInst.alu
        }
        when(outInst.instruction(dstRange).asUInt === decode.addr1) {
          decode.src1 := outInst.alu
        }
      }
    }


  }

  val execute1 = new Area {
    val inInst = execute0.outInst.m2sPipe()

    io.d.cmd.valid := inInst.fire && inInst.ctrl.men
    io.d.cmd.wr := inInst.ctrl.m === M.XWR
    io.d.cmd.address := inInst.alu.asUInt
    io.d.cmd.payload.data := inInst.src1
    io.d.cmd.size := inInst.ctrl.msk.map(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.H -> U(1)
    )

    val take_evec = False
    //TODO
    val ctrl_pc_sel = inInst.ctrl.br.map[PC.T](
      default -> PC.INC,
      BR.NE -> Mux(!inInst.br.eq, PC.BR1, PC.INC),
      BR.EQ -> Mux(inInst.br.eq, PC.BR1, PC.INC),
      BR.GE -> Mux(!inInst.br.lt, PC.BR1, PC.INC),
      BR.GEU -> Mux(!inInst.br.ltu, PC.BR1, PC.INC),
      BR.LT -> Mux(inInst.br.lt, PC.BR1, PC.INC),
      BR.LTU -> Mux(inInst.br.ltu, PC.BR1, PC.INC),
      BR.J -> PC.J,
      BR.JR -> PC.JR
    )

    when(take_evec) {
      ctrl_pc_sel := PC.EXC
    }

    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := inInst.fire && inInst.ctrl.instVal && ((ctrl_pc_sel =/= PC.INC) =/= inInst.predictorHasBranch)
    pcLoad.payload := ctrl_pc_sel.map(
      default -> inInst.brJumpPc,
      PC.EXC -> U(startAddress),
      PC.JR -> inInst.adder,
      PC.INC -> inInst.pcPlus4
    )

    when(inInst.fire && inInst.ctrl.br =/= BR.JR && inInst.ctrl.br =/= BR.N && inInst.ctrl.br =/= BR.J){
      val line = BranchPredictorLine()
      val newHistory = inInst.branchHistory.payload.resize(branchPredictorHistoryWidth + 1) + Mux(ctrl_pc_sel === PC.INC,S(1),S(-1))
      line.pc := inInst.pc(pcWidth-1 downto 2 + branchPredictorSizeLog2)
      when(inInst.branchHistory.valid){
        line.history := newHistory.resized
      }otherwise {
        line.history := (ctrl_pc_sel =/= PC.INC).asSInt.resized
      }
      line.valid := True
      when(newHistory(newHistory.high downto newHistory.high - 1) =/= S"10") { //no history overflow
        brancheCache(inInst.pc(2, branchPredictorSizeLog2 bit)) := line
      }
    }

    val outInst = Stream(wrap(new Bundle {
      val pc = UInt(pcWidth bit)
      val alu = Bits(32 bit)
      val regFileAddress = UInt(5 bit)
      val ctrl = InstructionCtrl()
      val instruction = Bits(32 bit)
      val pcPlus4 = UInt(32 bit)
    }))
    outInst.arbitrationFrom(inInst.haltWhen((inInst.valid && inInst.ctrl.men && !io.d.cmd.ready)))
    outInst.pc := inInst.pc
    outInst.alu := inInst.ctrl.alu.map(
      (ALU.SLL1) -> Reverse(inInst.shift),
      (ALU.SRL1,ALU.SRA1) -> inInst.shift,
      default -> inInst.alu
    )
    outInst.regFileAddress := inInst.instruction(dstRange).asUInt
    outInst.ctrl := inInst.ctrl
    outInst.instruction := inInst.instruction
    outInst.pcPlus4 := inInst.pcPlus4

    def doBypass() : Unit ={
      when(outInst.ctrl.aluBypass && outInst.ctrl.rfen && inInst.valid) {
        when(outInst.instruction(dstRange).asUInt === decode.addr0) {
          decode.src0 := outInst.alu
        }
        when(outInst.instruction(dstRange).asUInt === decode.addr1) {
          decode.src1 := outInst.alu
        }
      }
    }
  }

  val writeBack = new Area{
    val inInst = execute1.outInst.m2sPipe()

    val dRspRfmt = inInst.ctrl.msk.map(
      default -> io.d.rsp.payload, //W
      MSK.B   -> B(default -> (io.d.rsp.payload(7) && ! inInst.instruction(14)),(7 downto 0) -> io.d.rsp.payload(7 downto 0)),
      MSK.H   -> B(default -> (io.d.rsp.payload(15) && ! inInst.instruction(14)),(15 downto 0) -> io.d.rsp.payload(15 downto 0))
    )

    val regFileData = inInst.ctrl.wb.map (
      default -> B(0,32 bit), //CSR1
      WB.ALU1 -> inInst.alu,
      WB.MEM  -> dRspRfmt,
      WB.PC4  -> (inInst.pcPlus4).asBits.resized
    )

    val regFileWriteBack = new Area{
      val valid = RegNext(False)
      val addr = RegNext(inInst.regFileAddress)
      val data = RegNext(regFileData)
    }

    when(inInst.fire && inInst.ctrl.rfen) {
      regFileWriteBack.valid := True
      regFile(inInst.regFileAddress) := regFileData
    }
    inInst.ready := inInst.ctrl.wb =/= WB.MEM || inInst.ctrl.m =/= M.XRD || io.d.rsp.valid

    def doBypass() : Unit ={
      when(regFileWriteBack.valid) {
        when(regFileWriteBack.addr === decode.addr0) {
          decode.src0 := regFileWriteBack.data
        }
        when(regFileWriteBack.addr === decode.addr1) {
          decode.src1 := regFileWriteBack.data
        }
      }
      when(inInst.ctrl.rfen && inInst.valid) {
        when(inInst.regFileAddress === decode.addr0) {
          decode.src0 := regFileData
        }
        when(inInst.regFileAddress === decode.addr1) {
          decode.src1 := regFileData
        }
      }
    }
  }


  val branchArbiter = new Area {
      fetchCmd.pcLoad.valid := decode.pcLoad.valid
      fetchCmd.pcLoad.payload := decode.pcLoad.payload
      when(decode.pcLoad.valid){
        decode.flush := True
      }
      when(execute1.pcLoad.valid){
        execute0.flush :=  True
        fetchCmd.pcLoad.valid := True
        fetchCmd.pcLoad.payload := execute1.pcLoad.payload
      }

    val loadCounter = Counter(1<<30,execute1.pcLoad.valid).value.keep()
    val flushCounter = Counter(1<<30,io.i.flush).value.keep()
  }


  val hazardTracker = new  Area {
    val checkAddr0 = decode.addr0 =/= 0
    val checkAddr1 = decode.addr1 =/= 0

    if(bypassWriteBack) {
      writeBack.doBypass()
    }else{
      when(writeBack.inInst.valid && writeBack.inInst.ctrl.rfen) {
        when(checkAddr0 && decode.addr0 === writeBack.inInst.regFileAddress) {
          decode.hazard := True
        }
        when(checkAddr1 && writeBack.inInst.valid && decode.addr1 === writeBack.inInst.regFileAddress && writeBack.inInst.ctrl.rfen) {
          decode.hazard := True
        }
      }
      when(writeBack.regFileWriteBack.valid) {
        when(checkAddr0 && decode.addr0 === writeBack.regFileWriteBack.addr) {
          decode.hazard := True
        }
        when(checkAddr1 && decode.addr1 === writeBack.regFileWriteBack.addr) {
          decode.hazard := True
        }
      }
    }
    if(bypassExecute1) {
      execute1.doBypass()
    }
    when(execute1.inInst.valid && execute1.inInst.ctrl.rfen && (Bool(!bypassExecute1) || !execute1.inInst.ctrl.aluBypass)) { //TODO alu execute 1 by pass != aluBypass
      when(checkAddr0 && decode.addr0 === execute1.outInst.regFileAddress) {
        decode.hazard := True
      }
      when(checkAddr1 && decode.addr1 === execute1.outInst.regFileAddress) {
        decode.hazard := True
      }
    }

    if(bypassExecute0) {
      execute0.doBypass()
    }
    when(execute0.inInst.valid && execute0.inInst.ctrl.rfen && (Bool(!bypassExecute0) || !execute0.inInst.ctrl.aluBypass)){
      when(checkAddr0 && decode.addr0 === execute0.outInst.instruction(dstRange).asUInt) {
        decode.hazard := True
      }
      when(checkAddr1 && decode.addr1 === execute0.outInst.instruction(dstRange).asUInt) {
        decode.hazard := True
      }
    }
  }
}




object CoreMain{
  def main(args: Array[String]) {
    implicit val p = CoreParm(
      pcWidth = 32,
      addrWidth = 32,
      enableDynamicBranchPrediction = false,
      bypassExecute0 = false,
      bypassExecute1 = false,
      bypassWriteBack = false,
      startAddress = 0x200,
      branchPredictorSizeLog2 = 7)
    SpinalVhdl(new Core(),_.setLibrary("riscv"))
  }
}



//val br_src0 = (src0.msb && br_signed) ## src0
//val br_src1 = (src1.msb && br_signed) ## src1
//br_result :=  Mux(br_eq,(src0 === src1),(br_src0.asUInt-br_src1.asUInt).msb)

