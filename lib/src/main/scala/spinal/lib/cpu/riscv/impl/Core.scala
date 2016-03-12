package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils._


case class CoreParm(val pcWidth : Int = 32,val addrWidth : Int = 32,val startAddress : Int = 0,val branchPredictorSizeLog2 : Int = 4)

case class CoreInstCmd(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.addrWidth bit)
}
case class CoreInstRsp(implicit p : CoreParm) extends Bundle{
  val pc = UInt(p.pcWidth bit)
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
  val pc = UInt(p.pcWidth-p.branchPredictorSizeLog2 bit)
  val jump = UInt(p.pcWidth bit)
}
// assert(latencyAnalysis(io.iCmd.data,io.iRsp.data) == 1)
class Core(implicit p : CoreParm) extends Component{
  import p._
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

  val fetch = new Area {
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
  }

  val decode = new Area{
    val hazard = False
    val inInst = io.i.rsp.throwWhen(io.i.flush).m2sPipe()
    val ctrl = InstructionCtrl(inInst.instruction)

    val addr0 = inInst.instruction(src0Range).asUInt
    val addr1 = inInst.instruction(src1Range).asUInt

    val srcInstruction = Mux(inInst.isStall,inInst.instruction,io.i.rsp.instruction)
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
    }))

    val imm = IMM(inInst.instruction)

    val brjmpImm = Mux(ctrl.jmp, imm.j_sext, imm.b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt

    val pcLoad = Flow(UInt(pcWidth bit))  //give token to pcload.valid and remove hazard flag
    pcLoad.valid := inInst.valid && !hazard && outInst.ready && (ctrl.br =/= BR.JR && ctrl.br =/= BR.N) && ctrl.instVal && (brjmpImm.msb || ctrl.br === BR.J)
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
      val predictorHasBranch = Bool
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
    outInst.predictorHasBranch := inInst.predictorHasBranch
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
    outInst.alu := inInst.alu
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
      fetch.pcLoad.valid := decode.pcLoad.valid
      fetch.pcLoad.payload := decode.pcLoad.payload
      when(decode.pcLoad.valid){
        decode.flush := True
      }
      when(execute1.pcLoad.valid){
        execute0.flush :=  True
        fetch.pcLoad.valid := True
        fetch.pcLoad.payload := execute1.pcLoad.payload
      }
    

    val loadCounter = Counter(1<<30,execute1.pcLoad.valid).value.keep()
    val flushCounter = Counter(1<<30,io.i.flush).value.keep()
  }


  val hazardTracker = new  Area {
    writeBack.doBypass()
    execute1.doBypass()
    execute0.doBypass()


    val checkAddr0 = decode.addr0 =/= 0
    val checkAddr1 = decode.addr1 =/= 0
    when(execute0.inInst.valid && execute0.inInst.ctrl.rfen && !execute0.inInst.ctrl.aluBypass){
      when(checkAddr0 && decode.addr0 === execute0.outInst.instruction(dstRange).asUInt) {
        decode.hazard := True
      }
      when(checkAddr1 && decode.addr1 === execute0.outInst.instruction(dstRange).asUInt) {
        decode.hazard := True
      }
    }
    when(execute1.inInst.valid && execute1.inInst.ctrl.rfen && !execute1.inInst.ctrl.aluBypass) {
      when(checkAddr0 && decode.addr0 === execute1.outInst.regFileAddress) {
        decode.hazard := True
      }
      when(checkAddr1 && decode.addr1 === execute1.outInst.regFileAddress) {
        decode.hazard := True
      }
    }
    when(writeBack.inInst.valid && writeBack.inInst.ctrl.rfen ) {
      when(checkAddr0 && decode.addr0 === writeBack.inInst.regFileAddress) {
//        decode.hazard := True
      }
      when(checkAddr1 && writeBack.inInst.valid && decode.addr1 === writeBack.inInst.regFileAddress && writeBack.inInst.ctrl.rfen) {
//        decode.hazard := True
      }
    }
    when(writeBack.regFileWriteBack.valid) {
      when(checkAddr0 && decode.addr0 === writeBack.regFileWriteBack.addr) {
//        decode.hazard := True
      }
      when(checkAddr1 && decode.addr1 === writeBack.regFileWriteBack.addr) {
//        decode.hazard := True
      }
    }
  }
}




object CoreMain{
  def main(args: Array[String]) {
    implicit val p = CoreParm(32,32,0x200,7)
    SpinalVhdl(new Core(),_.setLibrary("riscv"))
  }
}


//    val hit = new Area{
//      val line = cache(execute1.inInst.pc(branchPredictorSizeLog2-1 downto 0)) //TODO OPT
//      val valid = line.valid && line.pc === execute1.inInst.pc(pcWidth-1 downto branchPredictorSizeLog2)
//    }
//    when(execute1.inInst.fire){
//      when(execute1.pcLoad.valid){
//        write.valid := hit.valid
//        write.line.valid := True
//      }otherwise{
//        when(hit.valid){
//          write.valid := True
//          write.line.valid := False
//        }
//      }
//    }


//val br_src0 = (src0.msb && br_signed) ## src0
//val br_src1 = (src1.msb && br_signed) ## src1
//br_result :=  Mux(br_eq,(src0 === src1),(br_src0.asUInt-br_src1.asUInt).msb)





//
//    //Reg file read after write bypass
//    when(regFileWriteBack.valid){
//      when(regFileWriteBack.addr === execute.addr0) {
//        execute.inInst.src0 := regFileWriteBack.data
//      }
//      when(regFileWriteBack.addr === execute.addr1) {
//        execute.inInst.src1 := regFileWriteBack.data
//      }
//    }
//
//    //ALU result bypass
//    when(inInst.valid && inInst.ctrl.ren && inInst.ctrl.bypassable){ //TODO bypassable is maybe not usefull
//      when(inInst.regFileAddress === execute.addr0) {
//        execute.inInst.src0 := inInst.alu
//      }
//      when(inInst.regFileAddress === execute.addr1) {
//        execute.inInst.src1 := inInst.alu
//      }
//    }


//    val regFileBypass = new Area{
//      val valid = RegNext(False)
//      val addr = RegNext(inInst.regFileAddress)
//      val data = RegNext(regFileData)
//    }
//
//    when(inInst.fire && inInst.ctrl.ren) {
//      regFileBypass.valid := True
//      regFile(inInst.regFileAddress) := regFileData
//      when(!inInst.ctrl.bypassable && inInst.regFileAddress =/= 0 && (inInst.regFileAddress === execute.addr0 || inInst.regFileAddress === execute.addr1)){//TODO redondoent logic with alu result bypass
//        execute.hazard := True
//      }
//    }
//    inInst.ready := inInst.ctrl.wb =/= WB.MEM || inInst.ctrl.m =/= M.XRD || io.d.rsp.valid
//
//    //Reg file read after write bypass
//    when(regFileBypass.valid){
//      when(regFileBypass.addr === execute.addr0) {
//        execute.inInst.src0 := regFileBypass.data
//      }
//      when(regFileBypass.addr === execute.addr1) {
//        execute.inInst.src1 := regFileBypass.data
//      }
//    }
//
//    //ALU result bypass
//    when(inInst.valid && inInst.ctrl.ren && inInst.ctrl.bypassable){ //TODO bypassable is maybe not usefull
//      when(inInst.regFileAddress === execute.addr0) {
//        execute.inInst.src0 := inInst.alu
//      }
//      when(inInst.regFileAddress === execute.addr1) {
//        execute.inInst.src1 := inInst.alu
//      }
//    }