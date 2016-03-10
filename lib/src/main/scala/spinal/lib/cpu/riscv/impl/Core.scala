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

  val regFile = Mem(Bits(32 bit),32)

  val fetch = new Area {
    val pc = Reg(UInt(pcWidth bit)) init(U(startAddress,pcWidth bit))
    val inc = RegInit(False)
    val pcNext = pc + Mux(inc,U(4),U(0))
    //val pcPredict = UInt(32 bit)
 //   val pcNext = Mux(inc,pcPredict,pc)
   // val pcNext = UInt(32 bit)
    val pcLoad = Flow(pc)
    io.i.flush := pcLoad.valid
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
    val inInst = io.i.rsp.throwWhen(io.i.flush).m2sPipe().throwWhen(io.i.flush)

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
    }))

    // immediates
    val imm_i = inInst.instruction(31 downto 20)
    val imm_s = inInst.instruction(31, 25) ## inInst.instruction(11, 7)
    val imm_b = inInst.instruction(31) ## inInst.instruction(7) ## inInst.instruction(30 downto 25) ## inInst.instruction(11 downto 8)
    val imm_u = inInst.instruction(31 downto 12) ## U"x000"
    val imm_j = inInst.instruction(31) ## inInst.instruction(19 downto 12) ## inInst.instruction(20) ## inInst.instruction(30 downto 21)
    val imm_z = inInst.instruction(19 downto 15)

    // sign-extend immediates
    val imm_i_sext = B((19 downto 0) -> imm_i(11)) ## imm_i
    val imm_s_sext = B((19 downto 0) -> imm_s(11)) ## imm_s
    val imm_b_sext = B((18 downto 0) -> imm_b(11)) ## imm_b ## False
    val imm_j_sext = B((10 downto 0) -> imm_j(19)) ## imm_j ## False

    outInst.arbitrationFrom(inInst.haltWhen(hazard))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.ctrl := InstructionCtrl(inInst.instruction)
    outInst.src0 := Mux(addr0 =/= 0, src0, B(0, 32 bit))
    outInst.src1 := Mux(addr1 =/= 0, src1, B(0, 32 bit))
    outInst.alu_op0 := outInst.ctrl.op1.map(
      default -> outInst.src0,
      OP1.IMU -> imm_u.resized,
      OP1.IMZ -> imm_z.resized
    )
    outInst.alu_op1 := outInst.ctrl.op2.map(
      default -> outInst.src1,
      OP2.IMI -> imm_i_sext.resized,
      OP2.IMS -> imm_s_sext.resized,
      OP2.PC1 -> inInst.pc.asBits.resized
    )
  }


  val execute0 = new Area {
    val inInst = decode.outInst.throwWhen(io.i.flush).m2sPipe()
    val ctrl = inInst.ctrl
    val addr0 = inInst.instruction(19 downto 15).asUInt
    val addr1 = inInst.instruction(24 downto 20).asUInt

    // immediates
    val imm_i = inInst.instruction(31 downto 20)
    val imm_s = inInst.instruction(31, 25) ## inInst.instruction(11, 7)
    val imm_b = inInst.instruction(31) ## inInst.instruction(7) ## inInst.instruction(30 downto 25) ## inInst.instruction(11 downto 8)
    val imm_u = inInst.instruction(31 downto 12) ## U"x000"
    val imm_j = inInst.instruction(31) ## inInst.instruction(19 downto 12) ## inInst.instruction(20) ## inInst.instruction(30 downto 21)
    val imm_z = inInst.instruction(19 downto 15)

    // sign-extend immediates
    val imm_i_sext = B((19 downto 0) -> imm_i(11)) ## imm_i
    val imm_s_sext = B((19 downto 0) -> imm_s(11)) ## imm_s
    val imm_b_sext = B((18 downto 0) -> imm_b(11)) ## imm_b ## False
    val imm_j_sext = B((10 downto 0) -> imm_j(19)) ## imm_j ## False

    val br = new Area {
      val eq = (inInst.src0 === inInst.src1)      //TODO opt
      val lt = (inInst.src0.asSInt < inInst.src1.asSInt)
      val ltu = (inInst.src0.asUInt < inInst.src1.asUInt)
    }



    val brjmpImm = Mux(ctrl.jmp, imm_j_sext, imm_b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val ctrl = InstructionCtrl()
      val br = new Bundle {
        val eq, lt, ltu = Bool
      }
      val alu_op0 = Bits(32 bit)
      val alu_op1 = Bits(32 bit)
      val src1 = Bits(32 bit)
      val brJumpPc = UInt(32 bit)
      val instruction = Bits(32 bit)
    }))
    outInst.arbitrationFrom(inInst)
    outInst.pc := inInst.pc
    outInst.ctrl := ctrl
    outInst.br.eq := br.eq
    outInst.br.lt := br.lt
    outInst.br.ltu := br.ltu
    outInst.alu_op0 := inInst.alu_op0
    outInst.alu_op1 := inInst.alu_op1
    outInst.src1 := inInst.src1
    outInst.brJumpPc := brJumpPc
    outInst.instruction := inInst.instruction
  }

  val execute1 = new Area{
    val inInst = execute0.outInst.clone()
    val rValid = RegInit(False)
    val rData = Reg(execute0.outInst.dataType)
    execute0.outInst.ready := (!inInst.valid) || inInst.ready

    when(execute0.outInst.ready) {
      rValid := execute0.outInst.valid
      rData := execute0.outInst.payload
    }
    when(io.i.flush){
      rValid := False
    }
    inInst.valid := rValid
    inInst.payload := rData

    val alu = new Alu
    alu.io.func := inInst.ctrl.alu
    alu.io.src0 := inInst.alu_op0
    alu.io.src1 := inInst.alu_op1

    io.d.cmd.valid := inInst.fire && inInst.ctrl.men
    io.d.cmd.wr := inInst.ctrl.m === M.XWR
    io.d.cmd.address := alu.io.result.asUInt
    io.d.cmd.payload.data := inInst.src1
    io.d.cmd.size := inInst.ctrl.msk.map(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.H -> U(1)
    )

    val take_evec = False //TODO
    val ctrl_pc_sel = inInst.ctrl.br.map[PC.T](
        default -> PC.INC,
        BR.NE   -> Mux(!inInst.br.eq,  PC.BR1, PC.INC),
        BR.EQ   -> Mux( inInst.br.eq,  PC.BR1, PC.INC),
        BR.GE   -> Mux(!inInst.br.lt,  PC.BR1, PC.INC),
        BR.GEU  -> Mux(!inInst.br.ltu, PC.BR1, PC.INC),
        BR.LT   -> Mux( inInst.br.lt,  PC.BR1, PC.INC),
        BR.LTU  -> Mux( inInst.br.ltu, PC.BR1, PC.INC),
        BR.J    -> PC.J,
        BR.JR   -> PC.JR
      )

    when(take_evec){
      ctrl_pc_sel := PC.EXC
    }

    //    io.i.flush := ctrl_pc_sel =/= PC.INC && inInst.fire //Throw fetched/decoded instruction if PC is not incremental

    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := inInst.fire && !(ctrl_pc_sel === PC.INC) && inInst.ctrl.instVal
    pcLoad.payload := ctrl_pc_sel.map(
      default -> inInst.brJumpPc,
      PC.EXC -> U(startAddress),
      PC.JR ->  alu.io.adder
    )

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val alu = Bits(32 bit)
      val regFileAddress = UInt(5 bit)
      val ctrl = InstructionCtrl()
      val instruction = Bits(32 bit)
    }))
    outInst.arbitrationFrom(inInst.haltWhen((inInst.valid && inInst.ctrl.men && !io.d.cmd.ready)))
    outInst.pc := inInst.pc
    outInst.alu := alu.io.result.asBits
    outInst.regFileAddress := inInst.instruction(regFileRange).asUInt
    outInst.ctrl := inInst.ctrl
    outInst.instruction := inInst.instruction
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
      WB.PC4  -> (inInst.pc + 4).asBits.resized
    )

    val regFileWriteBack = new Area{
      val valid = RegNext(False)
      val addr = RegNext(inInst.regFileAddress)
    }

    when(inInst.fire && inInst.ctrl.rfen) {
      regFileWriteBack.valid := True
      regFile(inInst.regFileAddress) := regFileData
    }
    inInst.ready := inInst.ctrl.wb =/= WB.MEM || inInst.ctrl.m =/= M.XRD || io.d.rsp.valid
  }

  val noBranchPredictor = new Area{
    fetch.pcLoad.valid := execute1.pcLoad.valid
    fetch.pcLoad.payload := execute1.pcLoad.payload
//    fetch.pcNext := fetch.pc + Mux(fetch.inc,U(4),U(0))

    val loadCounter = Counter(1<<30,execute1.pcLoad.valid).value.keep()
    val flushCounter = Counter(1<<30,io.i.flush).value.keep()
  }

//  val branchPredictor = new Area{
//    val branchPredictorSize = 1<<branchPredictorSizeLog2
//    val cache = Mem(BranchPredictorLine(),branchPredictorSize)
//
//    val line = cache.readSync(fetch.pcNext(branchPredictorSizeLog2-1 downto 0),io.i.cmd.fire  || fetch.pcLoad.fire,writeToReadKind = readFirst)
//    when(line.valid && line.pc === fetch.pc(pcWidth-1 downto branchPredictorSizeLog2)){
//      fetch.pcPredict := line.jump
//    }otherwise{
//      fetch.pcPredict := fetch.pc + 4
//    }
//
//    val write = Flow(wrap(new Bundle{
//      val addr = UInt(branchPredictorSizeLog2 bit)
//      val line = BranchPredictorLine()
//    }))
//
//    when(write.valid){
//      cache(write.addr) := write.line
//    }
//
//    val nextPc = UInt(pcWidth bit)
//    val nextPcReg =  RegNext(nextPc) init(U(startAddress,pcWidth bit))
//    nextPc := nextPcReg
//    when(execute1.inInst.fire){
//      when(execute1.pcLoad.valid){
//        nextPc := execute1.pcLoad.payload
//      }otherwise{
//        nextPc := execute1.inInst.pc + 4
//      }
//    }
//
//
//
//    fetch.pcLoad.valid := execute0.outInst.valid && execute0.outInst.pc =/= nextPc
//    fetch.pcLoad.payload := nextPc
//    //    fetch.pcLoad.valid := execute1.pcLoad.valid
//    //    fetch.pcLoad.payload := execute1.pcLoad.payload
//
//    write.valid := execute1.inInst.fire
//    write.addr := execute1.inInst.pc(branchPredictorSizeLog2-1 downto 0)
//    write.line.valid := True
//    write.line.pc := execute1.inInst.pc(pcWidth-1 downto branchPredictorSizeLog2)
//    write.line.jump := nextPc
//
//
//    val initCounter = RegInit(U(0,branchPredictorSizeLog2+1 bit))
//    val ready = initCounter.msb
//    when(!ready){
//      initCounter := initCounter + 1
//      write.valid := True
//      write.addr := initCounter.resized
//      write.line.valid := False
//      decode.hazard := True
//    }
//
//    val loadCounter = Counter(1<<30,execute1.pcLoad.valid).value.keep()
//    val flushCounter = Counter(1<<30,io.i.flush).value.keep()
//  }

  val hazardTracker = new  Area{
    val checkAddr0 = decode.addr0 =/= 0
    val checkAddr1 = decode.addr1 =/= 0
    when(checkAddr0 && execute0.inInst.valid && decode.addr0 === execute0.outInst.instruction(regFileRange).asUInt && execute0.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr1 && execute0.inInst.valid && decode.addr1 === execute0.outInst.instruction(regFileRange).asUInt && execute0.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr0 && execute1.inInst.valid && decode.addr0 === execute1.outInst.regFileAddress && execute1.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr1 && execute1.inInst.valid && decode.addr1 === execute1.outInst.regFileAddress && execute1.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr0 && writeBack.inInst.valid && decode.addr0 === writeBack.inInst.regFileAddress && writeBack.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr1 && writeBack.inInst.valid && decode.addr1 === writeBack.inInst.regFileAddress && writeBack.inInst.ctrl.rfen){
      decode.hazard := True
    }
    when(checkAddr0 && writeBack.regFileWriteBack.valid && decode.addr0 === writeBack.regFileWriteBack.addr ){
      decode.hazard := True
    }
    when(checkAddr1 && writeBack.regFileWriteBack.valid && decode.addr1 === writeBack.regFileWriteBack.addr ){
      decode.hazard := True
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