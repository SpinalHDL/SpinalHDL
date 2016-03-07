package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils._


case class CoreParm(val pcWidth : Int = 32,val addrWidth : Int = 32,val startAddress : Int = 0)

case class CoreInstCmd(implicit p : CoreParm) extends Bundle{
  val address = UInt(p.addrWidth bit)
}

case class CoreDataCmd(implicit p : CoreParm) extends Bundle{
  val wr = Bool
  val address = UInt(p.addrWidth bit)
  val data = Bits(32 bit)
  val size = UInt(2 bit)
}

// assert(latencyAnalysis(io.iCmd.data,io.iRsp.data) == 1)
class Core(implicit p : CoreParm) extends Component{
  import p._
  val io = new Bundle{
    val iCmd = master Stream(CoreInstCmd())
    val iRsp = slave Stream(Bits(32 bit))

    val dCmd = master Stream(CoreDataCmd())
    val dRsp = slave Flow(Bits(32 bit))
  }

  val regFile = Mem(Bits(32 bit),32)

  val fetch = new Area {
    val pc = Reg(UInt(pcWidth bit)) init(U(startAddress,pcWidth bit)-4)
    val pcNext = pc + 4
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.payload
    }

    io.iCmd.valid := True
    io.iCmd.address := pcNext
    when(io.iCmd.fire){
      pc := pcNext
    }
  }

  val decode = new Area{
    val drop = Bool

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val instruction = Bits(32 bit)
    }))

    outInst.arbitrationFrom(io.iRsp.throwWhen(drop))
    outInst.pc := fetch.pc
    outInst.instruction := io.iRsp.payload
  }


  val execute = new Area{
    val hazard = False
    val inInst = decode.outInst.m2sPipe()
    val ctrl = InstructionCtrl(inInst.instruction)
    val addr0 = inInst.instruction(19 downto 15).asUInt
    val addr1 = inInst.instruction(24 downto 20).asUInt

    // immediates
    val imm_i = inInst.instruction(31 downto 20)
    val imm_s = inInst.instruction(31, 25) ## inInst.instruction(11,7)
    val imm_b = inInst.instruction(31) ## inInst.instruction(7) ## inInst.instruction(30 downto 25) ## inInst.instruction(11 downto 8)
    val imm_u = inInst.instruction(31 downto 12) ## U"x000"
    val imm_j = inInst.instruction(31) ## inInst.instruction(19 downto 12) ## inInst.instruction(20) ## inInst.instruction(30 downto 21)
    val imm_z = inInst.instruction(19 downto 15)

    // sign-extend immediates
    val imm_i_sext = B((19 downto 0) -> imm_i(11)) ## imm_i
    val imm_s_sext = B((19 downto 0) -> imm_s(11)) ## imm_s
    val imm_b_sext = B((18 downto 0) -> imm_b(11)) ## imm_b ## False
    val imm_j_sext = B((10 downto 0) -> imm_j(19)) ## imm_j ## False

    val bypassSrc0 = regFile.readSync(Mux(inInst.isStall,addr0,io.iRsp.payload(19 downto 15).asUInt))  //Overridden by the write back stage
    val bypassSrc1 = regFile.readSync(Mux(inInst.isStall,addr1,io.iRsp.payload(24 downto 20).asUInt))

    val src0 = Mux(addr0 =/= 0, bypassSrc0, B(0, 32 bit))
    val src1 = Mux(addr1 =/= 0, bypassSrc1, B(0, 32 bit))

    val alu_op1 = ctrl.op1.map(
      default -> src0,
      OP1.IMU -> imm_u.resized,
      OP1.IMZ -> imm_z.resized
    )
    val alu_op2 = ctrl.op2.map(
      default -> src1,
      OP2.IMI -> imm_i_sext.resized,
      OP2.IMS -> imm_s_sext.resized,
      OP2.PC1 -> inInst.pc.asBits.resized
    )

    val alu = new Alu
    alu.io.func := ctrl.alu
    alu.io.src1 := alu_op1
    alu.io.src0 := alu_op2
    
    io.dCmd.valid := inInst.fire && ctrl.men
    io.dCmd.wr := ctrl.m === M.XWR
    io.dCmd.address := alu.io.result.asUInt
    io.dCmd.payload.data := src1
    io.dCmd.size := ctrl.msk.map(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.BU -> U(0),
      MSK.H -> U(1),
      MSK.HU -> U(1)
    )

    val br_eq  = (src0 === src1)
    val br_lt  = (src0.asSInt < src1.asSInt) //TODO opt
    val br_ltu = (src0.asUInt < src1.asUInt)

    val brjmpImm = Mux(ctrl.jmp, imm_j_sext, imm_b_sext)
    val brJumpPc = inInst.pc + brjmpImm.asUInt
    val take_evec = False //TODO

    val ctrl_pc_sel = ctrl.br.map[PC.T](
      default -> PC.INC,
      BR.NE   -> Mux(!br_eq,  PC.BR1, PC.INC),
      BR.EQ   -> Mux( br_eq,  PC.BR1, PC.INC),
      BR.GE   -> Mux(!br_lt,  PC.BR1, PC.INC),
      BR.GEU  -> Mux(!br_ltu, PC.BR1, PC.INC),
      BR.LT   -> Mux( br_lt,  PC.BR1, PC.INC),
      BR.LTU  -> Mux( br_ltu, PC.BR1, PC.INC),
      BR.J    -> PC.J,
      BR.JR   -> PC.JR
    )

    when(take_evec){
      ctrl_pc_sel := PC.EXC
    }

    decode.drop := ctrl_pc_sel =/= PC.INC && inInst.fire //Throw decoded instruction if PC is not incremental

    fetch.pcLoad.valid := inInst.valid && !(ctrl_pc_sel === PC.INC) && ctrl.instVal
    fetch.pcLoad.payload := ctrl_pc_sel.map(
      default -> brJumpPc,
      PC.EXC -> U(startAddress),
      PC.JR ->  alu.io.adder
    )

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val alu = Bits(32 bit)
      val regFileAddress = UInt(5 bit)
      val ctrl = InstructionCtrl()
    }))
    outInst.arbitrationFrom(inInst.haltWhen((inInst.valid && ctrl.men && !io.dCmd.ready) || hazard))
    outInst.pc := fetch.pc
    outInst.alu := alu.io.result.asBits
    outInst.regFileAddress := inInst.instruction(11 downto 7).asUInt
    outInst.ctrl := ctrl
  }

  val writeBack = new Area{
    val inInst = execute.outInst.m2sPipe()

    val dRspRfmt = inInst.ctrl.msk.map(
      default -> io.dRsp.payload, //W
      MSK.B   -> B(default -> io.dRsp.payload(7),(7 downto 0) -> io.dRsp.payload(7 downto 0)),
      MSK.BU  -> B(default -> false,(7 downto 0) -> io.dRsp.payload(7 downto 0)),
      MSK.H   -> B(default -> io.dRsp.payload(15),(15 downto 0) -> io.dRsp.payload(15 downto 0)),
      MSK.HU  -> B(default -> false,(15 downto 0) -> io.dRsp.payload(15 downto 0))
    )

    val regFileData = inInst.ctrl.wb.map (
      default -> B(0,32 bit), //CSR1
      WB.ALU1 -> inInst.alu,
      WB.MEM  -> dRspRfmt,
      WB.PC4  -> inInst.pc.asBits.resized
    )

    val regFileBypass = new Area{
      val valid = RegNext(False)
      val addr = RegNext(inInst.regFileAddress)
      val data = RegNext(regFileData)
    }

    when(inInst.fire && inInst.ctrl.ren) {
      regFileBypass.valid := True
      regFile(inInst.regFileAddress) := regFileData
      when(!inInst.ctrl.bypassable && inInst.regFileAddress =/= 0 && (inInst.regFileAddress === execute.addr0 || inInst.regFileAddress === execute.addr1)){//TODO redondoent logic with alu result bypass
        execute.hazard := True
      }
    }
    inInst.ready := inInst.ctrl.wb =/= WB.MEM || inInst.ctrl.m =/= M.XRD || io.dRsp.valid

    //Reg file read after write bypass
    when(regFileBypass.valid){
      when(regFileBypass.addr === execute.addr0) {
        execute.bypassSrc0 := regFileBypass.data
      }
      when(regFileBypass.addr === execute.addr1) {
        execute.bypassSrc1 := regFileBypass.data
      }
    }

    //ALU result bypass
    when(inInst.valid && inInst.ctrl.ren && inInst.ctrl.bypassable){ //TODO bypassable is maybe not usefull
      when(inInst.regFileAddress === execute.addr0) {
        execute.bypassSrc0 := inInst.alu
      }
      when(inInst.regFileAddress === execute.addr1) {
        execute.bypassSrc1 := inInst.alu
      }
    }
  }

}




object CoreMain{
  def main(args: Array[String]) {
    implicit val p = CoreParm(32,32,0x200)
    SpinalVhdl(new Core(),_.setLibrary("riscv"))
  }
}


//val br_src0 = (src0.msb && br_signed) ## src0
//val br_src1 = (src1.msb && br_signed) ## src1
//br_result :=  Mux(br_eq,(src0 === src1),(br_src0.asUInt-br_src1.asUInt).msb)