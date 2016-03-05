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
  //regFile.initialContent = Array.fill(32)(B"x00014000")

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
    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val instruction = Bits(32 bit)
    }))

    val reg0 = regFile.readSync(io.iRsp.payload(19 downto 15).asUInt,outInst.fire)
    val reg1 = regFile.readSync(io.iRsp.payload(24 downto 20).asUInt,outInst.fire)


    outInst.arbitrationFrom(io.iRsp)
    outInst.pc := fetch.pc
    outInst.instruction := io.iRsp.payload

  }


  val execute = new Area{
    val drop = Bool
    val stall = False
    val inInst = decode.outInst.throwWhen(drop).m2sPipe().haltWhen(stall)
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

    val bypassedSrc0 = Bits(32 bit)
    val bypassedSrc1 = Bits(32 bit)

    bypassedSrc0 := decode.reg0
    bypassedSrc1 := decode.reg1

    val src0 = Mux(addr0 =/= 0, bypassedSrc0, B(0, 32 bit))
    val src1 = Mux(addr1 =/= 0, bypassedSrc1, B(0, 32 bit))


    val exe_alu_op1 = ctrl.op1.map(
      default -> src0,
      OP1.IMU -> imm_u.resized,
      OP1.IMZ -> imm_z.resized
    )
    val exe_alu_op2 = ctrl.op2.map(
      default -> src1,
      OP2.IMI -> imm_i_sext.resized,
      OP2.IMS -> imm_s_sext.resized,
      OP2.PC1 -> inInst.pc.asBits.resized
    )

    val alu = new Alu
    alu.io.fn := ctrl.alu
    alu.io.in1 := exe_alu_op1.asUInt
    alu.io.in2 := exe_alu_op2.asUInt


    io.dCmd.valid := ctrl.men
    io.dCmd.wr := ctrl.m === M.XWR
    io.dCmd.address := alu.io.result
    io.dCmd.payload.data := src1
    io.dCmd.size := ctrl.msk.map(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.BU -> U(0),
      MSK.H -> U(1),
      MSK.HU -> U(1)
    )

    val br_eq  = (src0 === src1) //TODO opt
    val br_lt  = (src0.asSInt < src1.asSInt)
    val br_ltu = (src0.asUInt < src1.asUInt)


    val imm_brjmp = Mux(ctrl.jmp, imm_j_sext, imm_b_sext)
    val exe_brjmp_target = inInst.pc + imm_brjmp.asUInt
    val exe_jump_reg_target = alu.io.adder_out
    val take_evec = False //TODO

    val ctrl_pc_sel = Mux(take_evec            ,  PC.EXC,
      Mux(ctrl.br === BR.N  ,  PC.INC,
        Mux(ctrl.br === BR.NE ,  Mux(!br_eq,  PC.BR1, PC.INC),
          Mux(ctrl.br === BR.EQ ,  Mux( br_eq,  PC.BR1, PC.INC),
            Mux(ctrl.br === BR.GE ,  Mux(!br_lt,  PC.BR1, PC.INC),
              Mux(ctrl.br === BR.GEU,  Mux(!br_ltu, PC.BR1, PC.INC),
                Mux(ctrl.br === BR.LT ,  Mux( br_lt,  PC.BR1, PC.INC),
                  Mux(ctrl.br === BR.LTU,  Mux( br_ltu, PC.BR1, PC.INC),
                    Mux(ctrl.br === BR.J  ,  PC.J,
                      Mux(ctrl.br === BR.JR ,  PC.JR,
                        PC.INC
                      ))))))))))
    drop := ctrl_pc_sel =/= PC.INC && inInst.fire

    val exception_target = U(0) //TODO
    val take_pc = Mux(ctrl_pc_sel === PC.EXC,   exception_target,
      Mux(ctrl_pc_sel === PC.JR,    exe_jump_reg_target,
        exe_brjmp_target)) // PC_BR or PC_J

    fetch.pcLoad.valid := inInst.valid && !(ctrl_pc_sel === PC.INC) && ctrl.instVal
    fetch.pcLoad.payload := take_pc

    val outInst = Stream(wrap(new Bundle{
      val pc = UInt(pcWidth bit)
      val alu = Bits(32 bit)
      val regFileAddress = UInt(5 bit)
      val ctrl = InstructionCtrl()
    }))
    outInst.arbitrationFrom(inInst.haltWhen(io.dCmd.isStall))
    outInst.pc := fetch.pc
    outInst.alu := alu.io.result.asBits
    outInst.regFileAddress := inInst.instruction(11 downto 7).asUInt
    outInst.ctrl := ctrl
  }

  val writeBack = new Area{
    val inInst = execute.outInst.m2sPipe()

    val rspPayload = inInst.ctrl.msk.map(
      default -> io.dRsp.payload, //W
      MSK.B   -> B(default -> io.dRsp.payload(7),(7 downto 0) -> io.dRsp.payload(7 downto 0)),
      MSK.BU   -> B(default -> false,(7 downto 0) -> io.dRsp.payload(7 downto 0)),
      MSK.H   -> B(default -> io.dRsp.payload(15),(15 downto 0) -> io.dRsp.payload(15 downto 0)),
      MSK.HU   -> B(default -> false,(15 downto 0) -> io.dRsp.payload(15 downto 0))
    )

    val writeData = inInst.ctrl.wb.map (
      default -> B(0,32 bit), //CSR1
      WB.ALU1 -> inInst.alu,
      WB.MEM -> rspPayload,
      WB.PC4 -> inInst.pc.asBits.resized
    )

    val lastWrite = new Area{
      val valid = RegNext(False)
      val addr = RegNext(inInst.regFileAddress)
      val data = RegNext(writeData)
    }

    when(inInst.fire && inInst.ctrl.ren) {
      lastWrite.valid := True
      regFile(inInst.regFileAddress) := writeData
      when(!inInst.ctrl.bypassable){
        execute.stall := True
      }
    }
    inInst.ready := inInst.ctrl.wb =/= WB.MEM || inInst.ctrl.m =/= M.XRD || io.dRsp.valid

    //Reg file read after write bypass
    when(lastWrite.valid){
      when(lastWrite.addr === execute.addr0) {
        execute.bypassedSrc0 := lastWrite.data
      }
      when(lastWrite.addr === execute.addr1) {
        execute.bypassedSrc1 := lastWrite.data
      }
    }

    //ALU result bypass
    when(inInst.valid && inInst.ctrl.ren && inInst.ctrl.bypassable){ //TODO bypassable is maybe not usefull
      when(inInst.regFileAddress === execute.addr0) {
        execute.bypassedSrc0 := inInst.alu
      }
      when(inInst.regFileAddress === execute.addr1) {
        execute.bypassedSrc1 := inInst.alu
      }
    }
  }

}




object CoreMain{
  def main(args: Array[String]) {
    implicit val p = CoreParm(32,32,1024*64)
    SpinalVhdl(new Core(),_.setLibrary("riscv"))
  }
}