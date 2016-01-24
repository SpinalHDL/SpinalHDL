package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._


case class CoreParm(val pcWidth : Int = 30,val addrWidth : Int = 30)

case class CoreDataCmd(implicit p : CoreParm) extends Bundle{
  val address = Bits(p.addrWidth bit)
  val mask = Bits(4 bit)
}

// assert(latencyAnalysis(io.iCmd.data,io.iRsp.data) == 1)
class Core(implicit p : CoreParm) extends Component{
  import p._
  val io = new Bundle{
    val iCmd = master Stream(Bits(pcWidth bit))
    val iRsp = slave Stream(Bits(32 bit))

    val dCmd = master Stream(CoreDataCmd())
    val dRsp = slave Stream(Bits(32 bit))
  }

  val regFile = Mem(Bits(32 bit),32)


  val fetch = new Area {
    val pc = Reg(UInt(pcWidth bit))
    val pcNext = pc + 1
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.data
    }

    io.iCmd.valid := True
    io.iCmd.data := pcNext.toBits
    when(io.iCmd.fire){
      pc := pcNext
    }
  }

  val decode = new Area{
    val outInst = Stream(new Bundle{
      val pc = UInt(pcWidth bit)
      val instruction = Bits(32 bit)
    })
    outInst.valid := io.dRsp.valid
    outInst.pc := fetch.pc
    outInst.instruction := io.iRsp.data
    val reg0 = regFile.readSync(outInst.instruction(19 downto 15).toUInt,outInst.fire)
    val reg1 = regFile.readSync(outInst.instruction(24 downto 20).toUInt,outInst.fire)

  }


  val execute = new Area{
    val inInst = decode.outInst.m2sPipe()

    val imm_i = inInst.instruction(31, 20)
    val imm_s = Cat(inInst.instruction(31, 25), inInst.instruction(11,7))
    val imm_b = Cat(inInst.instruction(31), inInst.instruction(7), inInst.instruction(30,25), inInst.instruction(11,8))
    val imm_u = Cat(inInst.instruction(31, 12), B"h000")
    val imm_j = Cat(inInst.instruction(31), inInst.instruction(19,12), inInst.instruction(20), inInst.instruction(30,21))
    val imm_z = inInst.instruction(19,15)

//    val imm_i_sext = Cat(Fill(imm_i(11), 20), imm_i)
//    val imm_s_sext = Cat(Fill(imm_s(11), 20), imm_s)
//    val imm_b_sext = Cat(Fill(imm_b(11), 19), imm_b, B"0")
//    val imm_j_sext = Cat(Fill(imm_j(19), 11), imm_j, B"0")

    val bypassedSrc0 = Bits(32 bit)
    val bypassedSrc1 = Bits(32 bit)



    val alu = new Area{
      val result = Bits(32 bit)
    }

    fetch.pcLoad.valid := False
    fetch.pcLoad.data.assignDontCare()

    io.dCmd.valid := False
    io.dCmd.address := alu.result
    io.dCmd.mask := ???

    val outInst = Stream(new Bundle{
      val pc = UInt(pcWidth bit)
      val alu = Bits(32 bit)
      val regFileAddress = UInt(5 bit)
    })
    when(inInst.fire){
      outInst.pc := fetch.pc
      outInst.alu := alu.result
    }
  }

  val writeBack = new Area{
    val inInst = execute.outInst.m2sPipe()

    val value = Bits(32 bit)
    value := inInst.alu
    regFile(inInst.regFileAddress) := value
  }

}
