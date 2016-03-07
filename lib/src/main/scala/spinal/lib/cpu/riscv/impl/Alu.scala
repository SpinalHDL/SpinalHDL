package spinal.lib.cpu.riscv.impl

import spinal.core._
import Utils._
import spinal.lib.Reverse

class Alu extends Component{
  val io = new Bundle{
    val func = in(ALU)
    val src0 = in Bits(32 bit)
    val src1 = in Bits(32 bit)
    val result = out Bits(32 bit)
    val adder = out UInt(32 bit)
  }

  val msb = 31

  // ADD, SUB
  val addSub = (io.src1.asSInt + Mux(io.func === ALU.ADD, io.src0.asSInt, (-io.src0.asSInt))).asBits

  // AND, OR, XOR, COPY1
  val bitwise = io.func.map(
    ALU.AND1 -> (io.src1 & io.src0),
    ALU.OR1 ->  (io.src1 | io.src0),
    ALU.XOR1 -> (io.src1 ^ io.src0),
    default -> io.src1
  )
  // SLT, SLTU
  val less  = Mux(io.src1(msb) === io.src0(msb), addSub(msb),
    Mux(io.func === ALU.SLTU, io.src0(msb), io.src1(msb)))

  // SLL, SRL, SRA
  val shifter = new Area{
    val amplitude = io.src0(4 downto 0).asUInt
    val reversed = Mux(io.func === ALU.SLL1 , Reverse(io.src1), io.src1)
    val shiftRight = (Cat(io.func === ALU.SRA1 & reversed(msb), reversed).asSInt >> amplitude)(msb downto 0).asBits
    val shiftLeft = Reverse(shiftRight).asBits
  }

  // mux results
  io.result := io.func.map(
    (ALU.SLL1) -> shifter.shiftLeft,
    (ALU.SRL1,ALU.SRA1) -> shifter.shiftRight,
    (ALU.SLT,ALU.SLTU) -> less.asBits(32 bit),
    (ALU.ADD,ALU.SUB1) -> addSub,
    default  -> bitwise
  )

  io.adder := addSub.asUInt
}

object AluMain{
  def main(args: Array[String]) {
    SpinalVhdl(new Alu().setDefinitionName("TopLevel"))
  }
}


//Simple model =>
//    alu_shamt     = io.in2(4,0).asUInt
//    adder_out = (io.in1.asUInt + io.in2.asUInt)(31,0)
//    
//    ALU.ADD   -> exe_adder_out.asBits,
//    ALU.SUB1  -> (io.in1.asUInt - io.in2.asUInt).asBits,
//    ALU.AND1  -> (io.in1 & io.in2),
//    ALU.OR1   -> (io.in1 | io.in2),
//    ALU.XOR1  -> (io.in1 ^ io.in2),
//    ALU.SLT  -> (io.in1.asSInt < io.in2.asSInt).asBits.resized,
//    ALU.SLTU  -> (io.in1.asUInt < io.in2.asUInt).asBits.resized,
//    ALU.SLL1  -> ((io.in1 << alu_shamt)(31, 0)),
//    ALU.SRA1  -> (io.in1.asSInt >> alu_shamt).asBits,
//    ALU.SRL1  -> (io.in1 >> alu_shamt),
//    ALU.COPY1 -> io.in1




