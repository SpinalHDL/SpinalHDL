package spinal.lib.cpu.riscv.impl

import spinal.core._
import Utils._
import spinal.lib.Reverse

class Alu extends Component{
  val io = new Bundle{
    val func = in(ALU)
    val doSub = in Bool
    val src0 = in Bits(32 bit)
    val src1 = in Bits(32 bit)
    val result = out Bits(32 bit)
    val adder = out UInt(32 bit)
    val shift = out Bits(32 bit)
  }

  // ADD, SUB
  val addSub = (io.src0.asSInt + Mux(io.doSub, ~io.src1, io.src1).asSInt + Mux(io.doSub,S(1),S(0))).asBits

  // AND, OR, XOR, COPY0
  val bitwise = io.func.map(
    ALU.AND1 -> (io.src0 & io.src1),
    ALU.OR1 ->  (io.src0 | io.src1),
    ALU.XOR1 -> (io.src0 ^ io.src1),
    default -> io.src0
  )
  // SLT, SLTU
  val less  = Mux(io.src0.msb === io.src1.msb, addSub.msb,
    Mux(io.func === ALU.SLTU, io.src1.msb, io.src0.msb))

  // SLL, SRL, SRA
  val shifter = new Area{
    val amplitude = io.src1(4 downto 0).asUInt
    val reversed = Mux(io.func === ALU.SLL1 , Reverse(io.src0), io.src0)
    val shiftRight = (Cat(io.func === ALU.SRA1 & reversed.msb, reversed).asSInt >> amplitude)(31 downto 0).asBits
  }

  // mux results
  io.result := io.func.map(
    (ALU.SLT,ALU.SLTU) -> less.asBits(32 bit),
    (ALU.ADD,ALU.SUB1) -> addSub,
    default  -> bitwise
  )

  io.adder := addSub.asUInt
  io.shift := shifter.shiftRight
}





object AluMain{
  def main(args: Array[String]) {
    SpinalVhdl(new Alu().setDefinitionName("TopLevel"))
  }
}



