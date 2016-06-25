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
  }

  // ADD, SUB
  val addSub = (io.src0.asSInt + Mux(io.doSub, ~io.src1, io.src1).asSInt + Mux(io.doSub,S(1),S(0))).asBits

  // AND, OR, XOR, COPY0
  val bitwise = io.func.mux(
    ALU.AND -> (io.src0 & io.src1),
    ALU.OR ->  (io.src0 | io.src1),
    ALU.XOR -> (io.src0 ^ io.src1),
    default -> io.src0
  )

  // SLT, SLTU
  val less  = Mux(io.src0.msb === io.src1.msb, addSub.msb,
    Mux(io.func === ALU.SLTU, io.src1.msb, io.src0.msb))


  // mux results
  io.result := io.func.mux(
    (ALU.SLT,ALU.SLTU) -> less.asBits(32 bit),
    (ALU.ADD,ALU.SUB) -> addSub,
    default  -> bitwise
  )

  io.adder := addSub.asUInt.resized
}





object AluMain{
  def main(args: Array[String]) {
    SpinalVhdl(new Alu().setDefinitionName("TopLevel"))
  }
}



