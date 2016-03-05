package spinal.lib.cpu.riscv.impl

import spinal.core._
import Utils._
import spinal.lib.Reverse

class Alu extends Component{
  val io = new Bundle{
    val fn = in(ALU)
    val in2 = in UInt(32 bit)
    val in1 = in UInt(32 bit)
    val result = out UInt(32 bit)
    val adder_out = out UInt(32 bit)
  }

  val msb = 31

  // ADD, SUB
  val sum = io.in1 + Mux(io.fn === ALU.SUB1, (-io.in2.asSInt).asUInt, io.in2)

  // SLT, SLTU
  val less  = Mux(io.in1(msb) === io.in2(msb), sum(msb),
    Mux(io.fn === ALU.SLTU, io.in2(msb), io.in1(msb)))

  // SLL, SRL, SRA
  //  val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0)).toUInt
  //  val shin_hi_32 = Mux(isSub(io.fn), Fill(32, io.in1(31)), UInt(0,32))
  //  val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  //  val shin_hi = shin_hi_32
  //  val shin_r = Cat(shin_hi, io.in1(31,0))
  val shamt = io.in2(4 downto 0)
  val shin_r = io.in1(31 downto 0)
  val shin = Mux(io.fn === ALU.SRL1  || io.fn === ALU.SRA1, shin_r, Reverse(shin_r))
  val shout_r = (Cat(io.fn === ALU.SUB1 & shin(msb), shin).asSInt >> shamt)(msb,0)
  val shout_l = Reverse(shout_r)

  val bitwise_logic =
    Mux(io.fn === ALU.AND1, io.in1 & io.in2,
      Mux(io.fn === ALU.OR1,  io.in1 | io.in2,
        Mux(io.fn === ALU.XOR1, io.in1 ^ io.in2,
          io.in1))) // ALU_COPY1

  //  val out64 =
  val out_xpr_length =
    Mux(io.fn === ALU.ADD || io.fn === ALU.SUB1,  sum,
      Mux(io.fn === ALU.SLT || io.fn === ALU.SLTU, less.asUInt(32 bit),
        Mux(io.fn === ALU.SRL1 || io.fn === ALU.SRA1,  shout_r.asUInt,
          Mux(io.fn === ALU.SLL1,                       shout_l.asUInt,
            bitwise_logic))))

  //  val out_hi = Mux(io.dw === DW_64, out64(63,32), Fill(32, out64(31)))
  //  io.out := Cat(out_hi, out64(31,0)).toUInt
  io.result := out_xpr_length(31 downto 0)
  io.adder_out := sum
}


object AluMain{
  def main(args: Array[String]) {
    SpinalVhdl(new Alu().setDefinitionName("TopLevel"))
  }
}