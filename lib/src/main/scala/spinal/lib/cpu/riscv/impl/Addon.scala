package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib.cpu.riscv.impl.Utils._

/**
 * Created by PIC32F_USER on 12/03/2016.
 */
class MulDivExtension extends CoreExtension{
  override def needTag: Boolean = true
  override def applyIt(core: Core): Area = new Area{
    import core._


    def RegNext [T<:Data] (that : T) = that
    val aSigned,bSigned = False
    val a,b = Bits(32 bit)


    a := execute0.inInst.alu_op0
    b := execute0.inInst.alu_op1


    val outLow = Bits(32 bit)
    val outHigh = Bits(32 bit)

    val aULow = RegNext(a)(15 downto 0).asUInt
    val bULow = RegNext(b)(15 downto 0).asUInt
    val aLow = RegNext(False ## a)(15 downto 0).asSInt
    val bLow = RegNext(False ## b)(15 downto 0).asSInt
    val aHigh = RegNext(((aSigned && a.msb) ## a(31 downto 16))).asSInt
    val bHigh = RegNext(((bSigned && b.msb) ## b(31 downto 16))).asSInt

    val mul_ll = RegNext(aULow*bULow)
    val mul_lh = aLow * bHigh
    val mul_hl = aHigh * bLow
    val mul_hh = RegNext(aHigh*bHigh)
    val mul_mm = RegNext(mul_lh + mul_hl)

    val resultLow = mul_ll.asSInt + ((False ## mul_mm).asSInt << 16)
    val resultHigh = RegNext(resultLow) + RegNext((mul_hh << 32))

    outLow := RegNext(resultLow(31 downto 0).asBits)
    outHigh := RegNext(resultHigh(63 downto 32).asBits)

    when(isMyTag(execute0.ctrl)){
      switch(execute0.inInst.instruction(14 downto 12)){
        is(B"000"){
          execute0.outInst.alu := outLow
          //execute0.outInst.alu := (execute0.inInst.alu_op0.asUInt * execute0.inInst.alu_op1.asUInt).asBits.resized
        }
        is(B"001"){
          aSigned := True
          bSigned := True
          execute0.outInst.alu := outHigh
          //execute0.outInst.alu := ((execute0.inInst.alu_op0.asSInt * execute0.inInst.alu_op1.asSInt).asBits >> 32).resized
        }
        is(B"010"){
          aSigned := True
          execute0.outInst.alu := outHigh
          //execute0.outInst.alu := ((execute0.inInst.alu_op0.asSInt * (False ## execute0.inInst.alu_op1).asSInt).asBits >> 32).resized
        }
        is(B"011"){
          execute0.outInst.alu := outHigh
          //execute0.outInst.alu := ((execute0.inInst.alu_op0.asUInt * execute0.inInst.alu_op1.asUInt).asBits >> 32).resized
        }
        is(B"100"){
          execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt / execute0.inInst.alu_op1.asSInt).asBits
        }
        is(B"101"){
          execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt / execute0.inInst.alu_op1.asUInt).asBits
        }
        is(B"110"){
          execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt % execute0.inInst.alu_op1.asSInt).asBits
        }
        is(B"111"){
          execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt % execute0.inInst.alu_op1.asUInt).asBits
        }
      }
    }
  }
  def MULX       = M"0000001------------------0110011"
  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === MULX){
      applyTag(ctrl)
      ctrl.instVal := True
      ctrl.op1 := OP1.RS1
      ctrl.op2 := OP2.RS2
      ctrl.wb  := WB.ALU1
      ctrl.rfen := True
      ctrl.execute0AluBypass := False
      ctrl.execute1AluBypass := True
    }
  }
}
