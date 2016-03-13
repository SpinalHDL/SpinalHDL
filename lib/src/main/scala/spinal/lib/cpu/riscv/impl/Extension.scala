package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib.Reverse
import spinal.lib.cpu.riscv.impl.Utils._

/**
 * Created by PIC32F_USER on 12/03/2016.
 */
class MulDivExtension extends CoreExtension{
  override def needTag: Boolean = true
  override def getName: String = "mulDiv"
  override def applyIt(core: Core): Area = new Area{
    import core._
    val aSigned,bSigned = Bool
    val a,b = Bits(32 bit)

    a := decode.outInst.alu_op0
    b := decode.outInst.alu_op1
    switch(decode.outInst.instruction(13 downto 12)) {
      is(B"01") {
        aSigned := True
        bSigned := True
      }
      is(B"10") {
        aSigned := True
        bSigned := False
      }
      default {
        aSigned := False
        bSigned := False
      }
    }

//    val ref0 = (execute0.inInst.alu_op0.asUInt * execute0.inInst.alu_op1.asUInt)(31 downto 0).asBits.resized.keep()
//    val ref1 =  ((execute0.inInst.alu_op0.asSInt * execute0.inInst.alu_op1.asSInt).asBits >> 32)(31 downto 0).resized.keep()
//    val ref2 =  ((execute0.inInst.alu_op0.asSInt * (False ## execute0.inInst.alu_op1).asSInt).asBits >> 32)(31 downto 0).resized.keep()
//    val ref3 = ((execute0.inInst.alu_op0.asUInt * execute0.inInst.alu_op1.asUInt).asBits >> 32)(31 downto 0).keep()

    var sample : Bool = null
    def RegPip [T<:Data] (that : T) = spinal.core.RegNextWhen(that,sample)

    // sample inputs
    sample = decode.outInst.ready
    val aULow = RegPip(a)(15 downto 0).asUInt
    val bULow = RegPip(b)(15 downto 0).asUInt
    val aLow = RegPip(False ## a(15 downto 0)).asSInt
    val bLow = RegPip(False ## b(15 downto 0)).asSInt
    val aHigh = RegPip(((aSigned && a.msb) ## a(31 downto 16))).asSInt
    val bHigh = RegPip(((bSigned && b.msb) ## b(31 downto 16))).asSInt

    //first stage
    sample = execute0.outInst.ready
    val mul_ll = RegPip(aULow*bULow)
    val mul_lh = aLow * bHigh
    val mul_hl = aHigh * bLow
    val mul_hh = RegPip(aHigh*bHigh)
    val mul_mm = RegPip(mul_lh + mul_hl)

    //second stage
    val resultLow = (False ## mul_ll).asSInt + ((mul_mm.msb ## mul_mm).asSInt << 16)
    val outLow = resultLow(31 downto 0).asBits

    //third stage
    val mulhReady = RegInit(False)
    sample = ! mulhReady
    val resultHigh = RegPip(resultLow) + RegPip((mul_hh << 32))
    val outHigh = resultHigh(63 downto 32).asBits


    //pipeline insertion logic
    when(isMyTag(execute1.inInst.ctrl)){
      switch(execute1.inInst.instruction(14 downto 12)){
        is(B"000"){
          execute1.outInst.alu := outLow
        }
        is(B"001",B"010",B"011"){
          execute1.outInst.alu := outHigh
          execute1.inInst.ready := mulhReady
          when(!mulhReady){
            mulhReady := execute1.inInst.valid
          }otherwise{
            mulhReady := !execute1.inInst.fire
          }
        }
        is(B"100"){
       //   execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt / execute0.inInst.alu_op1.asSInt).asBits
        }
        is(B"101"){
       //   execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt / execute0.inInst.alu_op1.asUInt).asBits
        }
        is(B"110"){
       //   execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt % execute0.inInst.alu_op1.asSInt).asBits
        }
        is(B"111"){
       //   execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt % execute0.inInst.alu_op1.asUInt).asBits
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
      ctrl.execute1AluBypass := instruction(14 downto 12) === B"000"
    }
  }
}

class BarrelShifterFullExtension extends CoreExtension{
  override def needTag: Boolean = false
  override def getName: String = "barrelShifterFull"
  override def applyIt(core: Core): Area = new Area{
    import core._

    var sample : Bool = null
    def RegPip [T<:Data] (that : T) = spinal.core.RegNextWhen(that,sample)

    //first stage
    sample = execute0.outInst.ready
    val amplitude = execute0.inInst.alu_op1(4 downto 0).asUInt
    val reversed = Mux(execute0.inInst.ctrl.alu === ALU.SLL1 , Reverse(execute0.inInst.alu_op0), execute0.inInst.alu_op0)
    val shiftRight = RegPip((Cat(execute0.inInst.ctrl.alu === ALU.SRA1 & reversed.msb, reversed).asSInt >> amplitude)(31 downto 0).asBits)

    //seconde stage + result insertion
    switch(execute1.inInst.ctrl.alu){
      is(ALU.SLL1){
        execute1.outInst.alu :=Reverse(shiftRight)
      }
      is(ALU.SRL1,ALU.SRA1){
        execute1.outInst.alu := shiftRight
      }
    }
  }

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = { }
}

