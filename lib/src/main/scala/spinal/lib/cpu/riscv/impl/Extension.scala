package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils._

/**
 * Created by PIC32F_USER on 12/03/2016.
 */

case class DividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
  val numerator = UInt(nWidth bit)
  val denominator = UInt(dWidth bit)
}
case class DividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
  val quotient = UInt(nWidth bit)
  val remainder = UInt(dWidth bit)
  val error = Bool
}


class UnsignedDivider(nWidth : Int, dWidth : Int) extends Component{
  val io = new Bundle{
    val cmd = slave Stream(DividerCmd(nWidth,dWidth))
    val rsp = master Stream(DividerRsp(nWidth,dWidth))
  }
  val done = RegInit(True)
  val waitRsp = RegInit(False)
  val counter = Counter(nWidth)
  val numerator = Reg(UInt(nWidth bit))
  val remainder = Reg(UInt(dWidth bit))
  val remainderShifted = (remainder ## numerator.msb).asUInt
  val remainderMinusDenominator = remainderShifted - io.cmd.denominator

  io.cmd.ready := False
  io.rsp.valid := False
  io.rsp.quotient := numerator
  io.rsp.remainder := remainder
  io.rsp.error := False

  when(done){
    when(io.cmd.valid && (!waitRsp || io.rsp.ready)){
      counter.clear()
      remainder := 0
      numerator := io.cmd.numerator
      when(io.cmd.denominator === 0) {
        io.rsp.error := True
        io.rsp.valid := True
        io.cmd.ready := io.rsp.ready
      }otherwise{
        done := False
      }
    }
    when(io.rsp.ready){
      waitRsp := False
    }
  }.otherwise{
    counter.increment()
    remainder := remainderShifted.resized
    numerator := (numerator ## !remainderMinusDenominator.msb).asUInt.resized
    when(!remainderMinusDenominator.msb){
      remainder := remainderMinusDenominator.resized
    }
    when(counter.willOverflowIfInc){
      done := True
      waitRsp := True
      io.cmd.ready := True
    }
  }
}

class MulDivExtension extends CoreExtension{
  override def needTag: Boolean = true
  override def getName: String = "mulDiv"
  override def applyIt(core: Core): Area = new Area{
    import core._

    var sample : Bool = null
    def RegPip [T<:Data] (that : T) = spinal.core.RegNextWhen(that,sample)

    val s1 = new Area {
      val aSigned,bSigned = Bool
      val a,b = Bits(32 bit)

      a := execute0.inInst.alu_op0
      b := execute0.inInst.alu_op1
      switch(execute0.inInst.instruction(13 downto 12)) {
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

      val aULow = a(15 downto 0).asUInt
      val bULow = b(15 downto 0).asUInt
      val aSLow = (False ## a(15 downto 0)).asSInt
      val bSLow = (False ## b(15 downto 0)).asSInt
      val aHigh = (((aSigned && a.msb) ## a(31 downto 16))).asSInt
      val bHigh = (((bSigned && b.msb) ## b(31 downto 16))).asSInt
      val mul_ll = aULow * bULow
      val mul_lh = aSLow * bHigh
      val mul_hl = aHigh * bSLow
      val mul_hh = aHigh * bHigh
    }

    val s2 = new Area {
      sample = execute0.outInst.ready
      val mul_ll = RegPip(s1.mul_ll)
      val mul_lh = RegPip(s1.mul_lh)
      val mul_hl = RegPip(s1.mul_hl)
      val mul_hh = RegPip(s1.mul_hh)
      val low = S(0, mul_hl.getWidth + 2 bit) + (False ## mul_ll).asSInt + (mul_lh << 16) + (mul_hl << 16)
    }

    val s3 = new Area {
      sample = execute1.outInst.ready
      val low = RegPip(s2.low)
      val mul_hh = RegPip(s2.mul_hh)
      val result = low + (mul_hh << 32)
    }

    val s4 = new Area {
      val mulhReady = RegInit(False)
      sample = !mulhReady
      val result = RegPip(s3.result)
    }


    val divider = new UnsignedDivider(32,32)
    divider.io.cmd.valid := False  //TODO comment it to create bad report
    divider.io.cmd.numerator := execute0.inInst.alu_op0.asUInt
    divider.io.cmd.denominator := execute0.inInst.alu_op1.asUInt
    divider.io.rsp.ready := True
    when(isMyTag(execute0.inInst.ctrl)) {
      switch(execute0.inInst.instruction(14)) {
        is(True) {
          divider.io.cmd.valid := execute0.inInst.valid
          execute0.inInst.ready := divider.io.cmd.ready
          execute0.outInst.alu := divider.io.rsp.quotient.asBits
        }
      }
    }
    //pipeline insertion logic
    when(isMyTag(writeBack.inInst.ctrl)){
      switch(writeBack.inInst.instruction(14 downto 12)){
        is(B"000"){
          writeBack.inInst.alu := s3.low(31 downto 0).asBits
        }
        is(B"001",B"010",B"011"){
          writeBack.inInst.alu := s3.result(63 downto 32).asBits
          when(!s4.mulhReady){
            s4.mulhReady := writeBack.inInst.valid
          }otherwise{
            s4.mulhReady := !writeBack.inInst.fire
          }
        }
//        is(B"100"){
//          execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt / execute0.inInst.alu_op1.asSInt).asBits
//        }
//        is(B"101"){
//          execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt / execute0.inInst.alu_op1.asUInt).asBits
//        }
//        is(B"110"){
//          execute0.outInst.alu := (execute0.inInst.alu_op0.asSInt % execute0.inInst.alu_op1.asSInt).asBits
//        }
//        is(B"111"){
//          execute0.outInst.alu :=  (execute0.inInst.alu_op0.asUInt % execute0.inInst.alu_op1.asUInt).asBits
//        }
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
      ctrl.execute1AluBypass := False //instruction(14 downto 12) === B"000"
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
    val s1 = new Area {
      val amplitude = execute0.inInst.alu_op1(4 downto 0).asUInt
      val reversed = Mux(execute0.inInst.ctrl.alu === ALU.SLL1, Reverse(execute0.inInst.alu_op0), execute0.inInst.alu_op0)
      val shiftRight = (Cat(execute0.inInst.ctrl.alu === ALU.SRA1 & reversed.msb, reversed).asSInt >> amplitude)(31 downto 0).asBits
    }

    val s2 = new Area{
      sample = execute0.outInst.ready
      val shiftRight = RegPip(s1.shiftRight)
      switch(execute1.inInst.ctrl.alu){
        is(ALU.SLL1){
          execute1.outInst.alu :=Reverse(shiftRight)
        }
        is(ALU.SRL1,ALU.SRA1){
          execute1.outInst.alu := shiftRight
        }
      }
    }

  }

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = { }
}

