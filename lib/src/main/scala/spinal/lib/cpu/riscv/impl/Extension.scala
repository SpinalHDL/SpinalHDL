package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl.Utils._

/**
 * Created by PIC32F_USER on 12/03/2016.
 */

case class SignedDividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
  val numerator = SInt(nWidth bit)
  val denominator = SInt(dWidth bit)
}
case class SignedDividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
  val quotient = SInt(nWidth bit)
  val remainder = SInt(dWidth bit)
  val error = Bool
}
class SignedDivider(nWidth : Int, dWidth : Int,storeDenominator : Boolean) extends Component{
  val io = new Bundle{
    val cmd = slave Stream(SignedDividerCmd(nWidth,dWidth))
    val rsp = master Stream(SignedDividerRsp(nWidth,dWidth))
  }
  val divider = new UnsignedDivider(nWidth,dWidth,storeDenominator,Bits(2 bit))
  divider.io.cmd.arbitrationFrom(io.cmd)
  divider.io.cmd.numerator := io.cmd.numerator.abs
  divider.io.cmd.denominator := io.cmd.denominator.abs
  divider.io.cmd.context(0) := (io.cmd.numerator.msb ^ io.cmd.denominator.msb)
  divider.io.cmd.context(1) := io.cmd.numerator.msb

  io.rsp.arbitrationFrom(divider.io.rsp)
  io.rsp.quotient := divider.io.rsp.quotient.twoComplement(divider.io.rsp.context(0))
  io.rsp.remainder := divider.io.rsp.remainder.twoComplement(divider.io.rsp.context(1))
}


case class MixedDividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
  val numerator = Bits(nWidth bit)
  val denominator = Bits(dWidth bit)
  val signed = Bool
}
case class MixedDividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
  val quotient = Bits(nWidth bit)
  val remainder = Bits(dWidth bit)
  val error = Bool
}
class MixedDivider(nWidth : Int, dWidth : Int,storeDenominator : Boolean) extends Component{
  val io = new Bundle{
    val cmd = slave Stream(MixedDividerCmd(nWidth,dWidth))
    val rsp = master Stream(MixedDividerRsp(nWidth,dWidth))
  }
  val divider = new UnsignedDivider(nWidth,dWidth,storeDenominator,Bits(2 bit))
  divider.io.cmd.arbitrationFrom(io.cmd)
  divider.io.cmd.numerator := io.cmd.numerator.asSInt.abs(io.cmd.signed)
  divider.io.cmd.denominator := io.cmd.denominator.asSInt.abs(io.cmd.signed)
  divider.io.cmd.context(0) := io.cmd.signed && (io.cmd.numerator.msb ^ io.cmd.denominator.msb)
  divider.io.cmd.context(1) := io.cmd.signed && io.cmd.numerator.msb

  io.rsp.arbitrationFrom(divider.io.rsp)
  io.rsp.quotient := divider.io.rsp.quotient.twoComplement(divider.io.rsp.context(0)).asBits.resized
  io.rsp.remainder := divider.io.rsp.remainder.twoComplement(divider.io.rsp.context(1)).asBits.resized
}



case class UnsignedDividerCmd[T <: Data](nWidth : Int, dWidth : Int,contextType : T) extends Bundle{
  val numerator = UInt(nWidth bit)
  val denominator = UInt(dWidth bit)
  val context = contextType.clone
}
case class UnsignedDividerRsp[T <: Data](nWidth : Int, dWidth : Int,contextType : T)extends Bundle{
  val quotient = UInt(nWidth bit)
  val remainder = UInt(dWidth bit)
  val error = Bool
  val context = contextType.clone
}


class UnsignedDivider[T <: Data](nWidth : Int, dWidth : Int,storeDenominator : Boolean,contextType : T = NoData) extends Component{
  val io = new Bundle{
    val cmd = slave Stream(UnsignedDividerCmd(nWidth,dWidth,contextType))
    val rsp = master Stream(UnsignedDividerRsp(nWidth,dWidth,contextType))
  }
  val done = RegInit(True)
  val waitRsp = RegInit(False)
  val counter = Counter(nWidth)
  val numerator = Reg(UInt(nWidth bit))
  val denominator = if(storeDenominator) Reg(UInt(dWidth bit)) else io.cmd.denominator
  val context = if(storeDenominator) Reg(contextType) else io.cmd.context
  val remainder = Reg(UInt(dWidth bit))
  val remainderShifted = (remainder ## numerator.msb).asUInt
  val remainderMinusDenominator = remainderShifted - denominator

  io.cmd.ready := False
  io.rsp.valid := waitRsp
  io.rsp.quotient := numerator
  io.rsp.remainder := remainder
  io.rsp.context := context

  when(io.rsp.ready){
    waitRsp := False
  }
  io.rsp.error := denominator === 0
  when(done){
    when(!waitRsp || io.rsp.ready){ //ready for new command
      counter.clear()
      remainder := 0
      numerator := io.cmd.numerator
      if(storeDenominator){
        denominator := io.cmd.denominator
        context := io.cmd.context
      }

      done := !io.cmd.valid
      if(storeDenominator) {
        io.cmd.ready := True
      }
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
      if(storeDenominator) {
        io.cmd.ready := True
      }
    }
  }

}

class MulExtension extends CoreExtension{
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

    //pipeline insertion logic
    when(isMyTag(access.inInst.ctrl)){
      switch(access.inInst.instruction(13 downto 12)){
        is(B"00"){
          access.inInst.alu := s3.low(31 downto 0).asBits
        }
        is(B"01",B"10",B"11"){
          access.inInst.alu := s3.result(63 downto 32).asBits
        }
      }
    }
    sample = null
  }

  def MULX       = M"0000001----------0-------0110011"
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



class DivExtension extends CoreExtension{
  override def needTag: Boolean = true
  override def getName: String = "mulDiv"
  override def applyIt(core: Core): Area = new Area{
    import core._

    val divider = new MixedDivider(32,32,true)
    divider.io.cmd.valid := False  //TODO comment it to create bad report
    divider.io.cmd.numerator := execute0.inInst.alu_op0
    divider.io.cmd.denominator := execute0.inInst.alu_op1
    divider.io.cmd.signed := !execute0.inInst.instruction(12)

    val rspReady = RegInit(False)
    val rsp = RegNext(Mux(execute1.inInst.instruction(13), divider.io.rsp.remainder, divider.io.rsp.quotient).asBits)
    divider.io.rsp.ready := False

    when(isMyTag(execute0.inInst.ctrl)) {
      divider.io.cmd.valid := execute0.outInst.valid
      when(!divider.io.cmd.ready) {
        execute0.halt := True
      }
    }

    when(isMyTag(execute1.inInst.ctrl)) {
      divider.io.rsp.ready := execute1.inInst.ready && rspReady
      rspReady := divider.io.rsp.valid && !execute1.inInst.ready
      when(!rspReady){
        execute1.halt := True
      }

      execute1.outInst.alu := rsp
    }
  }

  def DIVX       = M"0000001----------1-------0110011"
  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === DIVX){
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
    sample = null
  }

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = { }
}




//    val s4 = new Area {
//      val mulhReady = RegInit(False)
//      sample = !mulhReady
//      val result = RegPip(s3.result)
//    }
//          writeBack.inInst.ready := s4.mulhReady
//          when(!s4.mulhReady){
//            s4.mulhReady := writeBack.inInst.valid
//          }otherwise{
//            s4.mulhReady := !writeBack.inInst.fire
//          }