package spinal.lib.cpu.riscv.impl.extension

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib.math.MixedDivider

class DivExtension extends CoreExtension{
  override def needTag: Boolean = true
  override def getName: String = "DivExtension"
  override def applyIt(core: RiscvCore): Area = new Area{
    import core._

    val divider = new MixedDivider(32,32,true)
    divider.io.flush := execute1.throwIt
    divider.io.cmd.valid := False
    divider.io.cmd.numerator := execute0.inInst.alu_op0
    divider.io.cmd.denominator := execute0.inInst.alu_op1
    divider.io.cmd.signed := !execute0.inInst.instruction(12)

    val rspReady = RegInit(False)
    val rsp = RegNext(Mux(execute1.inInst.instruction(13), divider.io.rsp.remainder, divider.io.rsp.quotient).asBits)
    divider.io.rsp.ready := False

    when(execute0.inInst.valid && isMyTag(execute0.inInst.ctrl)) {
      divider.io.cmd.valid := execute0.outInst.valid
      when(!divider.io.cmd.ready) {
        execute0.halt := True
      }
    }

    when(execute1.inInst.valid && isMyTag(execute1.inInst.ctrl)) {
      divider.io.rsp.ready := execute1.inInst.ready && rspReady
      rspReady := divider.io.rsp.valid && !execute1.inInst.ready
      when(!rspReady){
        execute1.halt := True
      }

      execute1.outInst.result := rsp
    }

    when(execute1.throwIt){
      rspReady := False
    }

    divider.io.rsp.payload.error.allowPruning
  }

  def DIVX       = M"0000001----------1-------0110011"
  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === DIVX){
      applyTag(ctrl)
      ctrl.instVal := True
      ctrl.op0 := OP0.RS
      ctrl.op1 := OP1.RS
      ctrl.wb  := WB.ALU
      ctrl.rfen := True
      ctrl.execute0AluBypass := False
      ctrl.execute1AluBypass := True
      ctrl.useSrc0 := True
      ctrl.useSrc1 := True
    }
  }
}
