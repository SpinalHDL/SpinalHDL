package spinal.lib.cpu.riscv.impl.extension


import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._

class BarrelShifterLightExtension extends CoreExtension{
  override def needTag: Boolean = false
  override def getName: String = "barrelShifterLight"
  override def applyIt(core: RiscvCore): Area = new Area{
    import core._

    val s1 = new Area {
      val amplitude = execute0.inInst.alu_op1(4 downto 0).asUInt
      val isShift = execute0.inInst.ctrl.alu === ALU.SLL1 || execute0.inInst.ctrl.alu === ALU.SRL || execute0.inInst.ctrl.alu === ALU.SRA
      when(execute0.inInst.valid && isShift){
        execute0.outInst.result := execute0.inInst.alu_op0
        when(amplitude =/= 0){
          execute0.halt := True
          execute0.inInst.alu_op1.getDrivingReg(4 downto 0) := (execute0.inInst.alu_op1(4 downto 0).asUInt - 1).asBits
          switch(execute0.inInst.ctrl.alu){
            is(ALU.SLL1){
              execute0.inInst.alu_op0.getDrivingReg := (execute0.inInst.alu_op0 << 1).resized
            }
            is(ALU.SRL,ALU.SRA){
              execute0.inInst.alu_op0.getDrivingReg := (((execute0.inInst.ctrl.alu === ALU.SRA && execute0.inInst.alu_op0.msb) ## execute0.inInst.alu_op0).asSInt >> 1).asBits
            }
          }
        }
      }
    }
  }

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = { }
}

