package spinal.lib.cpu.riscv.impl.extension

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._

class BarrelShifterFullExtension extends CoreExtension{
  override def needTag: Boolean = false
  override def getName: String = "BarrelShifterFullExtension"
  override def applyIt(core: RiscvCore): Area = new Area{
    import core._

    var sample : Bool = null
    def RegPip [T<:Data] (that : T) = spinal.core.RegNextWhen(that,sample)

    //first stage
    val s1 = new Area {
      val amplitude = execute0.inInst.alu_op1(4 downto 0).asUInt
      val reversed = Mux(execute0.inInst.ctrl.alu === ALU.SLL1, Reverse(execute0.inInst.alu_op0), execute0.inInst.alu_op0)
      val shiftRight = (Cat(execute0.inInst.ctrl.alu === ALU.SRA & reversed.msb, reversed).asSInt >> amplitude)(31 downto 0).asBits
    }

    val s2 = new Area{
      sample = execute0.outInst.ready
      val shiftRight = RegPip(s1.shiftRight)
      switch(execute1.inInst.ctrl.alu){
        is(ALU.SLL1){
          execute1.outInst.result :=Reverse(shiftRight)
        }
        is(ALU.SRL,ALU.SRA){
          execute1.outInst.result := shiftRight
        }
      }
    }
    sample = null
  }

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = { }
}

