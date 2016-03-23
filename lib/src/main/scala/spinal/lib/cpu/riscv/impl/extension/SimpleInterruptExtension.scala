package spinal.lib.cpu.riscv.impl.extension


import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl._
import spinal.lib.cpu.riscv.impl.Utils._

class SimpleInterruptExtension(exceptionVector : Int) extends CoreExtension{
  val interruptUsage = scala.collection.mutable.HashMap[Int,(Bool,String,IrqUsage)]()

  def addIrq(id : Int,pin : Bool,irqUsage: IrqUsage,name : String): this.type = {
    interruptUsage(id) = Tuple3(pin,name, irqUsage)
    this
  }

  override def applyIt(core: Core): Area = new Area {

    import core._
    for((id,(pin,name,exeption)) <- interruptUsage){
      access.irq.sources(id) := pin.pull().setName(name)
    }

    val inIrq = RegInit(False)
    val exceptionPc = Reg(UInt(32 bit))
    val irqValue = B(0,irqWidth bit)
    for((id,usage) <- irqUsages){
      if(usage.isException)
        irqValue(id) := RegNextWhen(access.irq.masked(id),!inIrq)
      else
        irqValue(id) := access.irq.masked(id)
    }
    when(!inIrq) {
      when((access.irq.masked & irqExceptionMask) =/= 0) {
        access.throwIt := True
        access.flushMemoryResponse := True
        access.pcLoad.valid := True
        access.pcLoad.payload := exceptionVector
        exceptionPc := access.inInst.pc
        inIrq := True
      }.elsewhen((access.irq.masked & ~B(irqExceptionMask,irqWidth bit)) =/= 0){
        decode.halt := True
        when(decode.inInst.valid &&  !execute0.inInst.valid && !execute1.inInst.valid && !access.inInst.valid) {
          access.pcLoad.valid := True
          access.pcLoad.payload := exceptionVector
          exceptionPc := decode.inInst.pc
          inIrq := True
        }
      }
    }
    when(execute1.inInst.valid) {
      when(isMyTag(execute1.inInst.ctrl)) {
        switch(execute1.inInst.instruction(26 downto 25)){
          is(B"00"){ // return from interrupt
            execute1.pc_sel := PC.J
            execute1.inInst.adder := exceptionPc
            when(execute1.outInst.fire) {
              inIrq := False
            }
          }
          is(B"01"){ //read irq value
            execute1.outInst.result := irqValue.resized
          }
          is(B"10"){ //read irq mask
            access.irq.mask := execute1.inInst.result.resized
          }
          is(B"11"){ //write irq mask
            execute1.outInst.result := access.irq.mask.resized
          }
        }
      }
    }
  }

  override def needTag: Boolean = true

  override def getName: String = "SimpleInterrupExtension"

  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === M"00000--------------------0001011"){
      applyTag(ctrl)
      ctrl.instVal := True
      ctrl.wb := WB.ALU1
      ctrl.alu := ALU.COPY1
      when(instruction(25)){
        ctrl.rfen := True
      }
    }
  }

  override def getIrqUsage: Seq[(Int, IrqUsage)] = interruptUsage.map(e => (e._1 -> e._2._3)).toSeq
}

