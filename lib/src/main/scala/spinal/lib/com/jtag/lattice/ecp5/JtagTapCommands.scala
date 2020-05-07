package spinal.lib.com.jtag.lattice.ecp5

import spinal.core._
import spinal.lib._

import spinal.lib.com.jtag.{JtagTapInstructionCtrl}

//══════════════════════════════════════════════════════════════════════════════
// JtagTapInstruction Write / Read / JtagTapInstructionFlowFragmentPush
//
// functionally identical with spinal.lib.com.jtag.JtagTapInstructionXXX classes
// but due to the fact that the ECP5 JTAGG has a buffer already implemented
// at the TDI signal, we can use the internal JTDI signal directly for combinatorial
// logic. 
// This way we mimic the same behaviour of the generic JtagTapInstructionXXX classes
// the code looks messy though...
//
// underlaying we implement a mealy-maschine to act like a moore-maschine.

//══════════════════════════════════════════════════════════════════════════════
// JtaggShifter
//
// An area where the MSB of the shiftreg value is either tdi or the last saved tdi state.
//
class JtaggShifter(dataWidth: Int, ctrl: JtagTapInstructionCtrl, readable: Boolean) extends ImplicitArea[Bits] {

    // in the original code, the internal shift register stores all serial data.
    // we use the JTDI signal directly as the MSB of this register.
    // because we're not shure if the JTDI signal changed in a transitional state,
    // we have to buffer the JTDI value when where not in the shifted state
    //
    // the readable feature:
    // With the readable flag set, we can read out the old value of the connected register
    // also we make sure, that the data in a jtag daisy chain is pushed through.
    // But the first TDO value is expected at the first pos clock edge in the shift state.
    // Due to the buffering of the JTDI this is a discrepancy of one clock cycle.
    // We can mitigate this by adding an additional shift state at the beginning of the transaction.
    // we need an additional flipflop to make shure the injected shift cycle is only run once per transaction.

    val shifter = Reg(Bits(dataWidth-1 bit))
    
    val lateshift = RegNext(ctrl.shift && ctrl.enable)
    val holdTdi = RegNextWhen(ctrl.tdi, lateshift)
    val shiftedOnce = readable generate Reg(Bool) init(False)

    val msb = lateshift ? ctrl.tdi | holdTdi
    val value = msb ## shifter

    when(ctrl.enable){
        if (readable) when(ctrl.update) {
            shiftedOnce := False
        }
    }

    def capture(store: Bits): Unit = {
        this.holdTdi := store.msb
        this.shifter := store.resized
    }

    def shift(): Unit = {
        if (readable) when(!this.shiftedOnce) {
            this.shifter := (holdTdi ## this.shifter) >> 1
            this.shiftedOnce :=True
        }

        if (readable) when(this.shiftedOnce) {
            this.shifter := (ctrl.tdi ## shifter) >> 1
        }

        if (!readable){
            this.shifter := (ctrl.tdi ## shifter) >> 1
        }
    }

    override def implicitValue: Bits = this.value
}


//══════════════════════════════════════════════════════════════════════════════
// JtagTapInstructionWrite
//
class JtagTapInstructionWrite[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true) extends Area {

  val ctrl = JtagTapInstructionCtrl()
  
  val shifter = new JtaggShifter(widthOf(data), ctrl, readable)                 
  val store = readable generate Reg(Bits(data.getBitsWidth bit))
  
  when(ctrl.enable) {
    if (readable) when(ctrl.capture) {
        shifter.capture(store)
    }

    when(ctrl.shift) {
      shifter.shift()
    }

    if (readable) when(ctrl.update) {
        store := shifter
    }
  }

  if (readable) ctrl.tdo := shifter.lsb else ctrl.tdo := False
  data.assignFromBits(if (!cleanUpdate) shifter else store)
}


//══════════════════════════════════════════════════════════════════════════════
// JtagTapInstructionRead
//
class JtagTapInstructionRead[T <: Data](data: T, light : Boolean) extends Area {    
  val ctrl = JtagTapInstructionCtrl()

  val full = !light generate new Area {
    val shifter = new JtaggShifter(widthOf(data), ctrl, true) // in read, always readable 
    when(ctrl.enable) {
        when(ctrl.capture) {
            shifter.capture(B(data))
        }
        when(ctrl.shift){
            shifter.shift()
        }
    }
    ctrl.tdo := shifter.lsb
  }

  val reduced = light generate new Area{  
    val counter = Reg(UInt(log2Up(widthOf(data)) bits))
    when(ctrl.enable) {
      when(ctrl.capture){
        counter := 0
      }
      when(ctrl.shift) {
        counter := counter + 1
      }
    }
    ctrl.tdo := B(data)(counter)
  }
}

//══════════════════════════════════════════════════════════════════════════════
// JtagTapInstructionFlowFragmentPush
//
class JtagTapInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val source = Flow Fragment(Bits(1 bit))
  val valid = RegNext(ctrl.enable && ctrl.shift) 
  val data = ctrl.tdi

  source.valid := valid
  source.last := !(ctrl.enable && ctrl.shift)
  source.fragment.lsb := data

  sink << FlowCCByToggle(source, outputClock = sinkClockDomain)

  ctrl.tdo := False
}