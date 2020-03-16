package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

// Created by PIC32F_USER on 05/04/2016.
// Modified / Extended by HWEngineer 15/03/2020

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstruction
//
class JtagTapInstruction(tap: JtagTapAccess, val instructionId: Int) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = tap.getInstruction === B(instructionId, tap.getInstruction.getWidth bits)

  Component.current.addPrePopTask(() => {
    when(instructionHit) {
      when(tap.getState === JtagState.DR_CAPTURE) {
        doCapture()
      }
      when(tap.getState === JtagState.DR_SHIFT) {
        doShift()
      }
      when(tap.getState === JtagState.DR_UPDATE) {
        doUpdate()
      }
    }
    when(tap.getState === JtagState.RESET) {
      doReset()
    }
  })
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstructionWrite
//
class JtagTapInstructionWrite[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)
                                     (tap: JtagTapAccess, instructionId: Int)
                                     extends JtagTapInstruction(tap, instructionId){

  val shifter,store = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := store
  }

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    if (readable) tap.setTdo(shifter.lsb)
  }

  override def doUpdate(): Unit = {
    store := shifter
  }

  if (!cleanUpdate)
    data.assignFromBits(shifter)
  else
    data.assignFromBits(store)
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstructionRead
//
class JtagTapInstructionRead[T <: Data](data: T)
                                    (tap: JtagTapAccess, instructionId: Int)
                                    extends JtagTapInstruction(tap, instructionId)
                                    with JtagTapShifter{

  override val shifter = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstructionWriteSimpleExampleB
//
class JtagTapInstructionWriteSimpleExampleB[T <: Data](data: T)
                                                   (tap: JtagTapAccess, instructionId: Int)
                                                   extends JtagTapInstruction(tap, instructionId){

  val shifter,store = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := store
  }
  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }
  override def doUpdate(): Unit = {
    store := shifter
  }

  data.assignFromBits(store)
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstructionIdcode
//
class JtagTapInstructionIdcode[T <: Data](value: Bits)
                                      (tap: JtagTapAccess, instructionId: Int)
                                      extends JtagTapInstruction(tap, instructionId){
  val shifter = Reg(Bits(32 bit))

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }

  override def doReset(): Unit = {
    shifter := value
    tap.setInstruction(B(instructionId, tap.getInstruction.getWidth bits))
  }
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapInstructionFlowFragmentPush
//
class JtagTapInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)
                                     (tap: JtagTapAccess, instructionId: Int)
                                     extends JtagTapInstruction(tap, instructionId){

  val source = Flow Fragment(Bits(1 bit))
  source.valid := False
  source.last := tap.getTms
  source.fragment.lsb := tap.getTdi

  sink << FlowCCByToggle(source, outputClock = sinkClockDomain)

  override def doShift(): Unit = {
    source.valid := True
  }
}
