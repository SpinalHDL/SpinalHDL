package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

// Created by PIC32F_USER on 05/04/2016.
// Modified / Extended by HWEngineer 15/03/2020

//══════════════════════════════════════════════════════════════════════════════
// define JtagInstruction
//
class JtagInstruction(tap: JtagTapAccess, val instructionId: Int) extends Area {
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
// define JtagInstructionWrite
//
class JtagInstructionWrite[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)
                                     (tap: JtagTapAccess, instructionId: Int)
                                     extends JtagInstruction(tap, instructionId){

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
// define JtagInstructionRead
//
class JtagInstructionRead[T <: Data](data: T)
                                    (tap: JtagTapAccess, instructionId: Int)
                                    extends JtagInstruction(tap, instructionId)
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
// define JtagInstructionWriteSimpleExampleB
//
class JtagInstructionWriteSimpleExampleB[T <: Data](data: T)
                                                   (tap: JtagTapAccess, instructionId: Int)
                                                   extends JtagInstruction(tap, instructionId){

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
// define JtagInstructionIdcode
//
class JtagInstructionIdcode[T <: Data](value: Bits)
                                      (tap: JtagTapAccess, instructionId: Int)
                                      extends JtagInstruction(tap, instructionId){
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
// define JtagInstructionFlowFragmentPush
//
class JtagInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)
                                     (tap: JtagTapAccess, instructionId: Int)
                                     extends JtagInstruction(tap, instructionId){

  val source = Flow Fragment(Bits(1 bit))
  source.valid := False
  source.last := tap.getTms
  source.fragment.lsb := tap.getTdi

  sink << FlowCCByToggle(source, outputClock = sinkClockDomain)

  override def doShift(): Unit = {
    source.valid := True
  }
}
