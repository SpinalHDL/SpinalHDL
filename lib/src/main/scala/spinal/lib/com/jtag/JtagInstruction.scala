package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

/**
 * Created by PIC32F_USER on 09/04/2016.
 */

trait JtagTapAccess {
  def getTdi : Bool
  def getTms : Bool
  def setTdo(value : Bool) : Unit

  def getState : JtagState.C
  def getInstruction() : Bits
  def setInstruction(value : Bits) : Unit

  //Instruction wrappers
  def idcode(value: Bits)(instructionId: Bits) =
    new JtagInstructionIdcode(value)(this,instructionId)

  def read[T <: Data](data: T)(instructionId: Bits)   =
    new JtagInstructionRead(data)(this,instructionId)

  def write[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Bits) =
    new JtagInstructionWrite[T](data,cleanUpdate,readable)(this,instructionId)

  def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]],sinkClockDomain : ClockDomain)(instructionId: Bits) =
    new JtagInstructionFlowFragmentPush(sink,sinkClockDomain)(this,instructionId)
}

class JtagInstruction(tap: JtagTapAccess,val instructionId: Bits) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = tap.getInstruction === instructionId.resized

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

class JtagInstructionWrite[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true) (tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId) {
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

class JtagInstructionRead[T <: Data](data: T) (tap: JtagTapAccess,instructionId: Bits)extends JtagInstruction(tap,instructionId) {
  val shifter = Reg(Bits(data.getBitsWidth bit))

  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }
}

class JtagInstructionWriteSimpleExample[T <: Data](data: T) (tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId) {
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


class JtagInstructionIdcode[T <: Data](value: Bits)(tap: JtagTapAccess, instructionId: Bits)extends JtagInstruction(tap,instructionId) {
  val shifter = Reg(Bits(32 bit))

  override def doShift(): Unit = {
    shifter := (tap.getTdi ## shifter) >> 1
    tap.setTdo(shifter.lsb)
  }

  override def doReset(): Unit = {
    shifter := value
    tap.setInstruction(instructionId.resized)
  }
}

class JtagInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]],sinkClockDomain : ClockDomain)(tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId){
  val source = Flow Fragment(Bits(1 bit))
  source.valid := False
  source.last := tap.getTms
  source.fragment.lsb := tap.getTdi

  sink << FlowCCByToggle(source,outputClock = sinkClockDomain)

  override def doShift(): Unit = {
    source.valid := True
  }
}