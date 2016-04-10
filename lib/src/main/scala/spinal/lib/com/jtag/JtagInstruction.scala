package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

/**
 * Created by PIC32F_USER on 09/04/2016.
 */

class JtagInstruction(val instructionId: Bits)(implicit tap: JtagTap) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = tap.instruction === instructionId

  def calls(): Unit = {
    when(instructionHit) {
      when(tap.fsm.state === JtagState.DR_CAPTURE) {
        doCapture()
      }
      when(tap.fsm.state === JtagState.DR_SHIFT) {
        doShift()
      }
      when(tap.fsm.state === JtagState.DR_UPDATE) {
        doUpdate()
      }
    }
    when(tap.fsm.state === JtagState.RESET) {
      doReset()
    }
  }
}

class JtagInstructionWrite[T <: Data](data: T, instructionId: Bits, cleanUpdate: Boolean = true, readable: Boolean = true)(implicit tap: JtagTap) extends JtagInstruction(instructionId) {
  val shifter = Reg(Bits(data.getBitsWidth bit))
  val dataReg: T = if (cleanUpdate) Reg(data) else null.asInstanceOf[T]
  if (!cleanUpdate)
    data.assignFromBits(shifter)
  else
    data := dataReg

  override def doShift(): Unit = {
    shifter := (tap.jtag.tdi ## shifter) >> 1
    if (readable) tap.jtag.tdo := shifter.lsb
  }

  override def doUpdate(): Unit = {
    if (cleanUpdate) dataReg.assignFromBits(shifter)
  }

  calls()
}

class JtagInstructionRead[T <: Data](data: T, instructionId: Bits)(implicit tap: JtagTap) extends JtagInstruction(instructionId) {
  val shifter = Reg(Bits(data.getBitsWidth bit))


  override def doCapture(): Unit = {
    shifter := data.asBits
  }

  override def doShift(): Unit = {
    shifter := (tap.jtag.tdi ## shifter) >> 1
    tap.jtag.tdo := shifter.lsb
  }

  calls()
}


class JtagInstructionIdcode[T <: Data](value: Bits, instructionId: Bits)(implicit tap: JtagTap) extends JtagInstruction(instructionId) {
  val shifter = Reg(Bits(32 bit))

  override def doShift(): Unit = {
    shifter := (tap.jtag.tdi ## shifter) >> 1
    tap.jtag.tdo := shifter.lsb
  }

  override def doReset(): Unit = {
    shifter := value
    tap.instruction := instructionId
  }

  calls()
}

class JtagInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]],sinkClockDomain : ClockDomain,instructionId: Bits)(implicit tap: JtagTap) extends JtagInstruction(instructionId){
  val source = Flow Fragment(Bits(1 bit))
  source.valid := False
  source.last := tap.jtag.tms
  source.fragment.lsb := tap.jtag.tdi

  sink << FlowCCByToggle(source,clockOut = sinkClockDomain)

  override def doShift(): Unit = {
    source.valid := True
  }

  calls()
}