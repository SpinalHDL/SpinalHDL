package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

/**
 * Created by PIC32F_USER on 09/04/2016.
 */

trait JtagTapAccess {
  def jtag : Jtag
  def state : JtagState.T
  def getInstruction() : Bits
  def setInstruction(value : Bits) : Unit
}

class JtagInstruction(tap: JtagTapAccess,val instructionId: Bits) extends Area {
  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doReset(): Unit = {}

  val instructionHit = tap.getInstruction === instructionId

  def calls(): Unit = {
    when(instructionHit) {
      when(tap.state === JtagState.DR_CAPTURE) {
        doCapture()
      }
      when(tap.state === JtagState.DR_SHIFT) {
        doShift()
      }
      when(tap.state === JtagState.DR_UPDATE) {
        doUpdate()
      }
    }
    when(tap.state === JtagState.RESET) {
      doReset()
    }
  }
}

class JtagInstructionWrite[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true) (tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId) {
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

class JtagInstructionRead[T <: Data](data: T) (tap: JtagTapAccess,instructionId: Bits)extends JtagInstruction(tap,instructionId) {
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


class JtagInstructionIdcode[T <: Data](value: Bits)(tap: JtagTapAccess, instructionId: Bits)extends JtagInstruction(tap,instructionId) {
  val shifter = Reg(Bits(32 bit))

  override def doShift(): Unit = {
    shifter := (tap.jtag.tdi ## shifter) >> 1
    tap.jtag.tdo := shifter.lsb
  }

  override def doReset(): Unit = {
    shifter := value
    tap.setInstruction(instructionId)
  }

  calls()
}

class JtagInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]],sinkClockDomain : ClockDomain)(tap: JtagTapAccess,instructionId: Bits) extends JtagInstruction(tap,instructionId){
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