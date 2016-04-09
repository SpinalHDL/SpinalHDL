package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 05/04/2016.
 */


case class Jtag() extends Bundle with IMasterSlave{
  val tms = Bool
  val tdi = Bool
  val tdo = Bool

  override def asMaster(): Jtag.this.type = {
    out(tdi,tms)
    in(tdo)
    this
  }
}

object JtagState extends SpinalEnum{
  val RESET,IDLE= newElement()
  val IR_SELECT,IR_CAPTURE,IR_SHIFT,IR_EXIT1,IR_PAUSE,IR_EXIT2,IR_UPDATE = newElement()
  val DR_SELECT,DR_CAPTURE,DR_SHIFT,DR_EXIT1,DR_PAUSE,DR_EXIT2,DR_UPDATE = newElement()
}

class JtagFsm(jtag : Jtag) extends Area {
  import JtagState._
  val stateNext = JtagState()
  val state = RegNext(stateNext)
  stateNext := state.map(
    default -> //reset
      Mux(jtag.tms,RESET,IDLE),
    IDLE ->
      Mux(jtag.tms,DR_SELECT,IDLE),
    IR_SELECT ->
      Mux(jtag.tms,RESET,IR_CAPTURE),
    IR_CAPTURE ->
      Mux(jtag.tms,IR_EXIT1,IR_SHIFT),
    IR_SHIFT ->
      Mux(jtag.tms,IR_EXIT1,IR_SHIFT),
    IR_EXIT1 ->
      Mux(jtag.tms,IR_UPDATE,IR_PAUSE),
    IR_PAUSE ->
      Mux(jtag.tms,IR_EXIT2,IR_PAUSE),
    IR_EXIT2 ->
      Mux(jtag.tms,IR_UPDATE,IR_SHIFT),
    IR_UPDATE ->
      Mux(jtag.tms,DR_SELECT,IDLE),
    DR_SELECT ->
      Mux(jtag.tms,IR_SELECT,DR_CAPTURE),
    DR_CAPTURE ->
      Mux(jtag.tms,DR_EXIT1,DR_SHIFT),
    DR_SHIFT ->
      Mux(jtag.tms,DR_EXIT1,DR_SHIFT),
    DR_EXIT1 ->
      Mux(jtag.tms,DR_UPDATE,DR_PAUSE),
    DR_PAUSE ->
      Mux(jtag.tms,DR_EXIT2,DR_PAUSE),
    DR_EXIT2 ->
      Mux(jtag.tms,DR_UPDATE,DR_SHIFT),
    DR_UPDATE ->
      Mux(jtag.tms,DR_SELECT,IDLE)
  )
}


class JtagInstruction(val instructionId : Bits)(implicit tap : JtagTap) extends Area{
  def doCapture() : Unit = {}
  def doShift() : Unit  = {}
  def doUpdate() : Unit  = {}
  def doReset() : Unit = {}

  val instructionHit = tap.instruction === instructionId

  def calls() : Unit = {
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

class JtagInstructionWrite[T <: Data](data : T, instructionId : Bits,cleanUpdate : Boolean = true,readable : Boolean = true)(implicit tap : JtagTap) extends JtagInstruction(instructionId){
  val shifter = Reg(Bits(data.getBitsWidth bit))
  val dataReg : T = if(cleanUpdate) Reg(data) else null.asInstanceOf[T]
  if(!cleanUpdate)
    data.assignFromBits(shifter)
  else
    data := dataReg

  override def doShift(): Unit = {
    shifter := (tap.jtag.tdi ## shifter) >> 1
    if(readable) tap.jtag.tdo := shifter.lsb
  }

  override def doUpdate(): Unit = {
    if(cleanUpdate) dataReg.assignFromBits(shifter)
  }

  calls()
}

class JtagInstructionRead[T <: Data](data : T, instructionId : Bits)(implicit tap : JtagTap) extends JtagInstruction(instructionId){
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


class JtagInstructionIdcode[T <: Data](value : Bits, instructionId : Bits)(implicit tap : JtagTap) extends JtagInstruction(instructionId){
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



class JtagTap(val jtag : Jtag,instructionWidth : Int) extends Area{
  val fsm = new JtagFsm(jtag)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val bypass = Reg(Bool)

  jtag.tdo := bypass

  switch(fsm.state){
    is(JtagState.IR_CAPTURE){
      instructionShift := instruction
    }
    is(JtagState.IR_SHIFT){
      instructionShift := (jtag.tdi ## instructionShift) >> 1
      jtag.tdo := instructionShift.lsb
    }
    is(JtagState.IR_UPDATE){
      instruction := instructionShift
    }
    is(JtagState.DR_SHIFT){
      bypass := jtag.tdi
    }
  }
}



class SimpleTap extends Component{
  val io = new Bundle{
    val jtag = slave(Jtag())
    val switchs = in Bits(8 bit)
    val leds = out Bits(8 bit)
  }

  implicit val tap = new JtagTap(io.jtag,8)
  val idcodeArea = new JtagInstructionIdcode(B"x87654321",16)
  val switchsArea = new JtagInstructionRead(io.switchs,18)
  val ledsArea = new JtagInstructionWrite(io.leds,17,cleanUpdate = false,readable = false)
}


object SimpleTap{
  def main(args: Array[String]) {
    SpinalVhdl(new SimpleTap(),_.setLibrary("SimpleTap_lib"))
  }
}