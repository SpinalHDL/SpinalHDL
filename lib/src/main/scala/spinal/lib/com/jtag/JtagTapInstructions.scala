package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

// Created by PIC32F_USER on 05/04/2016.
// Modified / Extended by HWEngineer 15/03/2020

<<<<<<< HEAD
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
=======

case class JtagTapInstructionCtrl() extends Bundle with IMasterSlave {
  val tdi = Bool()
  val tdo = Bool()
  val enable = Bool()
  val capture = Bool()
  val shift = Bool()
  val update = Bool()
  val reset = Bool()

  override def asMaster(): Unit = {
    out(tdi, enable, capture, shift, update, reset)
    in(tdo)
  }
}



class JtagTapInstructionWrite[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val shifter = Reg(Bits(data.getBitsWidth bit))
  val store = readable generate Reg(Bits(data.getBitsWidth bit))

  when(ctrl.enable) {
    if (readable) when(ctrl.capture) {
      shifter := store
    }

    when(ctrl.shift) {
      shifter := (ctrl.tdi ## shifter) >> 1
    }

    if (readable) when(ctrl.update) {
      store := shifter
    }
  }

  if (readable) ctrl.tdo := shifter.lsb else ctrl.tdo := False
  data.assignFromBits(if (!cleanUpdate) shifter else store)
}

class JtagTapInstructionRead[T <: Data](data: T, light : Boolean) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val full = !light generate new Area {
    val shifter = Reg(Bits(widthOf(data) bits))
    when(ctrl.enable) {
      when(ctrl.capture) {
        shifter := B(data)
      }

      when(ctrl.shift) {
        shifter := (ctrl.tdi ## shifter) >> 1
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


class JtagTapInstructionIdcode[T <: Data](value: Bits) extends Area {
  val ctrl = JtagTapInstructionCtrl()
  val shifter = Reg(Bits(32 bit))

  when(ctrl.enable) {
    when(ctrl.shift) {
      shifter := (ctrl.tdi ## shifter) >> 1
    }
  }

  when(ctrl.reset){
    shifter := value
  }

  ctrl.tdo := shifter.lsb
}


class JtagTapInstructionFlowFragmentPush(sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val source = Flow Fragment(Bits(1 bit))
  val valid = RegNext(ctrl.enable && ctrl.shift)
  val data = RegNext(ctrl.tdi)

  source.valid := valid
  source.last := !(ctrl.enable && ctrl.shift)
  source.fragment.lsb := data

  sink << FlowCCByToggle(source, outputClock = sinkClockDomain)

  ctrl.tdo := False
}



class JtagInstructionWrapper(headerWidth : Int) extends Area with JtagTapFunctions{
  val ctrl = JtagTapInstructionCtrl()

  val header = Reg(Bits(headerWidth bits))
  val headerNext = (ctrl.tdi ## header) >> 1
  val counter = Reg(UInt(log2Up(headerWidth) bits))
  val done = Reg(Bool())
  val sendCapture = False
  val sendShift   = False
  val sendUpdate  = False


  when(ctrl.enable){
    when(ctrl.capture){
      done := False
      counter := 0
    }
    when(ctrl.shift){
      when(!done) {
        counter := counter + 1
        header := headerNext
        when(counter === headerWidth - 1) {
          done := True
          sendCapture := True
        }
      } otherwise {
        sendShift := True
      }
    }
    when(ctrl.update){
      sendUpdate := True
    }
  }

  ctrl.tdo := False

  def map(userCtrl : JtagTapInstructionCtrl, instructionId : Int): Unit ={
    val hit = header === instructionId
    userCtrl.tdi     := ctrl.tdi
    userCtrl.enable  := True
    userCtrl.capture := headerNext === instructionId && sendCapture
    userCtrl.shift   := hit && sendShift
    userCtrl.update  := hit && sendUpdate
    userCtrl.reset   := ctrl.reset
    when(hit) { ctrl.tdo := userCtrl.tdo }
  }

  // implement traits of JtagTapFunctions
  override def idcode(value: Bits)(instructionId: Int) = ???
  override def read[T <: Data](data: T, light : Boolean = false)(instructionId: Int) = {
    val area = new JtagTapInstructionRead(data, light = light)
    map(area.ctrl, instructionId)
    area
  }
  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) = {
    val area = new JtagTapInstructionWrite(data, cleanUpdate, readable)
    map(area.ctrl, instructionId)
    area
  }
  override def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) = {
    val area = new JtagTapInstructionFlowFragmentPush(sink, sinkClockDomain)
    map(area.ctrl, instructionId)
    area
>>>>>>> 0d74d517c332bebef7e69ed58e5381275764f5c0
  }
}
