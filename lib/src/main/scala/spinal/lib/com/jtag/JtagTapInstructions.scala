package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.BSCANE2

// Created by PIC32F_USER on 05/04/2016.
// Modified / Extended by HWEngineer 15/03/2020


case class JtagTapInstructionCtrl() extends Bundle with IMasterSlave {
  val tdi = Bool()
  val enable = Bool()
  val capture = Bool()
  val shift = Bool()
  val update = Bool()
  val reset = Bool()
  val tdo = Bool()

  override def asMaster(): Unit = {
    out(tdi, enable, capture, shift, update, reset)
    in(tdo)
  }

  def fromXilinxBscane2(userId : Int) = {
    val tap = BSCANE2(userId)
    tdi     := tap.TDI
    enable  := tap.SEL
    capture := tap.CAPTURE
    shift   := tap.SHIFT
    update  := tap.UPDATE
    reset   := tap.RESET
    tap.TDO := tdo
    tap
  }

  def <<(that : JtagTapInstructionCtrl): Unit ={
    this.tdi := that.tdi
    this.enable := that.enable
    this.capture := that.capture
    this.shift := that.shift
    this.update := that.update
    this.reset := that.reset
    that.tdo := this.tdo
  }
}



class JtagTapInstructionWrite[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true) extends Area {
  val ctrl = JtagTapInstructionCtrl()

  val shifter = Reg(Bits(widthOf(data) bit))
  val store = readable generate Reg(Bits(widthOf(data) bit))

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

  when(ctrl.capture){
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
  }
}
