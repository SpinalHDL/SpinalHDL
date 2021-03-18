package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

// Created by PIC32F_USER on 05/04/2016.
// Modified / Extended by HWEngineer 15/03/2020

//══════════════════════════════════════════════════════════════════════════════
// define Jtag IO's
//
case class Jtag(useTck : Boolean = true) extends Bundle with IMasterSlave {
  val tms = Bool
  val tdi = Bool
  val tdo = Bool
  val tck = if(useTck) Bool else null

  def unclocked = {
    val ret = Jtag(false)
    ret.tms := this.tms
    ret.tdi := this.tdi
    this.tdo := ret.tdo
    ret
  }

  override def asMaster(): Unit = {
    out(tdi, tms)
    in(tdo)
    if(useTck) out(tck)
  }
}

//══════════════════════════════════════════════════════════════════════════════
// define Jtag States
//
object JtagState extends SpinalEnum {
  val RESET, IDLE,
      IR_SELECT, IR_CAPTURE, IR_SHIFT, IR_EXIT1, IR_PAUSE, IR_EXIT2, IR_UPDATE,
      DR_SELECT, DR_CAPTURE, DR_SHIFT, DR_EXIT1, DR_PAUSE, DR_EXIT2, DR_UPDATE
      = newElement()
}

//══════════════════════════════════════════════════════════════════════════════
// define Jtag FSM
//
class JtagFsm(jtag: Jtag) extends Area {
  import JtagState._
  val stateNext = JtagState()
  val state = RegNext(stateNext) randBoot()
  stateNext := state.mux(
    default    -> (jtag.tms ? RESET     | IDLE),           //RESET
    IDLE       -> (jtag.tms ? DR_SELECT | IDLE),
    IR_SELECT  -> (jtag.tms ? RESET     | IR_CAPTURE),
    IR_CAPTURE -> (jtag.tms ? IR_EXIT1  | IR_SHIFT),
    IR_SHIFT   -> (jtag.tms ? IR_EXIT1  | IR_SHIFT),
    IR_EXIT1   -> (jtag.tms ? IR_UPDATE | IR_PAUSE),
    IR_PAUSE   -> (jtag.tms ? IR_EXIT2  | IR_PAUSE),
    IR_EXIT2   -> (jtag.tms ? IR_UPDATE | IR_SHIFT),
    IR_UPDATE  -> (jtag.tms ? DR_SELECT | IDLE),
    DR_SELECT  -> (jtag.tms ? IR_SELECT | DR_CAPTURE),
    DR_CAPTURE -> (jtag.tms ? DR_EXIT1  | DR_SHIFT),
    DR_SHIFT   -> (jtag.tms ? DR_EXIT1  | DR_SHIFT),
    DR_EXIT1   -> (jtag.tms ? DR_UPDATE | DR_PAUSE),
    DR_PAUSE   -> (jtag.tms ? DR_EXIT2  | DR_PAUSE),
    DR_EXIT2   -> (jtag.tms ? DR_UPDATE | DR_SHIFT),
    DR_UPDATE  -> (jtag.tms ? DR_SELECT | IDLE)
  )
}

//══════════════════════════════════════════════════════════════════════════════
// define "generic" JTagTap
//
class JtagTap(jtag: Jtag, instructionWidth: Int) extends Area with JtagTapFunctions{
  val fsm = new JtagFsm(jtag)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val bypass = RegNext(jtag.tdi)
  val tdoUnbufferd = CombInit(bypass)
  val tdoDr = False
  val tdoIr = instructionShift.lsb

  jtag.tdo := ClockDomain.current.withRevertedClockEdge()(RegNext(tdoUnbufferd))

  switch(fsm.state) {
    is(JtagState.IR_CAPTURE) {
      instructionShift := B"01".resize(instructionWidth)
    }
    is(JtagState.IR_SHIFT) {
      instructionShift := (jtag.tdi ## instructionShift) >> 1
      tdoUnbufferd := tdoIr
    }
    is(JtagState.IR_UPDATE) {
      instruction := instructionShift
    }
    is(JtagState.DR_SHIFT) {
      instructionShift := (jtag.tdi ## instructionShift) >> 1
      tdoUnbufferd := tdoDr
    }
  }

  def map(ctrl : JtagTapInstructionCtrl, instructionId : Int): Unit ={
    ctrl.tdi     := jtag.tdi
    ctrl.enable  := instruction === instructionId
    ctrl.capture := fsm.state === JtagState.DR_CAPTURE
    ctrl.shift   := fsm.state === JtagState.DR_SHIFT
    ctrl.update  := fsm.state === JtagState.DR_UPDATE
    ctrl.reset   := fsm.state === JtagState.RESET
    when(ctrl.enable) { tdoDr := ctrl.tdo }
  }

  // implement traits of JtagTapFunctions
  override def idcode(value: Bits)(instructionId: Int) = {
    val area = new JtagTapInstructionIdcode(value)
    map(area.ctrl, instructionId)
    when(fsm.state === JtagState.RESET){ instruction := instructionId}
    area
  }

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

  def instructionWrapper(headerWidth : Int) (instructionId: Int)  = {
    val area = new JtagInstructionWrapper(headerWidth)
    map(area.ctrl, instructionId)
    area
  }

//  override def hasUpdate(now : Bool)(instructionId : Int): Unit ={
//    now :=  instruction === instructionId && fsm.state === JtagState.DR_UPDATE
//  }
}

//══════════════════════════════════════════════════════════════════════════════
// define SimpleJtagTap
//
class SimpleJtagTap extends Component {
  val io = new Bundle {
    val jtag    = slave(Jtag())
    val switchs = in  Bits(8 bit)
    val keys    = in  Bits(4 bit)
    val leds    = out Bits(8 bit)
  }

  val ctrl = new ClockingArea(ClockDomain(io.jtag.tck)) {
    val tap = new JtagTap(io.jtag, 8)
    val idcodeArea = tap.idcode(B"x87654321")(instructionId = 4)
    val switchsArea = tap.read(io.switchs)(instructionId = 5)
    val keysArea = tap.read(io.keys)(instructionId = 6)
    val ledsArea = tap.write(io.leds)(instructionId = 7)
  }
}

//══════════════════════════════════════════════════════════════════════════════
// define SimpleJtagTap object
//
object SimpleJtagTap {
  def main(args: Array[String]) {
    SpinalVhdl(new SimpleJtagTap())
  }
}
