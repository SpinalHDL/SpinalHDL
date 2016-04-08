package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

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


class JtagCtrl(instructionWidth : Int)extends Component{
  val io = new Bundle{
    val jtag = slave(Jtag())
    val value = out Bits(8 bit)
  }

  val fsm = new JtagFsm(io.jtag)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val idcode = Reg(Bits(32 bit))
  val bypass = Reg(Bool)
  when(fsm.state === JtagState.RESET){
    idcode := B"x87654321"
    instruction := 16
  }

  val value = Reg(Bits(8 bit))
  io.value := value
  io.jtag.tdo := False
  switch(fsm.state){
    is(JtagState.IR_CAPTURE){
      instructionShift := instruction
    }
    is(JtagState.IR_SHIFT){
      instructionShift := (io.jtag.tdi ## instructionShift) >> 1
      io.jtag.tdo := instructionShift(0)
    }
    is(JtagState.IR_UPDATE){
      instruction := instructionShift
    }

    is(JtagState.DR_CAPTURE){

    }
    is(JtagState.DR_SHIFT){
      instructionShift := (io.jtag.tdi ## instructionShift) >> 1
      switch(instruction){
        is(16){
          idcode := (io.jtag.tdi ## idcode) >> 1
          io.jtag.tdo := idcode(0)
        }
        is(17){
          value := (io.jtag.tdi ## value) >> 1
          io.jtag.tdo := value(0)
        }
        default {
          bypass := io.jtag.tdi
          io.jtag.tdo := bypass
        }
      }
    }
    is(JtagState.DR_UPDATE){
    }
  }
}

object TapCtrl{
  def main(args: Array[String]) {
    SpinalVhdl(new JtagCtrl(10).setDefinitionName("TopLevel"))
  }
}


class SimpleTap extends Component{
  val io = new Bundle{
    val jtag = slave(Jtag())
    val value = out Bits(8 bit)
  }

  val jtagCtrl = new JtagCtrl(8)
  jtagCtrl.io.jtag <> io.jtag


  io.value := jtagCtrl.io.value
}


object SimpleTap{
  def main(args: Array[String]) {
    SpinalVhdl(new SimpleTap(),_.setLibrary("SimpleTap_lib"))
  }
}