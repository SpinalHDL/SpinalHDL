package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 05/04/2016.
 */


case class Jtag() extends Bundle with IMasterSlave {
  val tms = Bool
  val tdi = Bool
  val tdo = Bool

  override def asMaster(): Jtag.this.type = {
    out(tdi, tms)
    in(tdo)
    this
  }
}

object JtagState extends SpinalEnum {
  val RESET, IDLE = newElement()
  val IR_SELECT, IR_CAPTURE, IR_SHIFT, IR_EXIT1, IR_PAUSE, IR_EXIT2, IR_UPDATE = newElement()
  val DR_SELECT, DR_CAPTURE, DR_SHIFT, DR_EXIT1, DR_PAUSE, DR_EXIT2, DR_UPDATE = newElement()
}

class JtagFsm(jtag: Jtag) extends Area {

  import JtagState._

  val stateNext = JtagState()
  val state = RegNext(stateNext) randBoot()
  stateNext := state.mux(
    default -> //reset
      Mux(jtag.tms, RESET, IDLE),
    IDLE ->
      Mux(jtag.tms, DR_SELECT, IDLE),
    IR_SELECT ->
      Mux(jtag.tms, RESET, IR_CAPTURE),
    IR_CAPTURE ->
      Mux(jtag.tms, IR_EXIT1, IR_SHIFT),
    IR_SHIFT ->
      Mux(jtag.tms, IR_EXIT1, IR_SHIFT),
    IR_EXIT1 ->
      Mux(jtag.tms, IR_UPDATE, IR_PAUSE),
    IR_PAUSE ->
      Mux(jtag.tms, IR_EXIT2, IR_PAUSE),
    IR_EXIT2 ->
      Mux(jtag.tms, IR_UPDATE, IR_SHIFT),
    IR_UPDATE ->
      Mux(jtag.tms, DR_SELECT, IDLE),
    DR_SELECT ->
      Mux(jtag.tms, IR_SELECT, DR_CAPTURE),
    DR_CAPTURE ->
      Mux(jtag.tms, DR_EXIT1, DR_SHIFT),
    DR_SHIFT ->
      Mux(jtag.tms, DR_EXIT1, DR_SHIFT),
    DR_EXIT1 ->
      Mux(jtag.tms, DR_UPDATE, DR_PAUSE),
    DR_PAUSE ->
      Mux(jtag.tms, DR_EXIT2, DR_PAUSE),
    DR_EXIT2 ->
      Mux(jtag.tms, DR_UPDATE, DR_SHIFT),
    DR_UPDATE ->
      Mux(jtag.tms, DR_SELECT, IDLE)
  )
}


class JtagTap(val jtag: Jtag, instructionWidth: Int) extends Area with JtagTapAccess{
  val fsm = new JtagFsm(jtag)
  val instruction = Reg(Bits(instructionWidth bit))
  val instructionShift = Reg(Bits(instructionWidth bit))
  val bypass = Reg(Bool)

  jtag.tdo := bypass

  switch(fsm.state) {
    is(JtagState.IR_CAPTURE) {
      instructionShift := instruction
    }
    is(JtagState.IR_SHIFT) {
      instructionShift := (jtag.tdi ## instructionShift) >> 1
      jtag.tdo := instructionShift.lsb
    }
    is(JtagState.IR_UPDATE) {
      instruction := instructionShift
    }
    is(JtagState.DR_SHIFT) {
      bypass := jtag.tdi
    }
  }

  //JtagTapAccess impl
  override def getInstruction(): Bits = instruction
  override def setInstruction(value: Bits): Unit = instruction := value
  override def state: JtagState.T = fsm.state

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


class SimpleTap extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val switchs = in Bits (8 bit)
    val leds = out Bits (8 bit)
  }

  implicit val tap = new JtagTap(io.jtag, 8)
  val idcodeArea = tap.idcode(B"x87654321")(16)
  val switchsArea =tap.read(io.switchs)(18)
  val ledsArea = tap.write(io.leds)(17)
}



object SimpleTap {
  def main(args: Array[String]) {
    SpinalVhdl(new SimpleTap(), _.setLibrary("SimpleTap_lib"))
  }
}