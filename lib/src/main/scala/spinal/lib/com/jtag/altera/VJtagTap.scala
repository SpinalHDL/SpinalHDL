package spinal.lib.com.jtag.altera
import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.JtagTapInstructionCtrl
import spinal.lib.blackbox.altera.VJTAG
import spinal.lib.com.jtag._


class VjtagTap(vjtag: VJTAG, instructionWidth: Int) extends Area with JtagTapFunctions {
  // bypass route if instruction not handled, will be overridden by instructions
  val bypass = RegNext(vjtag.tdi)

  vjtag.tdo := bypass

  def map(ctrl: JtagTapInstructionCtrl, instructionId: Int): Unit = {
    ctrl.tdi := vjtag.tdi
    ctrl.enable := vjtag.ir_in === instructionId
    ctrl.capture := vjtag.virtual_state_cdr
    ctrl.shift := vjtag.virtual_state_sdr
    ctrl.update := vjtag.virtual_state_udr
    ctrl.reset := False
    when(ctrl.enable) { vjtag.tdo := ctrl.tdo }
  }

  override def idcode(value: Bits)(instructionId: Int) = ???

  override def read[T <: Data](data: T, light: Boolean = false)(instructionId: Int) = {
    val area = new JtagTapInstructionRead(data, light = light)
    map(area.ctrl, instructionId)
    area
  }
  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) = {
    val area = new JtagTapInstructionWrite(data, cleanUpdate, readable)
    map(area.ctrl, instructionId)
    area
  }
  override def flowFragmentPush[T <: Data](sink: Flow[Fragment[Bits]], sinkClockDomain: ClockDomain)(
      instructionId: Int
  ) = {
    val area = new JtagTapInstructionFlowFragmentPush(sink, sinkClockDomain)
    map(area.ctrl, instructionId)
    area
  }
  // from a jtag main standpoint. the jtag debugger either updates(write to) the DR, or captures it (read from)
  override def readAndWrite[T <: Data](captureData: T, updateData: T, captureReady: Bool, updateValid: Bool)(
      instructionId: Int
  ) = {
    val area = new JtagTapInstructionReadWrite(captureData, updateData, captureReady)
    map(area.ctrl, instructionId)
    updateValid := area.ctrl.enable && area.ctrl.update
    area
  }
}
