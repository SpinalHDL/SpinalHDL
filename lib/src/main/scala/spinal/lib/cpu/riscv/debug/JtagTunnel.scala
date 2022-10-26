package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag._


class JtagTunnel(ctrl : JtagTapInstructionCtrl, instructionWidth : Int) extends Area with JtagTapFunctions{
  val shiftBuffer = Reg(Bits(instructionWidth + 7 + 1 bits))
  val instruction = Reg(Bits(instructionWidth bits))


  val sendCapture = False
  val sendShift   = False
  val sendUpdate  = False

  when(ctrl.reset){
    instruction := 0
  }

  when(ctrl.enable){
    when(ctrl.capture){
      sendCapture := True
    }
    when(ctrl.shift){
      shiftBuffer := (ctrl.tdi ## shiftBuffer) >> 1
      sendShift := True
    }
    when(ctrl.update){
      when(!shiftBuffer.msb){
        instruction := shiftBuffer.resized
      } otherwise {
        sendUpdate := True
      }
    }
  }

  //Basicaly, with JTAG, especialy if you are in a chain, the only reference you have in a dr-scan is when it finish,
  //as you may have some extra dr-scan cycle at the beggining to propagate through the chaine
  val tdiBuffer = Delay(ctrl.tdi, 1+7+1)
  val tdoBuffer = False
  val tdoShifter = Delay(tdoBuffer, 4)
  ctrl.tdo := tdoShifter

  def map(userCtrl : JtagTapInstructionCtrl, instructionId : Int): Unit ={
    val hit = instruction === instructionId
    userCtrl.tdi     := tdiBuffer
    userCtrl.enable  := hit
    userCtrl.capture := hit && sendCapture
    userCtrl.shift   := hit && sendShift
    userCtrl.update  := hit && sendUpdate
    userCtrl.reset   := ctrl.reset
    when(hit) { tdoBuffer := userCtrl.tdo }
  }

  override def idcode(value: Bits)(instructionId: Int) = ???
  override def read[T <: Data](data: T, light : Boolean = false)(instructionId: Int) = {
    ???
  }
  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) = {
    ???
  }
  override def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) = {
    ???
  }
  override def readAndWrite[T<: Data](captureData: T, updateData: T, captureReady: Bool, updateValid:Bool)(instructionId: Int) = {
    val area = new JtagTapInstructionReadWrite(captureData, updateData, captureReady)
    map(area.ctrl, instructionId)
    updateValid := area.ctrl.enable && area.ctrl.update
    area
  }

  override def isUpdating(instructionId: Int) = instruction === instructionId && sendUpdate
  override def isCapturing(instructionId: Int) = instruction === instructionId && sendCapture
  override def isReseting() = ctrl.reset
}