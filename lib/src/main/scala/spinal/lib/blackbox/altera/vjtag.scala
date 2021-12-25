package spinal.lib.blackbox.altera

import spinal.core._
import spinal.lib.com.jtag.JtagTapInstructionCtrl


case class VJTAG() extends BlackBox{
 

  val virtual_state_cdr  = out Bool()
  val virtual_state_sdr  = out Bool()
  val tck  = out Bool()
  val tdi  = out Bool()
  val virtual_state_udr  = out Bool()
  val tdo  = in Bool()

  def toJtagTapInstructionCtrl() = {
    val i = JtagTapInstructionCtrl()
    i.enable := True
    i.tdi     <> tdi
    i.capture <> virtual_state_cdr
    i.shift   <> virtual_state_sdr
    i.update  <> virtual_state_udr
    i.tdo     <> tdo
    i.reset := False
    i
  }
}

