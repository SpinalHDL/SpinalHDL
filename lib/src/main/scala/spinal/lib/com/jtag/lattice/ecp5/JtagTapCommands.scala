package spinal.lib.com.jtag.lattice.ecp5

import spinal.core._
import spinal.lib._

import spinal.lib.blackbox.lattice.ecp5.{JtaggIo}
import spinal.lib.com.jtag.{JtagTapFunctions, JtagTapShifter}

//══════════════════════════════════════════════════════════════════════════════
// JtagTapCommand
//
class JtagTapCommand(tap: JtagTapInternalProtocol, instructionId: Int) extends Area {

  assert( (instructionId == 0x32) || (instructionId == 0x38),
          "Only instructions 0x32 / 0x38 are for embedded jtagg usage")

  def doCapture(): Unit = {}
  def doShift(): Unit = {}
  def doUpdate(): Unit = {}
  def doLastShift(): Unit = {}
  def doReset(): Unit = {}

  if(instructionId == 0x32){
    Component.current.addPrePopTask(() => {
          switch(tap.getJTaggState){
            is(JtagTapState.Capture_DR1){
              doCapture()
            }
            is(JtagTapState.Shift_DR1){
              doShift()
            }
            is(JtagTapState.Shift_DR1_Last){
              doShift()
              doLastShift()
            }
            is(JtagTapState.Update_DR1){
              doUpdate()
            }
            is(JtagTapState.Reset){
              doReset()
            }
          }
      })
  }

  if(instructionId == 0x38){
    Component.current.addPrePopTask(() => {
          switch(tap.getJTaggState){
            is(JtagTapState.Capture_DR2){
              doCapture()
            }
            is(JtagTapState.Shift_DR2){
              doShift()
            }
            is(JtagTapState.Shift_DR2_Last){
              doShift()
              doLastShift()
            }
            is(JtagTapState.Update_DR2){
              doUpdate()
            }
            is(JtagTapState.Reset){
              doReset()
            }
          }
      })
  }
}

//══════════════════════════════════════════════════════════════════════════════
// JtagTapCommandRead
//
class JtagTapCommandRead[T <: Data](data: T)
                                   (tap: JtagTapInternalProtocol, instructionId: Int)
                                   extends JtagTapCommand(tap, instructionId)
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
// JtagTapCommandWrite
//
class JtagTapCommandWrite[T <: Data](data: T,  cleanUpdate: Boolean = true, readable: Boolean = true)
                                    (tap: JtagTapInternalProtocol, instructionId: Int)
                                    extends JtagTapCommand(tap, instructionId){

  val shifter = Reg(Bits(data.getBitsWidth bit))
  val store = Reg(Bits(data.getBitsWidth bit))

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
// JtagTapCommandFlowFragmentPush
//
class JtagTapCommandFlowFragmentPush(sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)
                                    (tap: JtagTapInternalProtocol, instructionId: Int)
                                    extends JtagTapCommand(tap, instructionId){

  val source = Flow Fragment(Bits(1 bit))
  source.valid := False
  source.last := False
  source.fragment.lsb := tap.getTdi

  sink << FlowCCByToggle(source, outputClock = sinkClockDomain)

  override def doLastShift(): Unit = {
    source.last := True
  }

  override def doShift(): Unit = {
    source.valid := True
  }
}
