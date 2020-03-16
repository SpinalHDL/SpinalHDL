package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapFunctions traits
// gets used to instance read / write operations on the JtagTap itself
//
trait JtagTapFunctions {
  //Instruction wrappers
  def idcode(value: Bits)(instructionId: Int)
  def read[T <: Data](data: T)(instructionId: Int) : JtagTapShifter
  def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int)
  def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int)
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapAccess traits
// is used for the "internal" use of the JtagTap to have defined interface to
// the subclasses
trait JtagTapAccess {
  def getTdi : Bool
  def getTms : Bool
  def setTdo(value : Bool) : Unit

  def getState : JtagState.C
  def getInstruction() : Bits
  def setInstruction(value : Bits) : Unit
}

//══════════════════════════════════════════════════════════════════════════════
// define JtagTapShifter traits
// this trait was a trade off.
// to not-break a current implementation of the JtagTap this trait was necessary
// its only used on the read commands of the JtagTagFunctions
trait JtagTapShifter {
  def shifter : Bits
}
