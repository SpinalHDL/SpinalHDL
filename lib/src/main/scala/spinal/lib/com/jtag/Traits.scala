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
  def read[T <: Data](data: T, light : Boolean)(instructionId: Int)
  def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int)
  def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int)
//  def hasUpdate(now : Bool)(instructionId : Int): Unit
}
