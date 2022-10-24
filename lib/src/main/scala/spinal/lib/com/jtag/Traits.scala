package spinal.lib.com.jtag

import spinal.core._
import spinal.lib._

/**
 * Defines JtagTapFunctions traits
 * gets used to instance read / write operations on the JtagTap itself
 */
trait JtagTapFunctions {
  //Instruction wrappers

  /**
   * must implement the IDCODE for jtag
   * @param value idcode that will be read from jtag access
   * @param instructionId instructionID or IR at which the idcode will be generated
   */
  def idcode(value: Bits)(instructionId: Int) : Area

  /**
   * must implement a read access (DR capture) for jtag
   * @param data element that will be captured
   * @param light generates a light version of the read function (no update from DR SCAN)
   * @param instructionId instructionID or IR at which the data read will be generated
   */
  def read[T <: Data](data: T, light : Boolean)(instructionId: Int) : Area

  /**
   * must implement a write access (DR UPDATE) for jtag
   * @param data element that will be updated
   * @param cleanUpdate data will only be updated when DR UPDATE is complete. shifted each TCK otherwise
   * @param readable  allow for data
   * @param instructionId instructionID or IR at which the data write will be generated
   */
  def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) : Area

  /**
   * must implement a read/write access (both DR SCAN/UPDATE) with different endpoints
   * This can easily be used with streams in/out of it.
   * @param captureData element that will be captured
   * @param updateData  element that will be updated
   * @param captureReady  Bool that indicates when the captureData is ready for a new value
   * @param updateValid   Bool of a 1 TCK long pulse indicating that the updateData is valid
   * @param instructionId instructionID or IR at which the data read/write will be generated
   */
  def readAndWrite[T<: Data](captureData: T, updateData: T, captureReady: Bool, updateValid:Bool)(instructionId: Int) : Area
  def readAndWriteWithEvents[T<: Data, T2 <: Data](captureType: HardType[T], updateType: HardType[T2])(instructionId: Int) = new Area {
    val captureData = captureType()
    val updateData = updateType()
    val captureValid = isCapturing(instructionId)
    val updateValid  = isUpdating(instructionId)
    val logic = readAndWrite(captureData,updateData,Bool() ,Bool())(instructionId)
  }

  /**
   * must implement a Fragment output from a DR UPDATE
   * @param sink  resulting flow[Fragment] of Bits
   * @param sinkClockDomain clockdomain of sink (for cross clocking)
   * @param instructionId instructionID or IR at which the fragment will be generated
   */
  def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) : Area


  def isUpdating(instructionId : Int) : Bool = ???
  def isCapturing(instructionId : Int) : Bool = ???
  def isReseting() : Bool = ???
}
