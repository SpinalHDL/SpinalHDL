package spinal.lib.bus.regif

import spinal.core._
import AccessType._

case class Field(name: String,
                 hardbit: BaseType,
                 section: Range,
                 accType: AccessType,
                 resetValue: BigInt,
                 readError: Boolean,
                 doc: String){
  private var _name = name

  def tailBitPos = section.max

  // FieldDescr implementation
  def getName()       : String     = _name
  def setName(name: String): Field = {_name = name ; this}
  def getWidth()      : Int        = hardbit.getBitsWidth
  def getSection()    : Range      = section
  def getAccessType() : AccessType = accType
  def getResetValue() : BigInt     = resetValue
  def getDoc()        : String     = doc
  def isWriteOnly()   : Boolean    = accType match {
    case `WO` | `WO1` | `WOS` | `WOC` => true
    case _ => false
  }
  def uvmBaseAcc = List(RO, RW, RC, RS, WRC, WRS, WC, WS, WSRC, WCRS, W1C, W1S, W1T, W0C, W0S, W0T, W1SRC, W1CRS, W0SRC, W0CRS, WO, WOC, WOS, W1, WO1)
  def isUvmAcc: Boolean = uvmBaseAcc.contains(getAccessType())
}