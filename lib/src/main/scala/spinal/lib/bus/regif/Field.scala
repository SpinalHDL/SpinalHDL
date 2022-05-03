package spinal.lib.bus.regif

import spinal.core._

case class Field(name: String,
                 hardbit: BaseType,
                 section: Range,
                 accType: AccessType,
                 resetValue: Long,
                 readError: Boolean,
                 doc: String) extends FieldDescr {
  private var _name = name

  def tailBitPos = section.max

  // FieldDescr implementation
  def getName()       : String     = _name
  def setName(name: String): Field = {_name = name ; this}
  def getWidth()      : Int        = hardbit.getBitsWidth
  def getSection()    : Range      = section
  def getAccessType() : AccessType = accType
  def getResetValue() : Long       = resetValue
  def getDoc()        : String     = doc
}