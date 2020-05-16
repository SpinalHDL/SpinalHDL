package spinal.lib.bus.regif

import spinal.core._

case class Field(name: String,
                 hardbit: Bits,
                 section: Range,
                 accType: AccessType,
                 resetValue: Long,
                 readError: Boolean,
                 doc: String) extends FieldDescr {

  def tailBitPos = section.max

  // FieldDescr implementation
  def getName()       : String     = name
  def getWidth()      : Int        = hardbit.getWidth
  def getSection()    : Range      = section
  def getAccessType() : AccessType = accType
  def getResetValue() : Long       = resetValue
  def getDoc()        : String     = doc
}