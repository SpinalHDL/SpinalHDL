package spinal.lib.bus.mmslavefactory

import spinal.core._

class Field(name: String,
            hardbit: Bits,
            section: Range,
            resetValue: Long,
            readError: Boolean,
            doc: String) extends FieldDescr {

  def tailBitPos = section.max
  def getHardbit = hardbit

  // FieldDescr implementation
  def getName()       : String     = name
  def getWidth()      : Int        = hardbit.getWidth
  def getSection()    : Range      = section
  def getAccessType() : AccessType = AccessType.NA
  def getResetValue() : Long       = resetValue
  def getDoc()        : String     = doc
}

class Reserved(name: String,
               hardbit: Bits,
               section: Range,
               doc: String) extends Field(name, hardbit, section, 0, false, doc) with FieldDescr {
}