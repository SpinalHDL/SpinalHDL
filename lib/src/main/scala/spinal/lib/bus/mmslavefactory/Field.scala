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
  def getWidth()      : Int        = section.length
  def getSection()    : Range      = section
  def getResetValue() : Long       = resetValue
  def getDoc()        : String     = doc
  def isReserved()    : Boolean    = false;
}

class Reserved(name: String,
               hardbit: Bits,
               section: Range,
               doc: String) extends Field(name, hardbit, section, 0, false, doc) with FieldDescr {
  
  override def isReserved() : Boolean = true;
}