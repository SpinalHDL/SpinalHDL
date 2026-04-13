package spinal.lib.bus.mmslavefactory

import spinal.core._

class Field(name: String,
            width: BitCount,
            resetValue: Long,
            doc: String) extends FieldDescr {

  // enable read/write error response
  protected var rerror: Boolean = false
  protected var werror: Boolean = false
  def readErrorTag = rerror
  def writeErrorTag = werror

  // Goes high when the current read/write request is accepted. That means that
  // the request can be served (ready signal).
  val readAccept = Bool()
  val writeAccept = Bool()

  def getHardbit = hardbit

  var hardbit : Bits = null

  def handleRead[T <: Data](that: T): Unit = {
    hardbit = that.asBits
  }

  def handleWrite[T <: Data](that: T, busData : Bits): Bits = {
    busData
  }

  // FieldDescr implementation
  def getName()       : String     = name
  def getWidth()      : Int        = if(hardbit == null) 0 else hardbit.getWidth
  def getSection()    : Range      = null
  def getResetValue() : Long       = resetValue
  def getDoc()        : String     = doc
  def isReserved()    : Boolean    = false;
}

class FieldReserved(name: String,
                    bitCount : BitCount,
                    doc: String) extends Field(name, bitCount, 0, doc) with FieldDescr {
  
  hardbit = Bits(bitCount)
  hardbit.clearAll()

  override def isReserved() : Boolean = true;
}

class FieldReadOnly(name: String,
                    bitCount : BitCount,
                    resetValue: Long,
                    doc: String) extends Field(name, bitCount, 0, doc) with FieldDescr {

  hardbit = resetValue

  override def handleWrite[T <: Data](that: T, busData : Bits): Bits = {
    hardbit
  }
}

class FieldReg(name: String,
               bitCount : BitCount,
               resetValue: Long,
               doc: String) extends Field(name, bitCount, 0, doc) with FieldDescr {

  writeAccept := True
  readAccept := True

  override def handleRead[T <: Data](that: T): Unit = {
    assert(that.isReg)
    hardbit = that.asBits
  }

  override def handleWrite[T <: Data](that: T, busData : Bits): Bits = {
    assert(that.isReg)
    that.removeAssignments()
    when(writeAccept) {
      that.assignFromBits(busData)
    }
    that.asBits
  }
}

class FieldWriteOnlyReg(name: String,
               bitCount : BitCount,
               resetValue: Long,
               doc: String) extends Field(name, bitCount, 0, doc) with FieldDescr {

  writeAccept := True
  readAccept := True

  hardbit = Bits(bitCount)
  hardbit.clearAll()

  override def handleRead[T <: Data](that: T): Unit = {
    // do nothing
  }

  override def handleWrite[T <: Data](that: T, busData : Bits): Bits = {
    assert(that.isReg)
    that.removeAssignments()
    when(writeAccept) {
      that.assignFromBits(busData)
    }
    that.asBits
  }
}