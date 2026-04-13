package spinal.lib.bus.mmslavefactory

trait RamDescr {
  def getName()        : String
  def getDoc()         : String
}

trait FifoDescr {
  def getName()        : String
  def getAddr()        : Long
  def getDoc()         : String
}

trait RegDescr {
  def getName()        : String
  def getAddr()        : Long
  def getAccess()      : String
  def getDoc()         : String
  def getFieldDescrs() : List[FieldDescr]
}

trait FieldDescr {
  def getName()       : String
  def getWidth()      : Int
  def getSection()    : Range
  def getResetValue() : Long
  def getDoc()        : String
  def isReserved()    : Boolean
}

trait  MMSlaveFactoryVisitor {
  def begin(busDataWidth : Int) : Unit
  def visit(descr : FifoDescr)  : Unit  
  def visit(descr : RegDescr)   : Unit
  def end()                     : Unit
}
