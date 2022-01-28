package spinal.lib.bus.regif

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
  def getDoc()         : String
  def getFieldDescrs() : List[FieldDescr]
}

trait FieldDescr {
  def getName()       : String
  def getWidth()      : Int
  def getSection()    : Range
  def getAccessType() : AccessType
  def getResetValue() : Long
  def getDoc()        : String
}

trait  BusIfVisitor {
  def begin(busDataWidth : Int) : Unit
  def visit(descr : FifoDescr)  : Unit  
  def visit(descr : RegDescr)   : Unit
  def end()                     : Unit
}
