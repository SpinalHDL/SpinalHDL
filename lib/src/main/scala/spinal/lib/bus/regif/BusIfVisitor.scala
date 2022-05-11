package spinal.lib.bus.regif

trait BaseDescriptor {
  def getName()        : String
  def getDoc()         : String
}

trait MemoryMappedDescriptor extends BaseDescriptor {
  def getAddr()        : BigInt
  def getSize()        : BigInt

  def accept(visitor: BusIfVisitor): Unit = visitor.visit(this)
}

trait RamDescr extends MemoryMappedDescriptor {}

trait FifoDescr extends MemoryMappedDescriptor {}

trait RegDescr extends MemoryMappedDescriptor {
  def getFieldDescrs() : List[FieldDescr]
}

trait FieldDescr extends BaseDescriptor {
  def getWidth()      : Int
  def getSection()    : Range
  def getAccessType() : AccessType
  def getResetValue() : Long
}

trait  BusIfVisitor {
  def begin(busDataWidth : Int)      : Unit
  def visit(descr : BaseDescriptor)  : Unit
  def end()                          : Unit
}
