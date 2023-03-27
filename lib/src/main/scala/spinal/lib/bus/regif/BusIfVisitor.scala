package spinal.lib.bus.regif
import AccessType._

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
  def getResetValue() : BigInt
  def uvmBaseAcc = List(RO, RW, RC, RS, WRC, WRS, WC, WS, WSRC, WCRS, W1C, W1S, W1T, W0C, W0S, W0T, W1SRC, W1CRS, W0SRC, W0CRS, WO, WOC, WOS, W1, WO1)
  def isUvmAcc: Boolean = uvmBaseAcc.contains(getAccessType())
}

trait  BusIfVisitor {
  def begin(busDataWidth : Int)      : Unit
  def visit(descr : BaseDescriptor)  : Unit
  def end()                          : Unit
}
