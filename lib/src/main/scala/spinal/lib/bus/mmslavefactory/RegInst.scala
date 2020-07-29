package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ListBuffer

class Section(val max: Int, val min: Int){
  override def toString(): String = {
    if(this.max == this.min) {
      s"[${this.min}]"
    } else {
      s"[${this.max}:${this.min}]"
    }
  }
}

object Section{
  def apply(x: Range): Section = new Section(x.max, x.min)
  implicit def tans(x: Range) = Section(x)
}

class Entry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegDescr {
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var rerror: Boolean = false

  def readErrorTag = rerror
  def getFields = fields.toList

  def getAddress : Long = addr

  val hitRead  = bus.readAddress === U(addr)
  val hitWrite = bus.writeAddress === U(addr)
  val hitDoRead  = hitRead && bus.doRead
  val hitDoWrite = hitWrite && bus.doWrite

  def finish = {
    val spareNumbers = if(fields.isEmpty) bus.busDataWidth else bus.busDataWidth-1 - fields.last.tailBitPos
    spareNumbers match {
      case x if x > 0 => reserved(x bits)
      case x if x < 0 => SpinalError(s"Range ${Section(fields.last.getSection)} exceed Bus width ${bus.busDataWidth} in register <${getName}>")
      case _ =>
    }
  }

  def readBits: Bits = {
    fields.map(_.getHardbit).reverse.foldRight(Bits(0 bit))((x,y) => x ## y)
  }
  
  def eventR() : Bool = {
    hitDoRead
  }
  
  def eventW() : Bool = {
    hitDoWrite
  }

  protected def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.assignFromBits(bus.writeData(section))
    that.asBits
  }

  protected def genReservedHandler(bc: BitCount): Bits = {
    val ret : Bits = Bits(bc)
    ret.clearAll()
    ret
  }

  def field[T <: Data](that: T, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Unit = {
    val section : Range = fieldPtr + that.getBitsWidth-1 downto fieldPtr
    val ret : Bits = genDataHandler(that, section, resetValue)
    fields   += new Field(symbol.name, ret, section, resetValue, rerror, doc)
    fieldPtr += that.getBitsWidth
  }

  def newField(bc : BitCount, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val data : Bits = Bits(bc)
    data := B(resetValue)
    field(data, resetValue, doc)
    data
  }

  def reserved(bc: BitCount, doc : String = "") : Bits =  {
    val section: Range = fieldPtr + bc.value-1 downto fieldPtr
    val ret : Bits = genReservedHandler(bc)
    val newdoc = if(doc.isEmpty) "Reserved" else doc
    fields   += new Reserved("reserved", ret, section, newdoc)
    fieldPtr += bc.value
    ret
  }

  // RegDescr implementation
  def getName()        : String           = name
  def getAddr()        : Long             = addr
  def getAccess()      : String           = "RW"
  def getDoc()         : String           = doc
  def getFieldDescrs() : List[FieldDescr] = getFields

  def accept(vs : MMSlaveFactoryVisitor) = {
      vs.visit(this)
  }
}

class RegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {

  override def eventR() : Bool = {
    val event = Reg(Bool) init(False)
    event := hitDoRead
    event
  }

  override def eventW() : Bool = {
    val event = Reg(Bool) init(False)
    event := hitDoWrite
    event
  }

  override def newField(bc : BitCount, resetValue : Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val data : Bits = Reg(Bits(bc)) init(resetValue)
    field(data, resetValue, doc)(symbol)
    data
  }

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    assert(that.isReg)
    when(hitDoWrite) {
      that.assignFromBits(bus.writeData(section))
    }
    that.asBits
  }
}

class ReadOnlyEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.asBits
  }

  override def getAccess() : String = "RO"
}

class WriteOnlyRegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegEntry(name, addr, doc, bus) with RegDescr {

  override def readBits: Bits = {
    val read : Bits = Bits(bus.busDataWidth bits)
    read.clearAll()
    read
  }

  override def getAccess() : String = "WO"
}

class ClearRegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegEntry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    assert(that.isReg)
    when(hitDoWrite) {
      that.assignFromBits((bus.writeData(section) & that.asBits) ^ that.asBits)
    }
    that.asBits
  }

  override def getAccess() : String = "C"
}