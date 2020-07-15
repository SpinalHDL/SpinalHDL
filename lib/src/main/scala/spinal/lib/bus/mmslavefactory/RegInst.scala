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

class ClearRegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegEntry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := (bus.writeData(section) & ret) ^ ret
    }
    ret
  }
}

class ReadOnlyEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends CombEntry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler(bc: BitCount, section: Range, resetValue: Long): Bits = Bits(bc)
}

class RegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends CombEntry(name, addr, doc, bus) with RegDescr {

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

  override def genDataHandler(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := bus.writeData(section)
    }
    ret
  }
}

class CombEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegDescr {
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

  def readErrorTag = Rerror
  def getFields = fields.toList

  def getAddress : Long = addr

  val hitRead  = bus.readAddress === U(addr)
  val hitWrite = bus.writeAddress === U(addr)
  val hitDoRead  = hitRead && bus.doRead
  val hitDoWrite = hitWrite && bus.doWrite

  def checkLast={
    val spareNumbers = if(fields.isEmpty) bus.busDataWidth else bus.busDataWidth-1 - fields.last.tailBitPos
    spareNumbers match {
      case x if x > 0 => addReserved(x bits)
      case x if x < 0 => SpinalError(s"Range ${Section(fields.last.getSection)} exceed Bus width ${bus.busDataWidth}")
      case _ =>
    }
  }

  def allIsNA: Boolean = {
    checkLast
    fields.map(_.getAccessType == AccessType.NA).foldLeft(true)(_&&_)
  }

  def readBits: Bits = {
    fields.map(_.getHardbit).reverse.foldRight(Bits(0 bit))((x,y) => x ## y) //TODO
  }
  
  def eventR() : Bool = {
    hitDoRead
  }
  
  def eventW() : Bool = {
    hitDoWrite
  }

  protected def genDataHandler(bc: BitCount, section: Range, resetValue: Long): Bits = {
    bus.writeData(section)
  }

  protected def genReservedHandler(bc: BitCount): Bits = {
    val ret : Bits = Bits(bc)
    ret.clearAll()
    ret
  }

  def field(bc: BitCount, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    addField(bc, resetValue, doc)
  }

  def addField(bc: BitCount, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val section : Range = fieldPtr + bc.value-1 downto fieldPtr
    val ret : Bits = genDataHandler(bc, section, resetValue)
    fields   += new Field(symbol.name, ret, section, resetValue, Rerror, doc)
    fieldPtr += bc.value
    ret
  }

  def addReserved(bc: BitCount, doc : String = "") : Bits =  {
    val section: Range = fieldPtr + bc.value-1 downto fieldPtr
    val ret : Bits = genReservedHandler(bc)
    val newdoc = if(doc.isEmpty) "Reserved" else doc
    fields   += new Reserved("--", ret, section, newdoc)
    fieldPtr += bc.value
    ret
  }

  // RegDescr implementation
  def getName()        : String           = name
  def getAddr()        : Long             = addr
  def getDoc()         : String           = doc
  def getFieldDescrs() : List[FieldDescr] = getFields

  def accept(vs : MMSlaveFactoryVisitor) = {
      vs.visit(this)
  }
}
