package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ListBuffer
import spinal.lib._

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
  protected var werror: Boolean = false

  def readErrorTag = rerror
  def writeErrorTag = werror
  def getFields = fields.toList

  def getAddress : Long = addr

  val hitRead  = bus.readAddress === U(addr)
  val hitWrite = bus.writeAddress === U(addr)
  val hitDoRead  = hitRead && bus.readResp
  val hitDoWrite = hitWrite && bus.writeResp

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

  def onReadReq() : Unit = {
    bus.readAccept()
  }

  def onWriteReq() : Unit = {
    bus.writeAccept()
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

  def addField[T <: Data](that: T, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Unit = {
    val section : Range = fieldPtr + that.getBitsWidth-1 downto fieldPtr
    val ret : Bits = genDataHandler(that, section, resetValue)
    fields   += new Field(symbol.name, ret, section, resetValue, rerror, doc)
    fieldPtr += that.getBitsWidth
  }

  def newField(bc : BitCount, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val data : Bits = Bits(bc)
    data := B(resetValue)
    addField(data, resetValue, doc)
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
    addField(data, resetValue, doc)(symbol)
    data.setName(s"mmslave_field_${symbol.name}")
    data
  }

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    assert(that.isReg)
    that.removeAssignments()
    when(hitDoWrite) {
      that.assignFromBits(bus.writeData(section))
    }
    that.asBits
  }

  override def onReadReq(): Unit = {
    bus.readAccept()
    bus.readRespond(readBits, false)
  }

  override def onWriteReq(): Unit = {
    bus.writeAccept()
    bus.writeRespond(false)
  }
}

class ReadOnlyEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.asBits
  }

  override def onReadReq(): Unit = {
    bus.readAccept()
    bus.readRespond(readBits, false)
  }

  override def onWriteReq(): Unit = {
    bus.writeRespond(true)
  }

  override def getAccess() : String = "RO"
}

class WriteOnlyRegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegEntry(name, addr, doc, bus) with RegDescr {

  override def readBits: Bits = {
    val read : Bits = Bits(bus.busDataWidth bits)
    read.clearAll()
    read
  }

  override def onReadReq(): Unit = {
    val data : Bits = Bits(bus.busDataWidth bits)
    data := 0x0l
    bus.readAccept()
    bus.readRespond(data, true)
  }

  override def getAccess() : String = "WO"
}

abstract class StreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {
  def newStreamField(bc : BitCount, resetValue : Long = 0, doc: String = "")(implicit symbol: SymbolName): Stream[Bits]

  override def newField(bc : BitCount, resetValue : Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val data : Bits = 0xdeadbeefl
    assert(false, "newField not implemented, use newStreamField!");
    data
  }
}

class WriteStreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends StreamEntry(name, addr, doc, bus) {
  val ready = Bool()
  
  override def readBits: Bits = {
    val read : Bits = Bits(bus.busDataWidth bits)
    read.clearAll()
    read
  }
  
  override def onWriteReq() : Unit = {
    when(ready) {
      bus.writeAccept()
      bus.writeRespond(false)
    }
  }

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.assignFromBits(bus.writeData(section))
    that.asBits
  }
  
  def newStreamField(bc : BitCount, resetValue : Long = 0, doc: String = "")(implicit symbol: SymbolName): Stream[Bits] = {
    val stream : Stream[Bits] = Stream(Bits(bc))
    val out = stream.stage()
    addField(stream.payload, resetValue, doc)(symbol)
    stream.payload.setName(s"mmslave_${symbol.name}_payload")
    stream.valid.setName(s"mmslave_${symbol.name}_valid")
    stream.ready.setName(s"mmslave_${symbol.name}_ready")
    stream.valid := hitDoWrite
    ready := stream.ready
    out
  }

  override def getAccess() : String = "WO"
}

class ReadStreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends StreamEntry(name, addr, doc, bus) {
  val valid = Bool()

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.asBits
  }
  
  override def onReadReq() : Unit = {
    when(valid) {
      bus.readAccept()
      bus.readRespond(readBits, false)
    }
  }

  override def onWriteReq() : Unit = {
      bus.writeAccept()
      bus.writeRespond(true)
  }
  
  def newStreamField(bc : BitCount, resetValue : Long = 0, doc: String = "")(implicit symbol: SymbolName): Stream[Bits] = {
    val stream : Stream[Bits] = Stream(Bits(bc))
    val out = stream.stage()
    addField(out.payload.getDrivingReg, resetValue, doc)(symbol)
    out.payload.setName(s"mmslave_streamfield_${symbol.name}")
    out.ready := hitRead && bus.readReq
    valid := out.valid
    stream
  }

  override def getAccess() : String = "RO"
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