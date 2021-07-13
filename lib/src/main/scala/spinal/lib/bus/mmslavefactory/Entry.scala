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

  // Goes high when the current read/write request is accepted. That means that
  // the request can be served.
  val readAccept = Bool()
  val writeAccept = Bool()

  def readErrorTag = rerror
  def writeErrorTag = werror
  def getFields = fields.toList
  def getAddress : Long = addr

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
    readAccept
  }
  
  def eventW() : Bool = {
    writeAccept
  }

  final def onReadReqIntern() : Unit = {
    onReadReq()
  }

  final def onWriteReqIntern() : Unit = {
    onWriteReq()
  }

  readAccept := False
  def onReadReq() : Unit = {
    readAccept := True
    bus.readAccept()
  }

  writeAccept := False
  def onWriteReq() : Unit = {
    writeAccept := True
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

  def addFieldEx[T <: Data](startIndex: Int, name: String, that: T, resetValue:Long = 0, doc: String = ""): Unit = {
    if(startIndex < fieldPtr)
      new Exception("Bits already occupied.")
    if(startIndex > fieldPtr)
      reserved((startIndex - fieldPtr) bits, "Reserved")
    val section : Range = fieldPtr + that.getBitsWidth-1 downto fieldPtr
    val ret : Bits = genDataHandler(that, section, resetValue)
    fields   += new Field(name, ret, section, resetValue, rerror, doc)
    fieldPtr += that.getBitsWidth
  }

  def addField[T <: Data](name: String, that: T, resetValue:Long = 0, doc: String = ""): Unit = {
    addFieldEx(fieldPtr, name, that, resetValue, doc)
  }

  def newFieldEx(startIndex: Int, name: String, bc : BitCount, resetValue:Long = 0, doc: String = ""): Bits = {
    val data : Bits = Bits(bc)
    data := B(resetValue)
    addFieldEx(startIndex, name, data, resetValue, doc)
    data
  }

  def newField(name: String, bc : BitCount, resetValue:Long = 0, doc: String = ""): Bits = {
    newFieldEx(fieldPtr, name, bc, resetValue, doc)
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
    event := readAccept
    event
  }

  override def eventW() : Bool = {
    val event = Reg(Bool) init(False)
    event := writeAccept
    event
  }

  override def newFieldEx(startIndex: Int, name: String, bc : BitCount, resetValue : Long = 0, doc: String = ""): Bits = {
    val data : Bits = Reg(Bits(bc)) init(resetValue)
    addFieldEx(startIndex, name, data, resetValue, doc)
    data.setName(s"mmslave_${this.name}_${name}")
    data
  }

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    assert(that.isReg)
    that.removeAssignments()
    when(writeAccept) {
      that.assignFromBits(bus.writeData(section))
    }
    that.asBits
  }

  override def onReadReq(): Unit = {
    readAccept := True
    bus.readAccept()
    bus.readRespond(readBits, false)
  }

  override def onWriteReq(): Unit = {
    writeAccept := True
    bus.writeAccept()
    bus.writeRespond(false)
  }
}

class ReadOnlyEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.asBits
  }

  override def onReadReq(): Unit = {
    readAccept := True
    bus.readAccept()
    bus.readRespond(readBits, false)
  }

  override def onWriteReq(): Unit = {
    writeAccept := True
    bus.writeAccept()
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
    readAccept := True
    bus.readAccept()
    bus.readRespond(data, true)
  }

  override def getAccess() : String = "WO"
}

abstract class StreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends Entry(name, addr, doc, bus) with RegDescr {
  def newStreamField(name: String, bc : BitCount, resetValue : Long = 0, doc: String = ""): Stream[Bits]

  override def newFieldEx(startIndex: Int, name: String, bc : BitCount, resetValue : Long = 0, doc: String = ""): Bits = {
    val data : Bits = 0xdeadbeefl
    assert(false, "newField not implemented, use newStreamField!");
    data
  }
}

class WriteStreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends StreamEntry(name, addr, doc, bus) {
  val valid = Bool()
  val ready = Bool()
  
  override def readBits: Bits = {
    val read : Bits = Bits(bus.busDataWidth bits)
    read.clearAll()
    read
  }
  
  valid := False
  override def onWriteReq() : Unit = {
    valid := True
    when(ready) {
      writeAccept := True
      bus.writeAccept()
      bus.writeRespond(false)
    }
  }

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.assignFromBits(bus.writeData(section))
    that.asBits
  }
  
  def newStreamField(name: String, bc : BitCount, resetValue : Long = 0, doc: String = ""): Stream[Bits] = {
    val stream : Stream[Bits] = Stream(Bits(bc))
    val out = stream.stage()
    addField(name, stream.payload, resetValue, doc)
    stream.payload.setName(s"mmslave_${this.name}_${name}_payload")
    stream.valid.setName(s"mmslave_${this.name}_${name}_valid")
    stream.ready.setName(s"mmslave_${this.name}_${name}_ready")
    stream.valid := valid
    ready := stream.ready
    out
  }

  override def getAccess() : String = "WO"
}

class ReadStreamEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends StreamEntry(name, addr, doc, bus) {
  val valid = Bool()
  val ready = Bool()

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    that.asBits
  }
  
  ready := False
  override def onReadReq() : Unit = {
    when(valid) {
      readAccept := True
      ready := True
      bus.readAccept()
      bus.readRespond(readBits, false)
    }
  }

  override def onWriteReq() : Unit = {
      writeAccept := True
      bus.writeAccept()
      bus.writeRespond(true)
  }
  
  def newStreamField(name: String, bc : BitCount, resetValue : Long = 0, doc: String = ""): Stream[Bits] = {
    val stream : Stream[Bits] = Stream(Bits(bc))
    val out = stream.stage()
    addField(name, out.payload.getDrivingReg, resetValue, doc)
    out.payload.setName(s"mmslave_${this.name}_${name}_payload")
    out.valid.setName(s"mmslave_${this.name}_${name}_valid")
    out.ready.setName(s"mmslave_${this.name}_${name}_ready")
    out.ready := ready
    valid := out.valid
    stream
  }

  override def getAccess() : String = "RO"
}

class ClearRegEntry(name: String, addr: Long, doc: String, bus: MMSlaveFactory) extends RegEntry(name, addr, doc, bus) with RegDescr {

  override def genDataHandler[T <: Data](that: T, section: Range, resetValue: Long): Bits = {
    assert(that.isReg)
    when(writeAccept) {
      that.assignFromBits((bus.writeData(section) & that.asBits) ^ that.asBits)
    }
    that.asBits
  }

  override def getAccess() : String = "C"
}