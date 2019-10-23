package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.IMasterSlave

import scala.collection.mutable.ListBuffer

class Section(val max: Int, val min: Int){
  override def toString(): String = s"[${this.max}:${this.min}]"
}

object Section{
  def apply(x: Range): Section = new Section(x.max, x.min)
  implicit def tans(x: Range) = Section(x)
}


case class RegInst(name: String, addr: Long, doc: String, busif: BusIfAdapter) {
  val fields = ListBuffer[Field]()
  private var fieldPtr: Int = 0
  private var Rerror: Boolean = false

  def readErrorTag = Rerror

  val hitRead  = busif.readAddress === U(addr)
  val hitWrite = busif.writeAddress === U(addr)
  val hitDoRead  = hitRead && busif.doRead
  val hitDoWrite = hitWrite && busif.doWrite

  def readBits: Bits = {
    fields.map(_.hardbit).reverse.foldRight(Bits(0 bit))((x,y) => x ## y) //TODO
  }

  def checkLast={
    val spareNumbers = if(fields.isEmpty) busif.busDataWidth else busif.busDataWidth-1 - fields.last.tailBitPos
    spareNumbers match {
      case x if x > 0 => field(x bits, AccessType.NA)(SymbolName("reserved"))
      case x if x < 0 => SpinalError(s"Range ${Section(fields.last.section)} exceed Bus width ${busif.busDataWidth}")
      case _ =>
    }
  }

  def allIsNA: Boolean = {
    checkLast
    fields.map(_.accType == AccessType.NA).foldLeft(true)(_&&_)
  }

  def fieldoffset(offset: Int, bc: BitCount, acc: AccessType, resetValue: Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val sectionNext: Section = offset+bc.value-1 downto offset
    val sectionExists: Section = fieldPtr downto 0
    val ret = offset match {
      case x if x < fieldPtr => SpinalError(s"field Start Point ${x} conflict to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= busif.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${busif.busDataWidth}")
      case x if (x == fieldPtr) => field(bc, acc, resetValue, doc)
      case _ => {
        field(offset - fieldPtr bits, AccessType.NA)(SymbolName("reserved"))
        field(bc, acc, resetValue, doc)
      }
    }
    fieldPtr = offset
    ret
  }

  def field(bc: BitCount, acc: AccessType, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val section: Range = fieldPtr+bc.value-1 downto fieldPtr
    val ret: Bits = acc match {
      case AccessType.RO    => RO(bc)                       //- W: no effect, R: no effect
      case AccessType.RW    => W( bc, section, resetValue)  //- W: as-is, R: no effect
      case AccessType.RC    => RC(bc, resetValue)           //- W: no effect, R: clears all bits
      case AccessType.RS    => RS(bc, resetValue)           //- W: no effect, R: sets all bits
      case AccessType.WRC   => WRC(bc, section, resetValue) //- W: as-is, R: clears all bits
      case AccessType.WRS   => WRS(bc, section, resetValue) //- W: as-is, R: sets all bits
      case AccessType.WC    => WC(bc, resetValue)           //- W: clears all bits, R: no effect
      case AccessType.WS    => WS(bc, resetValue)           //- W: sets all bits, R: no effect
      case AccessType.WSRC  => WSRC(bc, resetValue)         //- W: sets all bits, R: clears all bits
      case AccessType.WCRS  => WCRS(bc, resetValue)         //- W: clears all bits, R: sets all bits
      case AccessType.W1C   => WB(section, resetValue, AccessType.W1C )   //- W: 1/0 clears/no effect on matching bit, R: no effect
      case AccessType.W1S   => WB(section, resetValue, AccessType.W1S )   //- W: 1/0 sets/no effect on matching bit, R: no effect
      case AccessType.W1T   => WB(section, resetValue, AccessType.W1T )   //- W: 1/0 toggles/no effect on matching bit, R: no effect
      case AccessType.W0C   => WB(section, resetValue, AccessType.W0C )   //- W: 1/0 no effect on/clears matching bit, R: no effect
      case AccessType.W0S   => WB(section, resetValue, AccessType.W0S )   //- W: 1/0 no effect on/sets matching bit, R: no effect
      case AccessType.W0T   => WB(section, resetValue, AccessType.W0T )   //- W: 1/0 no effect on/toggles matching bit, R: no effect
      case AccessType.W1SRC => WBR(section, resetValue, AccessType.W1SRC) //- W: 1/0 sets/no effect on matching bit, R: clears all bits
      case AccessType.W1CRS => WBR(section, resetValue, AccessType.W1CRS) //- W: 1/0 clears/no effect on matching bit, R: sets all bits
      case AccessType.W0SRC => WBR(section, resetValue, AccessType.W0SRC) //- W: 1/0 no effect on/sets matching bit, R: clears all bits
      case AccessType.W0CRS => WBR(section, resetValue, AccessType.W0CRS) //- W: 1/0 no effect on/clears matching bit, R: sets all bits
      case AccessType.WO    => Rerror = true; W( bc, section, resetValue) //- W: as-is, R: error
      case AccessType.WOC   => Rerror = true; WC(bc, resetValue)          //- W: clears all bits, R: error
      case AccessType.WOS   => Rerror = true; WS(bc, resetValue)          //- W: sets all bits, R: error
      case AccessType.W1    =>                W1(bc, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
      case AccessType.WO1   => Rerror = true; W1(bc, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
      case AccessType.NA    => NA(bc)                                     // -W: reserved, R: reserved
    }
    val newdoc = if(doc.isEmpty && acc == AccessType.NA) "Reserved" else doc
    val nameRemoveNA = if(acc == AccessType.NA) "--" else symbol.name
    fields   += Field(nameRemoveNA, ret, section, acc, resetValue, Rerror, newdoc)
    fieldPtr += bc.value
    ret
  }

  private def RO(bc: BitCount): Bits = Bits(bc)

  private def W1(bc: BitCount, section: Range, resetValue: Long ): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    val hardRestFirstFlag = Reg(Bool()) init True
    when(hitDoWrite && hardRestFirstFlag){
      ret := busif.writeData(section)
      hardRestFirstFlag.clear()
    }
    ret
  }

  private def W(bc: BitCount, section: Range, resetValue: Long ): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.writeData(section)
    }
    ret
  }

  private def RC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoRead){
      ret.clearAll()
    }
    ret
  }

  private def RS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret.setAll()
    }
    ret
  }

  private def WRC(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.writeData(section)
    }.elsewhen(hitDoRead){
      ret.clearAll()
    }
    ret
  }

  private def WRS(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.writeData(section)
    }.elsewhen(hitDoRead){
      ret.setAll()
    }
    ret
  }

  private def WC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret.clearAll()
    }
    ret
  }

  private def WS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret.clearAll()
    }
    ret
  }

  private def WSRC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret.setAll()
    }.elsewhen(hitDoRead){
      ret.clearAll()
    }
    ret
  }

  private def WCRS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret.clearAll()
    }.elsewhen(hitDoRead){
      ret.setAll()
    }
    ret
  }

  private def WB(section: Range, resetValue: Long, accType: AccessType): Bits = {
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    when(hitDoWrite){
      for(x <- section) {
        val idx = x - section.min
        accType match {
          case AccessType.W1C => when( busif.writeData(x)){ret(idx).clear()}
          case AccessType.W1S => when( busif.writeData(x)){ret(idx).set()  }
          case AccessType.W1T => when( busif.writeData(x)){ret(idx) := ~ret(idx)}
          case AccessType.W0C => when(~busif.writeData(x)){ret(idx).clear()}
          case AccessType.W0S => when(~busif.writeData(x)){ret(idx).set()  }
          case AccessType.W0T => when(~busif.writeData(x)){ret(idx) := ~ret(idx)}
          case _ =>
        }
      }
    }
    ret
  }

  private def WBR(section: Range, resetValue: Long, accType: AccessType): Bits ={
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    for(x <- section) {
      val idx = x - section.min
      accType match {
        case AccessType.W1SRC => {
          when(hitDoWrite && busif.writeData(x)) {ret(idx).set()}
            .elsewhen(hitDoRead)                 {ret(idx).clear()}
        }
        case AccessType.W1CRS => {
          when(hitDoWrite && busif.writeData(x)) {ret(idx).clear()}
            .elsewhen(hitDoRead)                 {ret(idx).set()}
        }
        case AccessType.W0SRC => {
          when(hitDoWrite && ~busif.writeData(x)){ret(idx).set()}
            .elsewhen(hitDoRead)                 {ret(idx).clear()}
        }
        case AccessType.W0CRS => {
          when(hitDoWrite && ~busif.writeData(x)){ret(idx).clear()}
            .elsewhen(hitDoRead)                 {ret(idx).set()}
        }
        case _ =>
      }
    }
    ret
  }

  private def NA(bc: BitCount): Bits = {
    Bits(bc).clearAll()
  }

  def reserved(bc: BitCount): Bits = {
    field(bc, AccessType.NA)(SymbolName("reserved"))
  }
}
