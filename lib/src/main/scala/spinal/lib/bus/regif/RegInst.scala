package spinal.lib.bus.regif

import spinal.core.{Bits, _}
import spinal.lib.bus.misc.{AddressMapping, SingleMapping, SizeMapping}
import spinal.lib.{Flow, Stream}

import scala.collection.mutable.ListBuffer
import AccessType._
import spinal.lib.bus.bram.BRAM

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
  implicit def tans(x: Range) : Section = Section(x)
}

abstract class MappedBase(name: String, mapping: SizeMapping, doc: String, busif: BusIf) extends MemoryMappedDescriptor {
  protected var _name = name

  def getName(): String = _name

  def getAddr(): BigInt = mapping.base

  def getSize(): BigInt = mapping.size

  def getDoc(): String = doc

  def setName(name: String): MappedBase = {
    _name = name
    this
  }

  protected val hitRead: Bool = mapping.hit(busif.factory.readAddress())
  hitRead.setName(f"read_addr_decode_0x${mapping.lowerBound}%04x", weak = true)
  protected val hitDoRead: Bool = hitRead && busif.factory.readFire()
  hitDoRead.setName(f"read_fire_0x${mapping.lowerBound}%04x", weak = true)

  protected val hitWrite: Bool = mapping.hit(busif.factory.writeAddress())
  hitWrite.setName(f"write_addr_decode_0x${mapping.lowerBound}%04x", weak = true)
  protected val hitDoWrite: Bool = hitWrite && busif.factory.writeFire()
  hitDoWrite.setName(f"write_hit_0x${mapping.lowerBound}%04x", weak = true)

  def eventR(): Bool = {
    val event = Reg(Bool) init (False)
    event := hitDoRead
    event
  }

  def eventW(): Bool = {
    val event = Reg(Bool) init (False)
    event := hitDoWrite
    event
  }
}

case class RamInst(name: String, sizeMap: SizeMapping, doc: String, busif: BusIf) extends MappedBase(name,sizeMap, doc, busif) with RamDescr {
  private var Rerror: Boolean = false
  def readErrorTag = Rerror

  def hitRange(addr: UInt): Bool = {
    val hit = False
    when(addr >= sizeMap.base && addr < (sizeMap.base + sizeMap.size)){
      hit := True
    }
    hit
  }
}

class FIFOInst(name: String, addr: BigInt, doc:String, busif: BusIf) extends MappedBase(name,SizeMapping(addr, busif.wordAddressInc),doc,busif) with FifoDescr {
}

case class RegInst(name: String, addr: BigInt, doc: String, busif: BusIf) extends RegBase(name, addr, doc, busif) with RegDescr {

  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue = 0, doc = "")
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, doc: String)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue = 0, doc = doc)
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, resetValue:Long)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue, doc = "")
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, resetValue:Long , doc: String)(implicit symbol: SymbolName): T = {
    val sectionNext: Section = pos + hardType.getBitsWidth-1 downto pos
    val sectionExists: Section = fieldPtr downto 0
    val ret = pos match {
      case x if x < fieldPtr => SpinalError(s"next field section ${sectionNext} overlap to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= busif.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${busif.busDataWidth}")
      case x if (x == fieldPtr) => field(hardType, acc, resetValue, doc)
      case _ => {
        field(Bits(pos - fieldPtr bit), AccessType.NA)(SymbolName("reserved"))
        field(hardType, acc, resetValue, doc)
      }
    }
    fieldPtr = pos + hardType.getBitsWidth
    ret
  }

  @deprecated(message = "fieldAt(pos, Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType)(implicit symbol: SymbolName): Bits = fieldAt(pos, bc, acc, resetValue = 0, doc = "")(symbol)
  @deprecated(message = "fieldAt(pos, Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, doc: String)(implicit symbol: SymbolName): Bits = fieldAt(pos, bc, acc, resetValue = 0, doc = doc)(symbol)
  @deprecated(message = "fieldAt(pos, Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, resetValue: Long)(implicit symbol: SymbolName): Bits = fieldAt(pos, bc, acc, resetValue, doc = "")(symbol)
  @deprecated(message = "fieldAt(pos, Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, resetValue: Long, doc: String)(implicit symbol: SymbolName): Bits = {
    val sectionNext: Section = pos+bc.value-1 downto pos
    def sectionExists: Section = fieldPtr - 1 downto 0
    val ret = pos match {
      case x if x < fieldPtr => SpinalError(s"field Start Point ${x} conflict to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= busif.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${busif.busDataWidth}")
      case x if (x == fieldPtr) => field(bc, acc, resetValue, doc)
      case _ => {
        field(pos - fieldPtr bits, AccessType.NA)(SymbolName("reserved"))
        field(bc, acc, resetValue, doc)
      }
    }
    fieldPtr = pos + bc.value
    ret
  }

  def field[T <: BaseType](hardType: HardType[T], acc: AccessType)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue = 0, doc = "")
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, doc: String)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue = 0, doc = doc)
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, resetValue:Long)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue, doc = "")
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, resetValue:Long , doc: String)(implicit symbol: SymbolName): T = {
    val reg = acc match{
      case AccessType.NA => {
        val reg = hardType()
        reg.assignFromBits(B(0, reg.getBitsWidth bit))
        reg
      }
      case _ => {
        val reg = Reg(hardType)
        reg match {
          case t: Bool => t init(resetValue%2 == 1)
          case t: Bits => t.init(resetValue)
          case t: UInt => t.init(resetValue)
          case t: SInt => t.init(resetValue)
        }
        reg
      }
    }
    val signame = if(symbol.name.startsWith("<local ")){
      SpinalWarning("an unload signal created; `val signame = field(....)` is recomended instead `field(....)`")
      "unload"
    } else {
      symbol.name
    }
    reg.setName(signame, weak = true)
    registerInWithWriteLogic(reg, acc, resetValue, doc)
    reg
  }

  private def createReadErrorLogic(): Unit = {
    new Area {
      val hold = RegInit(False)
      val flag = hitDoRead || hold
      when(flag) {
        busif.factory.readError()
        hold.set()
      } elsewhen(hitDoWrite) {
        hold.clear()
      }
    }.setName("readError")
  }

  private def creatWriteLogic[T <: BaseType](reg: T, acc: AccessType, section: Range): Unit = {
    acc match {
      case AccessType.RO|AccessType.NA =>
      case _ => if(!reg.isRegOnAssign){
        SpinalError(s"$reg need be a register, not wire, check please")
      }
    }
    val preRError = Rerror
    acc match {
      case AccessType.RO    => _RO(reg)           //- W: no effect, R: no effect
      case AccessType.RW    => _W( reg, section)  //- W: as-is, R: no effect
      case AccessType.RC    => _RC(reg)           //- W: no effect, R: clears all bits
      case AccessType.RS    => _RS(reg)           //- W: no effect, R: sets all bits
      case AccessType.WRC   => _WRC(reg, section) //- W: as-is, R: clears all bits
      case AccessType.WRS   => _WRS(reg, section) //- W: as-is, R: sets all bits
      case AccessType.WC    => _WC(reg)           //- W: clears all bits, R: no effect
      case AccessType.WS    => _WS(reg)           //- W: sets all bits, R: no effect
      case AccessType.WSRC  => _WSRC(reg)         //- W: sets all bits, R: clears all bits
      case AccessType.WCRS  => _WCRS(reg)         //- W: clears all bits, R: sets all bits
      case AccessType.W1C   => _WB(reg, section, AccessType.W1C )   //- W: 1/0 clears/no effect on matching bit, R: no effect
      case AccessType.W1S   => _WB(reg, section, AccessType.W1S )   //- W: 1/0 sets/no effect on matching bit, R: no effect
      case AccessType.W1T   => _WB(reg, section, AccessType.W1T )   //- W: 1/0 toggles/no effect on matching bit, R: no effect
      case AccessType.W0C   => _WB(reg, section, AccessType.W0C )   //- W: 1/0 no effect on/clears matching bit, R: no effect
      case AccessType.W0S   => _WB(reg, section, AccessType.W0S )   //- W: 1/0 no effect on/sets matching bit, R: no effect
      case AccessType.W0T   => _WB(reg, section, AccessType.W0T )   //- W: 1/0 no effect on/toggles matching bit, R: no effect
      case AccessType.W1SRC => _WBR(reg, section, AccessType.W1SRC) //- W: 1/0 sets/no effect on matching bit, R: clears all bits
      case AccessType.W1CRS => _WBR(reg, section, AccessType.W1CRS) //- W: 1/0 clears/no effect on matching bit, R: sets all bits
      case AccessType.W0SRC => _WBR(reg, section, AccessType.W0SRC) //- W: 1/0 no effect on/sets matching bit, R: clears all bits
      case AccessType.W0CRS => _WBR(reg, section, AccessType.W0CRS) //- W: 1/0 no effect on/clears matching bit, R: sets all bits
      case AccessType.WO    => Rerror = true; _W( reg, section)    //- W: as-is, R: error
      case AccessType.WOC   => Rerror = true; _WC(reg)             //- W: clears all bits, R: error
      case AccessType.WOS   => Rerror = true; _WS(reg)             //- W: sets all bits, R: error
      case AccessType.W1    =>                _W1(reg, section)    //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
      case AccessType.WO1   => Rerror = true; _W1(reg, section)    //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
      case AccessType.NA    => NA(reg.getBitsWidth bit)            // -W: reserved, R: reserved
      case AccessType.W1P   => _WBP(reg, section, AccessType.W1P)  //- W: 1/0 pulse/no effect on matching bit, R: no effect
      case AccessType.W0P   => _WBP(reg, section, AccessType.W0P)  //- W: 0/1 pulse/no effect on matching bit, R: no effect
    }
    // Generate error read error
    if (Rerror && !preRError) {
      createReadErrorLogic()
    }
  }

  def registerInOnlyReadLogic[T <: BaseType](reg: T, acc: AccessType, resetValue:Long, doc: String): Unit = {
    registerInWithWriteLogic(reg, acc, resetValue, doc, dontCreatWriteLogic = true)
  }

  def registerAtOnlyReadLogic[T <: BaseType](pos: Int, reg: T, acc: AccessType, resetValue:Long, doc: String): Unit = {
    registerAtWithWriteLogic(pos, reg, acc, resetValue, doc, dontCreatWriteLogic = true)
  }

  def registerInWithWriteLogic[T <: BaseType](reg: T, acc: AccessType, resetValue:Long, doc: String, dontCreatWriteLogic: Boolean = false): Unit = {
    val section: Range = fieldPtr+reg.getBitsWidth-1 downto fieldPtr
    if(!dontCreatWriteLogic){
      creatWriteLogic(reg, acc, section)
    }
    val newdoc = if(doc.isEmpty && acc == AccessType.NA) "Reserved" else doc
    val nameRemoveNA = if(acc == AccessType.NA) "--" else reg.getName()
    reg.asBits
    fields   += Field(nameRemoveNA, reg, section, acc, resetValue, Rerror, newdoc)
    fieldPtr += reg.getBitsWidth
  }

  def registerAtWithWriteLogic[T<: BaseType](pos: Int, reg: T, acc: AccessType, resetValue:Long, doc: String, dontCreatWriteLogic: Boolean = false): Unit ={
    val sectionNext: Section = pos + reg.getBitsWidth-1 downto pos
    def sectionExists: Section = fieldPtr - 1 downto 0
    pos match {
      case x if x < fieldPtr => SpinalError(s"next field section ${sectionNext} overlap to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= busif.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${busif.busDataWidth}")
      case x if (x == fieldPtr) => registerInWithWriteLogic(reg, acc, resetValue, doc, dontCreatWriteLogic)
      case _ => {
        val reserved = B(0, pos - fieldPtr bit)
        registerInWithWriteLogic(reserved, AccessType.NA, 0, "Reserved", dontCreatWriteLogic)
        registerInWithWriteLogic(reg, acc, resetValue, doc, dontCreatWriteLogic)
      }
    }
    fieldPtr = pos + reg.getBitsWidth
  }

  @deprecated(message = "field(Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def field(bc: BitCount, acc: AccessType)(implicit symbol: SymbolName): Bits = field(bc, acc, resetValue = 0, doc = "")(symbol)
  @deprecated(message = "field(Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def field(bc: BitCount, acc: AccessType, doc: String)(implicit symbol: SymbolName): Bits = field(bc, acc, resetValue = 0, doc = doc)(symbol)
  @deprecated(message = "field(Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def field(bc: BitCount, acc: AccessType, resetValue: Long)(implicit symbol: SymbolName): Bits = field(bc, acc, resetValue, doc = "")(symbol)
  @deprecated(message = "field(Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def field(bc: BitCount, acc: AccessType, resetValue: Long, doc: String)(implicit symbol: SymbolName): Bits = {
    val section: Range = fieldPtr+bc.value-1 downto fieldPtr
    val preRError = Rerror
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
      case AccessType.W1P   => WBP(section, resetValue, AccessType.W1P )  //- W: 1/0 pulse/no effect on matching bit, R: no effect
      case AccessType.W0P   => WBP(section, resetValue, AccessType.W0P )  //- W: 0/1 pulse/no effect on matching bit, R: no effect
    }
    // Generate error read error
    if (Rerror && !preRError) {
      createReadErrorLogic()
    }

    val newdoc = if(doc.isEmpty && acc == AccessType.NA) "Reserved" else doc
    val signame = if(symbol.name.startsWith("<local ")){
      SpinalWarning("an unload signal created; `val signame = field(....)` is recomended instead `field(....)`")
      "unload"
    } else {
      symbol.name
    }
    val nameRemoveNA = if(acc == AccessType.NA) "--" else signame
    fields   += Field(nameRemoveNA, ret, section, acc, resetValue, Rerror, newdoc)
    fieldPtr += bc.value
    ret
  }

  def reserved(bc: BitCount): Bits = {
    field(bc, AccessType.NA)(SymbolName("reserved"))
  }

  // RegDescr implementation
  def getFieldDescrs() : List[FieldDescr] = getFields

  override def accept(vs : BusIfVisitor) = {
    insertLastNA()
    duplicateRenaming()
    vs.visit(this)
  }

  protected def duplicateRenaming() = {
    val counts = new scala.collection.mutable.HashMap[String, Int]()
    val ret = fields.zipWithIndex.map{case(fd, i) =>
      val name = fd.name
      val newname = if(counts.contains(name)){
        counts(name) += 1
        s"${name}${counts(name)}"
      } else {
        counts(name) = 0
        name
      }
      if(name != "--") {//dont touch RESERVED name
        fd.setName(newname)
      }
      fd
    }
    fields.clear()
    fields ++= ret
  }

  protected def insertLastNA(): Unit = {
    if (fields.isEmpty)
      fields += Field("--", null, 0 until busif.busDataWidth, AccessType.NA, 0, false, "Reserved")
    else if (fields.last.section.high != busif.busDataWidth-1)
      fields += Field("--", null, fields.last.section.high+1 until busif.busDataWidth, AccessType.NA, 0, false, "Reserved")
  }
}


abstract class RegBase(name: String, addr: BigInt, doc: String, busif: BusIf) extends MappedBase(name, SizeMapping(addr, busif.wordAddressInc), doc, busif) {
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

  def readErrorTag = Rerror
  def getFields = fields.toList

  protected def _RO[T <: BaseType](reg: T): T = busif.factory.read(reg, addr)

  protected def RO(bc: BitCount): Bits = _RO(Bits(bc))

  protected def _W1[T <: BaseType](reg: T, section: Range): T ={
    val hardResetFirstFlag = Reg(Bool()) init True
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    when(hitDoWrite && hardResetFirstFlag){
      reg.assignFromBits(writeData)
      hardResetFirstFlag.clear()
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def W1(bc: BitCount, section: Range, resetValue: Long): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    _W1(ret, section)
  }

  protected def _W[T <: BaseType](reg: T, section: Range): T ={
    busif.factory.write(reg, addr, section.low)
    busif.factory.read(reg, addr, section.low)
  }

  protected def W(bc: BitCount, section: Range, resetValue: Long ): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    _W(ret, section)
  }

  protected def _WO[T <: BaseType](reg: T, section: Range): T = {
    busif.factory.write(reg, addr, section.low)

  }

  protected def _RC[T <: BaseType](reg: T): T = {
    when(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).clearAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def RC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _RC(ret)
  }

  protected def _RS[T <: BaseType](reg: T): T = {
    when(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).setAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def RS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _RS(ret)
  }

  protected def _WRC[T <: BaseType](reg: T, section: Range): T = {
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    when(hitDoWrite){
      reg.assignFromBits(writeData)
    }.elsewhen(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).clearAll())
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def WRC(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WRC(ret, section)
  }

  protected def _WRS[T <: BaseType](reg: T, section: Range): T = {
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    when(hitDoWrite){
      reg.assignFromBits(writeData)
    }.elsewhen(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).setAll())
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def WRS(bc: BitCount, section: Range, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WRS(ret, section)
  }

  protected def _WC[T <: BaseType](reg: T): T = {
    when(hitDoWrite){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).clearAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def WC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WC(ret)
  }

  protected def _WS[T <: BaseType](reg: T): T = {
    when(hitDoWrite){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).setAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def WS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WS(ret)
  }

  protected def _WSRC[T <: BaseType](reg: T): T = {
    when(hitDoWrite){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).setAll())
    }.elsewhen(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).clearAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def WSRC(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WSRC(ret)
  }

  protected def _WCRS[T <: BaseType](reg: T): T = {
    when(hitDoWrite){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).clearAll())
    }.elsewhen(hitDoRead){
      reg.assignFromBits(Bits(reg.getBitsWidth bit).setAll())
    }
    busif.factory.read(reg, addr)
  }

  protected def WCRS(bc: BitCount, resetValue: Long): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    _WCRS(ret)
  }

  protected def _WB[T <: BaseType](reg: T, section: Range, accType: AccessType): T = {
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    when(hitDoWrite){
      section.reverse.map(_ - section.min).foreach{ i =>
        val regbit = reg match {
          case t: Bool => require(section.size == 1); t
          case t: BitVector => t(i)
        }
        val x = i + section.min
        accType match {
          case AccessType.W1C => when( writeData(x)){regbit.clear()   }
          case AccessType.W1S => when( writeData(x)){regbit.set()     }
          case AccessType.W1T => when( writeData(x)){regbit := ~regbit}
          case AccessType.W0C => when(~writeData(x)){regbit.clear()   }
          case AccessType.W0S => when(~writeData(x)){regbit.set()     }
          case AccessType.W0T => when(~writeData(x)){regbit := ~regbit}
          case _ =>
        }
      }
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def WB(section: Range, resetValue: Long, accType: AccessType): Bits = {
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    _WB(ret, section, accType)
  }

  protected def _WBR[T <: BaseType](reg: T, section: Range, accType: AccessType): T ={
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    section.reverse.map(_ - section.min).foreach { i =>
      val regbit = reg match {
        case t: Bool => require(section.size == 1); t
        case t: BitVector => t(i)
      }
      val x = i + section.min
      accType match {
        case AccessType.W1SRC => {
          when(hitDoWrite && writeData(x)) {regbit.set()}
            .elsewhen(hitDoRead)           {regbit.clear()}
        }
        case AccessType.W1CRS => {
          when(hitDoWrite && writeData(x)) {regbit.clear()}
            .elsewhen(hitDoRead)           {regbit.set()}
        }
        case AccessType.W0SRC => {
          when(hitDoWrite && ~writeData(x)){regbit.set()}
            .elsewhen(hitDoRead)           {regbit.clear()}
        }
        case AccessType.W0CRS => {
          when(hitDoWrite && ~writeData(x)){regbit.clear()}
            .elsewhen(hitDoRead)           {regbit.set()}
        }
        case _ =>
      }
      reg
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def WBR(section: Range, resetValue: Long, accType: AccessType): Bits ={
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    _WBR(ret, section, accType)
  }

  protected def _WBP[T <: BaseType](reg: T, section: Range, accType: AccessType): T ={
    val writeData = busif.factory.nonStopWrite(Bits(reg.getBitsWidth bit), section.low)
    section.reverse.map(_ - section.min).foreach { i =>
      val regbit = reg match {
        case t: Bool => require(section.size == 1); t
        case t: BitVector => t(i)
      }
      val x = i + section.min
      accType match {
        case AccessType.W1P => {
          when(hitDoWrite &&  writeData(x)){regbit := ~regbit}
            .otherwise{regbit := False}
        }
        case AccessType.W0P => {
          when(hitDoWrite && ~writeData(x)){regbit := ~regbit}
            .otherwise{regbit := False}
        }
      }
    }
    busif.factory.read(reg, addr, section.low)
  }

  protected def WBP(section: Range, resetValue: Long, accType: AccessType): Bits ={
    val resetValues = B(resetValue)
    val ret = Reg(Bits(section.size bits)) init resetValues
    _WBP(ret, section, accType)
  }

  protected def NA(bc: BitCount): Bits = {
    Bits(bc).clearAll()
  }
}
