package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ListBuffer
import AccessType._

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


case class RamInst(name: String, sizeMap: SizeMapping, doc: String, busif: BusIf) extends RamDescr {
  private var Rerror: Boolean = false
  def readErrorTag = Rerror

  def hitRange(addr: UInt): Bool = {
    val hit = False
    when(addr >= sizeMap.base && addr < (sizeMap.base + sizeMap.size)){
      hit := True
    }
    hit
  }

  val hitRead  = hitRange(busif.readAddress)
  val hitWrite = hitRange(busif.writeAddress)
  val hitDoRead  = hitRead && busif.doRead
  val hitDoWrite = hitWrite && busif.doWrite

  // RamDescr implementation
  def getName()        : String = name
  def getDoc()         : String = doc
  def getAddr()        : BigInt = sizeMap.base
  def getSize()        : BigInt = sizeMap.size

}

class FIFOInst(name: String, addr: BigInt, doc:String, busif: BusIf) extends RegBase(name,addr,doc,busif) with FifoDescr {

  // FifoDescr implementation
  def getAddr()        : BigInt = addr
  def getSize()        : BigInt = busif.wordAddressInc
  def getDoc()         : String = doc
  def setName(name: String): FIFOInst = {
    _name = name
    this
  }
}

case class RegInst(name: String, addr: BigInt, doc: String, busif: BusIf) extends RegBase(name, addr, doc, busif) with RegDescr {
  def setName(name: String): RegInst = {
    _name = name
    this
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

  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue = 0, doc = "")(symbol)
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, doc: String)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue = 0, doc = doc)(symbol)
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, resetValue:BigInt)(implicit symbol: SymbolName): T = fieldAt(pos, hardType, acc, resetValue, doc = "")(symbol)
  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], acc: AccessType, resetValue:BigInt , doc: String)(implicit symbol: SymbolName): T = {
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
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, resetValue: BigInt)(implicit symbol: SymbolName): Bits = fieldAt(pos, bc, acc, resetValue, doc = "")(symbol)
  @deprecated(message = "fieldAt(pos, Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, resetValue: BigInt, doc: String)(implicit symbol: SymbolName): Bits = {
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

  def field[T <: BaseType](hardType: HardType[T], acc: AccessType)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue = 0, doc = "")(symbol)
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, doc: String)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue = 0, doc = doc)(symbol)
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, resetValue:BigInt)(implicit symbol: SymbolName): T = field(hardType, acc, resetValue, doc = "")(symbol)
  def field[T <: BaseType](hardType: HardType[T], acc: AccessType, resetValue:BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val reg = acc match{
      case AccessType.NA => {
        val reg = hardType()
        reg.clearAll()
        reg
      }
      case AccessType.ROV => {
        B(resetValue, hardType().getBitsWidth bit).asInstanceOf[T]
      }
      case AccessType.RO => {
        hardType()
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
    (acc, symbol.name.startsWith("<local")) match {
      case (`ROV`, _) =>
      case (_, true)  =>{
        SpinalWarning("an unload signal created; `val signame = field(....)` is recomended instead `field(....)`")
        reg.setName("unload", weak = true)
      }
      case (_, false) =>reg.setName(symbol.name, weak = true)
    }
    registerInWithWriteLogic(reg, acc, resetValue, doc)
    reg
  }

  /*
  * Always used on more than one address shared one regsiter instance
  * Example:
  * val REG0 = busif.newReg(doc="Share Clock EnableRegiste  RW   address")
  * val REG1 = busif.newReg(doc="Share Clock EnableRegiste  W1S  address")
  * val REG2 = busif.newReg(doc="Share Clock EnableRegister W1C  address")
  * val REG3 = busif.newReg(doc="Share Clock EnableRegister read address")
  * val reg32bit = REG0.field(Bits(32 bit), RW , 0xffff, "clock enable reg RW")
  *                REG1.parasiteField(reg32bit,     W1S, 0xffff, "clock enable reg write 1 set ")
  *                REG2.parasiteField(reg32bit,     W1C, 0xffff, "clock enable reg write 1 clear")
  * val regwire  = REG3.field(Bits(32 bit), RO , 0xffff, "clock enable read only")
  * regwire := reg32bit
  * */
  def parasiteField[T <: BaseType](reg: T, acc: AccessType, resetValue: BigInt, doc: String): Unit = {
    assert(reg.isReg, "prasiteField should be register only, check please")
    registerInWithWriteLogic(reg, acc, resetValue, doc)
  }
  def parasiteFieldAt[T <: BaseType](pos: Int, reg: T, acc: AccessType, resetValue: BigInt, doc: String): Unit = {
    assert(reg.isReg, "prasiteField should be register only, check please")
    registerAtWithWriteLogic(pos, reg, acc, resetValue, doc)
  }

  def fieldHSRW[T <: BaseType](seten: Bool, setval: T)(implicit symbol: SymbolName): T = fieldHSRW(seten, setval, resetValue = 0, doc = "")(symbol)
  def fieldHSRW[T <: BaseType](seten: Bool, setval: T, resetValue:BigInt)(implicit symbol: SymbolName): T = fieldHSRW(seten, setval, resetValue, doc = "")(symbol)
  def fieldHSRW[T <: BaseType](seten: Bool, setval: T, resetValue:BigInt , doc: String)(implicit symbol: SymbolName): T = {
    val reg = field(hardType = cloneOf(setval), acc = AccessType.HSRW, resetValue = resetValue, doc = doc)(symbol = symbol)
    when(seten){
      reg := setval
    }
    reg
  }

  def fieldHSRWAt[T <: BaseType](pos: Int, seten: Bool, setval: T)(implicit symbol: SymbolName): T = fieldHSRWAt(pos, seten, setval, resetValue = 0, doc = "")(symbol)
  def fieldHSRWAt[T <: BaseType](pos: Int, seten: Bool, setval: T, resetValue:BigInt)(implicit symbol: SymbolName): T = fieldHSRWAt(pos, seten, setval, resetValue, doc = "")(symbol)
  def fieldHSRWAt[T <: BaseType](pos: Int, seten: Bool, setval: T, resetValue:BigInt , doc: String)(implicit symbol: SymbolName): T = {
    val reg = fieldAt(pos, hardType = cloneOf(setval), acc = AccessType.HSRW, resetValue = resetValue, doc = doc)(symbol = symbol)
    when(seten){
      reg := setval
    }
    reg
  }

  private def reginit[T <: BaseType](reg: T, resetValue: BigInt): T = {
    reg match {
      case t: Bool => t init (resetValue % 2 == 1)
      case t: Bits => t.init(resetValue)
      case t: UInt => t.init(resetValue)
      case t: SInt => t.init(resetValue)
    }
    reg
  }

  private def setname[T <: BaseType](reg: T, symbol: SymbolName) = {
    symbol.name.startsWith("<local") match {
      case true => {
        SpinalWarning("an unload signal created; `val signame = field(....)` is recomended instead `field(....)`")
        reg.setName("unload", weak = true)
      }
      case false => reg.setName(symbol.name, weak = true)
    }
  }

  def fieldRWHS[T <: BaseType](seten: Bool, setval: T)(implicit symbol: SymbolName): T = fieldRWHS(seten, setval, resetValue = 0, doc = "")(symbol)
  def fieldRWHS[T <: BaseType](seten: Bool, setval: T, resetValue: BigInt)(implicit symbol: SymbolName): T = fieldRWHS(seten, setval, resetValue, doc = "")(symbol)
  def fieldRWHS[T <: BaseType](seten: Bool, setval: T, resetValue: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val reg = reginit(cloneOf(setval).setAsReg(), resetValue)
    setname(reg, symbol)
    when(seten) {
      reg := setval
    }
    registerInWithWriteLogic(reg, AccessType.RWHS, resetValue, doc)
    reg
  }

  def fieldRWHSAt[T <: BaseType](pos: Int, seten: Bool, setval: T)(implicit symbol: SymbolName): T = fieldRWHSAt(pos, seten, setval, resetValue = 0, doc = "")(symbol)
  def fieldRWHSAt[T <: BaseType](pos: Int, seten: Bool, setval: T, resetValue: BigInt)(implicit symbol: SymbolName): T = fieldRWHSAt(pos, seten, setval, resetValue, doc = "")(symbol)
  def fieldRWHSAt[T <: BaseType](pos: Int, seten: Bool, setval: T, resetValue:BigInt , doc: String)(implicit symbol: SymbolName): T = {
    val reg = reginit(cloneOf(setval).setAsReg(), resetValue)
    setname(reg, symbol)
    when(seten){
      reg := setval
    }
    registerAtWithWriteLogic(pos, reg, AccessType.RWHS, resetValue, doc)
    reg
  }

  private def creatWriteLogic[T <: BaseType](reg: T, acc: AccessType, section: Range): Unit = {
    acc match {
      case AccessType.RO|AccessType.ROV|AccessType.NA =>
      case _ => if(!reg.isRegOnAssign){
        SpinalError(s"$reg need be a register, not wire, check please")
      }
    }
    acc match {
      case AccessType.RO    => _RO(reg)           //- W: no effect, R: no effect
      case AccessType.ROV   => _RO(reg)           //- ReadOnlyValue, used for constant like device-ID/hw-Version
      case AccessType.RW    => _W( reg, section)  //- W: as-is, R: no effect
      case AccessType.RC    => _RC(reg, section)  //- W: no effect, R: clears all bits
      case AccessType.RS    => _RS(reg, section)  //- W: no effect, R: sets all bits
      case AccessType.WRC   => _WRC(reg, section) //- W: as-is, R: clears all bits
      case AccessType.WRS   => _WRS(reg, section) //- W: as-is, R: sets all bits
      case AccessType.WC    => _WC(reg, section)  //- W: clears all bits, R: no effect
      case AccessType.WS    => _WS(reg, section)  //- W: sets all bits, R: no effect
      case AccessType.WSRC  => _WSRC(reg, section)//- W: sets all bits, R: clears all bits
      case AccessType.WCRS  => _WCRS(reg, section)//- W: clears all bits, R: sets all bits
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
      case AccessType.WOC   => Rerror = true; _WC(reg, section)    //- W: clears all bits, R: error
      case AccessType.WOS   => Rerror = true; _WS(reg, section)    //- W: sets all bits, R: error
      case AccessType.W1    =>                _W1(reg, section)    //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
      case AccessType.WO1   => Rerror = true; _W1(reg, section)    //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
      case AccessType.NA    => NA(reg.getBitsWidth bit)            // -W: reserved, R: reserved
      case AccessType.W1P   => _WBP(reg, section, AccessType.W1P)  //- W: 1/0 pulse/no effect on matching bit, R: no effect
      case AccessType.W0P   => _WBP(reg, section, AccessType.W0P)  //- W: 0/1 pulse/no effect on matching bit, R: no effect
      case AccessType.HSRW  => _W( reg, section)                   // HardWare Set then SoftWare RW
      case AccessType.RWHS  => _W( reg, section)                   // SoftWare RW then HardWare Set
      case x: AccessType.CSTM  =>                                  // CSTM-AccessType don't generate logic which implement use themselfs, only register for doc
    }
  }

  def registerInOnlyReadLogic[T <: BaseType](reg: T, acc: AccessType, resetValue:BigInt, doc: String): Unit = {
    registerInWithWriteLogic(reg, acc, resetValue, doc, dontCreatWriteLogic = true)
  }

  def registerAtOnlyReadLogic[T <: BaseType](pos: Int, reg: T, acc: AccessType, resetValue:BigInt, doc: String): Unit = {
    registerAtWithWriteLogic(pos, reg, acc, resetValue, doc, dontCreatWriteLogic = true)
  }

  def registerInWithWriteLogic[T <: BaseType](reg: T, acc: AccessType, resetValue:BigInt, doc: String, dontCreatWriteLogic: Boolean = false): Unit = {
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

  def registerAtWithWriteLogic[T<: BaseType](pos: Int, reg: T, acc: AccessType, resetValue:BigInt, doc: String, dontCreatWriteLogic: Boolean = false): Unit ={
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
  def field(bc: BitCount, acc: AccessType, resetValue: BigInt)(implicit symbol: SymbolName): Bits = field(bc, acc, resetValue, doc = "")(symbol)
  @deprecated(message = "field(Bits/UInt/SInt(n bit)/Bool, acc) recommend", since = "2022-12-31")
  def field(bc: BitCount, acc: AccessType, resetValue: BigInt, doc: String)(implicit symbol: SymbolName): Bits = {
    val section: Range = fieldPtr+bc.value-1 downto fieldPtr
    val ret: Bits = acc match {
      case AccessType.RO    => RO(bc)                         //- W: no effect, R: no effect
      case AccessType.ROV   => B(resetValue, bc.value bit)    //- ReadOnlyValue, used for constant like device-ID/hw-Version
      case AccessType.RW    => W( bc, section, resetValue)    //- W: as-is, R: no effect
      case AccessType.RC    => RC(bc, section, resetValue)    //- W: no effect, R: clears all bits
      case AccessType.RS    => RS(bc, section, resetValue)    //- W: no effect, R: sets all bits
      case AccessType.WRC   => WRC(bc, section, resetValue)   //- W: as-is, R: clears all bits
      case AccessType.WRS   => WRS(bc, section, resetValue)   //- W: as-is, R: sets all bits
      case AccessType.WC    => WC(bc, section, resetValue)    //- W: clears all bits, R: no effect
      case AccessType.WS    => WS(bc, section, resetValue)    //- W: sets all bits, R: no effect
      case AccessType.WSRC  => WSRC(bc, section, resetValue)  //- W: sets all bits, R: clears all bits
      case AccessType.WCRS  => WCRS(bc, section, resetValue)  //- W: clears all bits, R: sets all bits
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
      case AccessType.WOC   => Rerror = true; WC(bc, section, resetValue) //- W: clears all bits, R: error
      case AccessType.WOS   => Rerror = true; WS(bc, section, resetValue) //- W: sets all bits, R: error
      case AccessType.W1    =>                W1(bc, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
      case AccessType.WO1   => Rerror = true; W1(bc, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
      case AccessType.NA    => NA(bc)                                     // -W: reserved, R: reserved
      case AccessType.W1P   => WBP(section, resetValue, AccessType.W1P )  //- W: 1/0 pulse/no effect on matching bit, R: no effect
      case AccessType.W0P   => WBP(section, resetValue, AccessType.W0P )  //- W: 0/1 pulse/no effect on matching bit, R: no effect
      case AccessType.HSRW  => W( bc, section, resetValue)                //- depracated, please use feild(hardType[T]) instead
      case AccessType.RWHS  => W( bc, section, resetValue)                //- depracated, please use feild(hardType[T]) instead
      case x:AccessType.CSTM=> Bits(0 bit)                        //- depracated, please use feild(hardType[T]) instead
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
  def getAddr()        : BigInt           = addr
  def getSize()        : BigInt           = busif.wordAddressInc
  def getDoc()         : String           = doc
  def getFieldDescrs() : List[FieldDescr] = getFields

  override def accept(vs : BusIfVisitor) = {
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
}

abstract class RegBase(name: String, addr: BigInt, doc: String, busif: BusIf) {
  protected var _name = name
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

  def getName(): String = _name
  def setName(name: String): RegBase

  def readErrorTag = Rerror
  def getFields = fields.toList

  val hitDoRead  = busif.readAddress === U(addr) && busif.doRead
  hitDoRead.setName(f"read_hit_0x${addr}%04x", weak = true)
  val hitDoWrite = busif.writeAddress === U(addr) && busif.doWrite
  hitDoWrite.setName(f"write_hit_0x${addr}%04x", weak = true)

  def haveWO = fields.filter(_.isWriteOnly).size != 0
  def readBits: Bits = {
    //when field is WriteOnly, need mask data as 0 for security consider
    fields.map(t => if(t.isWriteOnly) B(0, t.getWidth bit) else t.hardbit)
      .reverse
      .foldRight(B(0, 0 bit))(_ ## _)
  }

  def eventR() : Bool = {
    val event = Reg(Bool()) init(False)
    event := hitDoRead
    event
  }

  def eventW() : Bool = {
    val event = Reg(Bool()) init(False)
    event := hitDoWrite
    event
  }

  protected def _RO[T <: BaseType](reg: T): T = reg

  protected def RO(bc: BitCount): Bits = Bits(bc)

  protected def _W1[T <: BaseType](reg: T, section: Range): T ={
    val hardRestFirstFlag = Reg(Bool()) init True
    hardRestFirstFlag.setName(s"${reg.getName}_w1lock_flag", weak = true)
    when(hitDoWrite && hardRestFirstFlag){
      reg.assignFromBits(busif.wdata(reg, section))
      hardRestFirstFlag.clear()
    }
    reg
  }

  protected def W1(bc: BitCount, section: Range, resetValue: BigInt): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    val hardRestFirstFlag = Reg(Bool()) init True
    hardRestFirstFlag.setName(s"wlock_flag", weak = true)
    when(hitDoWrite && hardRestFirstFlag){
      ret := busif.wdata(ret, section)
      hardRestFirstFlag.clear()
    }
    ret
  }

  protected def _W[T <: BaseType](reg: T, section: Range): T ={
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section))
    }
    reg
  }

  protected def W(bc: BitCount, section: Range, resetValue: BigInt ): Bits ={
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section)
    }
    ret
  }

  protected def _RC[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoRead){
      reg.clearAll() //busif.wdata(reg, section, "clear")
    }
    reg
  }

  protected def RC(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoRead){
      ret.clearAll()//ret := busif.wdata(ret, section, "clear")
    }
    ret
  }

  protected def _RS[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoRead){
      reg.setAll() //busif.wdata(reg, section, "set")
    }
    reg
  }

  protected def RS(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoRead){
      ret.setAll()//ret := busif.wdata(ret, section, "set")
    }
    ret
  }

  protected def _WRC[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section)  )//busif.writeData(section))
    }.elsewhen(hitDoRead){
      reg.clearAll() //busif.wdata(reg, section, "clear")
    }
    reg
  }

  protected def WRC(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section)//busif.writeData(section)
    }.elsewhen(hitDoRead){
      ret.clearAll() //ret := busif.wdata(ret, section, "clear")
    }
    ret
  }

  protected def _WRS[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section)  )//busif.writeData(section))
    }.elsewhen(hitDoRead){
      reg.setAll() //busif.wdata(reg, section, "set")
    }
    reg
  }

  protected def WRS(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section)//busif.writeData(section)
    }.elsewhen(hitDoRead){
      ret.setAll() //ret := busif.wdata(ret, section, "set")
    }
    ret
  }

  protected def _WC[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section, "clear"))//Bits(reg.getBitsWidth bit).clearAll())
    }
    reg
  }

  protected def WC(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section, "clear")//ret.clearAll()
    }
    ret
  }

  protected def _WS[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section, "set")  )//Bits(reg.getBitsWidth bit).setAll())
    }
    reg
  }

  protected def WS(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section, "set")  //ret.setAll()
    }
    ret
  }

  protected def _WSRC[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section, "set")  )//Bits(reg.getBitsWidth bit).setAll())
    }.elsewhen(hitDoRead){
      reg.clearAll() //busif.wdata(reg, section, "clear")
    }
    reg
  }

  protected def WSRC(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section, "set")  //ret.setAll()
    }.elsewhen(hitDoRead){
      ret.clearAll()//ret := busif.wdata(ret, section, "clear")
    }
    ret
  }

  protected def _WCRS[T <: BaseType](reg: T, section: Range): T = {
    when(hitDoWrite){
      reg.assignFromBits(busif.wdata(reg, section, "clear"))//Bits(reg.getBitsWidth bit).clearAll())
    }.elsewhen(hitDoRead){
      reg.setAll() //busif.wdata(reg, section, "set")
    }
    reg
  }

  protected def WCRS(bc: BitCount, section: Range, resetValue: BigInt): Bits = {
    val ret = Reg(Bits(bc)) init B(resetValue)
    when(hitDoWrite){
      ret := busif.wdata(ret, section, "clear")//ret.clearAll()
    }.elsewhen(hitDoRead){
      ret.setAll()//ret := busif.wdata(ret, section, "set")
    }
    ret
  }

  protected def _WB[T <: BaseType](reg: T, section: Range, accType: AccessType): T = {
    when(hitDoWrite){
      section.reverse.map(_ - section.min).foreach{ i =>
        val regbit: Bool = reg match {
          case t: Bool => require(section.size == 1); t
          case t: BitVector => t(i)
        }
        val x = i + section.min
        accType match {
          case AccessType.W1C => when( busif.mwdata(x)){regbit := busif.wdata(regbit, x, "clear" )}//regbit.clear()   }
          case AccessType.W1S => when( busif.mwdata(x)){regbit := busif.wdata(regbit, x, "set"   )}//regbit.set()     }
          case AccessType.W1T => when( busif.mwdata(x)){regbit := busif.wdata(regbit, x, "toggle")}//regbit := ~regbit}
          case AccessType.W0C => when(~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "clear" )}//regbit.clear()   }
          case AccessType.W0S => when(~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "set"   )}//regbit.set()     }
          case AccessType.W0T => when(~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "toggle")}//regbit := ~regbit}
          case _ =>
        }
      }
    }
    reg
  }

  protected def WB(section: Range, resetValue: BigInt, accType: AccessType): Bits = {
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    when(hitDoWrite){
      for(x <- section) {
        val idx = x - section.min
        accType match {
          case AccessType.W1C => when( busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "clear" )} //ret(idx).clear()
          case AccessType.W1S => when( busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "set"   )}//ret(idx).set()  }
          case AccessType.W1T => when( busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "toggle")}//ret(idx) := ~ret(idx)}
          case AccessType.W0C => when(~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "clear" )}//ret(idx).clear()}
          case AccessType.W0S => when(~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "set"   )}//ret(idx).set()  }
          case AccessType.W0T => when(~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), idx, "toggle")}//ret(idx) := ~ret(idx)}
          case _ =>
        }
      }
    }
    ret
  }

  protected def _WBR[T <: BaseType](reg: T, section: Range, accType: AccessType): T ={
    section.reverse.map(_ - section.min).foreach { i =>
      val regbit = reg match {
        case t: Bool => require(section.size == 1); t
        case t: BitVector => t(i)
      }
      val x = i + section.min
      accType match {
        case AccessType.W1SRC => {
          when(hitDoWrite && busif.mwdata(x)) {regbit := busif.wdata(regbit, x, "set" )  }//regbit.set()}
            .elsewhen(hitDoRead)              {regbit.clear()} //regbit := busif.wdata(regbit, x, "clear" )
        }
        case AccessType.W1CRS => {
          when(hitDoWrite && busif.mwdata(x)) {regbit := busif.wdata(regbit, x, "clear" )}//regbit.clear()}
            .elsewhen(hitDoRead)              {regbit.set()} //regbit := busif.wdata(regbit, x, "set" )
        }
        case AccessType.W0SRC => {
          when(hitDoWrite && ~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "set" )  }//regbit.set()}
            .elsewhen(hitDoRead)              {regbit.clear()} //regbit := busif.wdata(regbit, x, "clear" )
        }
        case AccessType.W0CRS => {
          when(hitDoWrite && ~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "clear" )}//regbit.clear()}
            .elsewhen(hitDoRead)              {regbit.set()} //regbit := busif.wdata(regbit, x, "set" )
        }
        case _ =>
      }
      reg
    }
    reg
  }

  protected def WBR(section: Range, resetValue: BigInt, accType: AccessType): Bits ={
    val ret = Reg(Bits(section.size bits)) init B(resetValue)
    for(x <- section) {
      val idx = x - section.min
      accType match {
        case AccessType.W1SRC => {
          when(hitDoWrite && busif.mwdata(x)) {ret(idx) := busif.wdata(ret(idx), x, "set" )  }//ret(idx).set()}
            .elsewhen(hitDoRead)              {ret(idx).clear()} //ret(idx) := busif.wdata(ret(idx), x, "clear" )
        }
        case AccessType.W1CRS => {
          when(hitDoWrite && busif.mwdata(x)) {ret(idx) := busif.wdata(ret(idx), x, "clear" )}//ret(idx).clear()}
            .elsewhen(hitDoRead)              {ret(idx).set()} //ret(idx) := busif.wdata(ret(idx), x, "set" )
        }
        case AccessType.W0SRC => {
          when(hitDoWrite && ~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), x, "set" )  }//ret(idx).set()}
            .elsewhen(hitDoRead)              {ret(idx).clear()} //ret(idx) := busif.wdata(ret(idx), x, "clear" )
        }
        case AccessType.W0CRS => {
          when(hitDoWrite && ~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), x, "clear" )}//ret(idx).clear()}
            .elsewhen(hitDoRead)              {ret(idx).set()} //ret(idx) := busif.wdata(ret(idx), x, "set" )
        }
        case _ =>
      }
    }
    ret
  }

  protected def _WBP[T <: BaseType](reg: T, section: Range, accType: AccessType): T ={
    section.reverse.map(_ - section.min).foreach { i =>
      val regbit = reg match {
        case t: Bool => require(section.size == 1); t
        case t: BitVector => t(i)
      }
      val x = i + section.min
      accType match {
        case AccessType.W1P => {
          when(hitDoWrite &&  busif.mwdata(x)){regbit := busif.wdata(regbit, x, "toggle" )}//~regbit}
            .otherwise{regbit := False}
        }
        case AccessType.W0P => {
          when(hitDoWrite && ~busif.mwdata(x)){regbit := busif.wdata(regbit, x, "toggle" )}//~regbit}
            .otherwise{regbit := False}
        }
      }
    }
    reg
  }

  protected def WBP(section: Range, resetValue: BigInt, accType: AccessType): Bits ={
    val resetValues = B(resetValue)
    val ret = Reg(Bits(section.size bits)) init resetValues
    for(x <- section) {
      val idx = x - section.min
      accType match {
        case AccessType.W1P => {
          when(hitDoWrite &&  busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), x, "toggle" )}//~ret(idx)}
            .otherwise{ret(idx) := False}
        }
        case AccessType.W0P => {
          when(hitDoWrite && ~busif.mwdata(x)){ret(idx) := busif.wdata(ret(idx), x, "toggle" )}//~ret(idx)}
            .otherwise{ret(idx) := resetValues(idx)}
        }
      }
    }
    ret
  }

  protected def NA(bc: BitCount): Bits = {
    Bits(bc).clearAll()
  }
}
