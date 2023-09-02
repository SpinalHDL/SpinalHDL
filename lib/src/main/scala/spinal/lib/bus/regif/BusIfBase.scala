package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.BigIntRicher
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping

import language.experimental.macros
import scala.collection.mutable.ListBuffer

trait BusIfBase extends Area{
  val askWrite: Bool
  val askRead: Bool
  val doWrite: Bool
  val doRead: Bool

  val readData: Bits
  val writeData: Bits
  val readError: Bool
  val readSync: Boolean = true
  val withStrb: Boolean
  val wstrb: Bits  //= withstrb generate(Bits(strbWidth bit))
  val wmask: Bits  //= withstrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits //= withstrb generate(Bits(busDataWidth bit))

  def readAddress(): UInt
  def writeAddress(): UInt

  def readHalt(): Unit
  def writeHalt(): Unit

  def busDataWidth: Int
  def wordAddressInc: Int = busDataWidth / 8
  def strbWidth: Int = busDataWidth / 8
  def underbitWidth: Int = log2Up(wordAddressInc)

  def mwdata(sec: Range): Bits = if(withStrb) writeData(sec) & wmask(sec) else writeData(sec)

  def initStrbMasks() = {
    if (withStrb) {
      (0 until strbWidth).foreach { i =>
        wmask ((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0xFF, 8 bit), B(0, 8 bit))
        wmaskn((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0, 8 bit), B(0xFF, 8 bit))
      }
    }
  }
  def wdata(reg: BaseType, sec: Range): Bits = wdata(reg, sec, oper = "normal")
  def wdata(reg: BaseType, sec: Range, oper: String):Bits = {
    oper match {
      case "clear" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec))
            case t: UInt => (t.asBits & wmaskn(sec))
            case t: SInt => (t.asBits & wmaskn(sec))
            case t: Bool => (t.asBits & wmaskn(sec))
            case _       => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else B(0, sec.size bit)
      case "set" =>
        if(withStrb) {
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | wmask(sec)
            case t: UInt => (t.asBits & wmaskn(sec)) | wmask(sec)
            case t: SInt => (t.asBits & wmaskn(sec)) | wmask(sec)
            case t: Bool => (t.asBits & wmaskn(sec)) | wmask(sec)
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        }else Bits(sec.size bit).setAll()
      case "normal" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: UInt => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: SInt => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: Bool => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else writeData(sec)
      case "toggle" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | (~t(sec)        & wmask(sec))
            case t: UInt => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case t: SInt => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case t: Bool => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else ~reg.asBits(sec)
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
  def mwdata(pos: Int): Bool = if(withStrb) writeData(pos) & wmask(pos) else writeData(pos)
  def wdata(reg: Bool, pos: Int): Bool = wdata(reg, pos, oper = "normal")
  def wdata(reg: Bool, pos: Int, oper: String): Bool = {
    oper match {
      case "clear"  => if (withStrb) (reg & wmaskn(pos))                                 else False
      case "set"    => if (withStrb) (reg & wmaskn(pos)) |                   wmask(pos)  else True
      case "normal" => if (withStrb) (reg & wmaskn(pos)) | (writeData(pos) & wmask(pos)) else writeData(pos)
      case "toggle" => if (withStrb) (reg & wmaskn(pos)) | (~reg           & wmask(pos)) else ~reg
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
}

trait BusIf extends BusIfBase {
  type B <: this.type
  private val RegInsts = ListBuffer[RegInst]()
  private var regPtr: BigInt = 0
  private var readDefaultValue: BigInt = 0

  def orderdRegInsts = RegInsts.sortBy(_.addr)
  def getModuleName: String
  def setReservedAddressReadValue(value: BigInt) = readDefaultValue = value
  def getReservedAddressReadValue = readDefaultValue

  val regPre: String

  private val regAddressHistory = ListBuffer[BigInt]()
  def addressUsed(addr: BigInt) = regAddressHistory.contains(addr)

  private def attachAddr(addr: BigInt) = {
    if(regAddressHistory.contains(addr)){
      SpinalError(s"Address: ${regPtr.hexString(16)} already used before, check please!")
    } else {
      regAddressHistory.append(addr)
    }
  }

  def getRegPtr(): BigInt = regPtr

  /*Attention: Should user make address no conflict them selves*/
  def regPtrReAnchorAt(pos: BigInt) = {
    require(pos % (busDataWidth/8) ==0, s"Address Postion need allign datawidth ${busDataWidth/8} byte")
    regPtr = pos
  }

  private def checkLastNA(): Unit = RegInsts.foreach(_.checkLast)
  private def regNameUpdate(): Unit = {
    val words = "\\w*".r
    val pre = regPre match{
      case "" => ""
      case words(_*) => regPre + "_"
      case _ => SpinalError(s"${regPre} should be Valid naming : '[A-Za-z0-9_]+'")
    }
    RegInsts.foreach(t => t.setName(s"${pre}${t.getName()}"))
  }

  private var isChecked: Boolean = false
  def preCheck(): Unit = {
    if(!isChecked){
      checkLastNA()
      regNameUpdate()
      isChecked = true
    }
  }

  component.addPrePopTask(() => {
    readGenerator()
  })

  def newRegAt(address: BigInt, doc: String)(implicit symbol: SymbolName) = {
    assert(address % wordAddressInc == 0, s"located Position not align by wordAddressInc: ${wordAddressInc}")
    val reg = creatReg(symbol.name, address, doc)
    attachAddr(address)
    regPtr = address + wordAddressInc
    reg
  }

  def newReg(doc: String)(implicit symbol: SymbolName) = {
    val res = creatReg(symbol.name, regPtr, doc)
    attachAddr(regPtr)
    regPtr += wordAddressInc
    res
  }

  def creatReg(name: String, addr: BigInt, doc: String) = {
    val ret = new RegInst(name, addr, doc, this)
    RegInsts += ret
    ret
  }

  def newRAM(name: String, addr: BigInt, size: BigInt, doc: String) = {
    class bmi extends Bundle{
      val wr     = Bool()
      val waddr  = UInt()
      val wdata  = Bits()
      val rd     = Bool()
      val raddr  = UInt()
      val rdata  = Bits()
    }
  }

  def FIFO(doc: String)(implicit symbol: SymbolName) = {
    val  res = creatReg(symbol.name, regPtr, doc)
    regPtr += wordAddressInc
    res
  }

  @deprecated(message = "", since = "2022-12-31")
  def FactoryInterruptWithMask(regNamePre: String, triggers: Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There have no inputs Trigger signals")
      case x if x > busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${busDataWidth}")
      case _ =>
    }
    val ENS    = newReg("Interrupt Enable Register")(SymbolName(s"${regNamePre}_ENABLES"))
    val MASKS  = newReg("Interrupt Mask   Register")(SymbolName(s"${regNamePre}_MASK"))
    val STATUS = newReg("Interrupt status Register")(SymbolName(s"${regNamePre}_STATUS"))
    val intWithMask = new ListBuffer[Bool]()
    triggers.foreach(trigger => {
      val en   = ENS.field(1 bits, AccessType.RW, doc= "int enable register")(SymbolName(s"_en"))(0)
      val mask = MASKS.field(1 bits, AccessType.RW, doc= "int mask register")(SymbolName(s"_mask"))(0)
      val stat = STATUS.field(1 bits, AccessType.RC, doc= "int status register")(SymbolName(s"_stat"))(0)
      when(trigger && en) {stat.set()}
      intWithMask +=  mask && stat
    })
    intWithMask.foldLeft(False)(_||_)
  }
  /*
    interrupt with Raw/Force/Mask/Status 4 Register Interface
    **/
  def interruptFactory(regNamePre: String, triggers: Bool*): Bool = interruptFactoryAt(regPtr, regNamePre, triggers:_*)
  def interruptFactoryAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    require(triggers.size > 0)
    val groups = triggers.grouped(this.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val offset = addrOffset + 4 * i * this.busDataWidth/8
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_RFMS(offset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }

  /*
    interrupt with Raw/Force/Mask_SET/Mask_CLR/Status 5 Register Interface
    It is to solve the atomic problem that may arise from operating the same mask address on multiple processor cores.
    Currently, two independent mask addresses operate on the same mask register to ensure:
    Two processor cores can independently set the state of one bit without affecting other bits
    */
  def interrupt_W1SCmask_FactoryAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    require(triggers.size > 0)
    val groups = triggers.grouped(this.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val offset = addrOffset + 5 * i * this.busDataWidth/8
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_RF2MS(offset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }
  /*
    interrupt with Raw/Mask/Status 3 Register Interface
    **/
  def interruptFactoryNoForce(regNamePre: String, triggers: Bool*): Bool = interruptFactoryNoForceAt(regPtr, regNamePre, triggers:_*)
  def interruptFactoryNoForceAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    require(triggers.size > 0)
    val groups = triggers.grouped(this.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val offset = addrOffset + 3 * i * this.busDataWidth/8
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_RMS(offset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }
  /*
    interrupt with Mask/Status 2 Register Interface
    always used for sys_level_int merge
    **/
  def interruptLevelFactory(regNamePre: String, levels: Bool*): Bool = interruptLevelFactoryAt(regPtr, regNamePre, levels:_*)
  def interruptLevelFactoryAt(addrOffset: BigInt, regNamePre: String, levels: Bool*): Bool = {
    require(levels.size > 0)
    val groups = levels.grouped(this.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val offset = addrOffset + 2 * i * this.busDataWidth/8
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_MS(offset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }
   /*
    interrupt with Mask_SET/Mask_CLR/Status 3 Register Interface
    always used for sys_level_int merge
    **/
  def interruptLevel_W1SCmask_FactoryAt(addrOffset: BigInt, regNamePre: String, levels: Bool*): Bool = {
    require(levels.size > 0)
    val groups = levels.grouped(this.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val offset = addrOffset + 3 * i * this.busDataWidth/8
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_2MS(offset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }
  /*
  interrupt with Raw/Force/Mask/Status Register Interface
  **/
  protected def int_RFMS(offset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    require(triggers.size <= this.busDataWidth )
    val RAW    = this.newRegAt(offset,"Interrupt Raw status Register\n set when event \n clear raw when write 1")(SymbolName(s"${regNamePre_}INT_RAW"))
    val FORCE  = this.newReg("Interrupt Force  Register\n for SW debug use \n write 1 set raw")(SymbolName(s"${regNamePre_}INT_FORCE"))
    val MASK   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = triggers.map{ event =>
      val nm = event.getPartialName()
      val raw   = RAW.field(Bool(), AccessType.W1C,    resetValue = 0, doc = s"raw, default 0" )(SymbolName(s"${nm}_raw"))
//      val force = FORCE.field(Bool(), AccessType.RW,   resetValue = 0, doc = s"force, default 0" )(SymbolName(s"${nm}_force"))
                  FORCE.parasiteField(raw, AccessType.W1S,   resetValue = 0, doc = s"force, write 1 set, debug use" )
      val mask  = MASK.field(Bool(), AccessType.RW,    resetValue = 1, doc = s"mask, default 1, int off" )(SymbolName(s"${nm}_mask"))
      val status= STATUS.field(Bool(), AccessType.RO,  resetValue = 0, doc = s"stauts default 0" )(SymbolName(s"${nm}_status"))
      raw.setWhen(event)
//      status := (raw || force) && (!mask)
      status := raw && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }
 /*
  interrupt with Raw/Force/Mask_SET/Mask_CLR/Status Register Interface
  **/
  protected def int_RF2MS(offset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    require(triggers.size <= this.busDataWidth )
    val RAW    = this.newRegAt(offset,"Interrupt Raw status Register\n set when event \n clear raw when write 1")(SymbolName(s"${regNamePre_}INT_RAW"))
    val FORCE  = this.newReg("Interrupt Force  Register\n for SW debug use \n write 1 set raw")(SymbolName(s"${regNamePre_}INT_FORCE"))
    val MASK_SET   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off \n write 1 set")(SymbolName(s"${regNamePre_}INT_MASK_SET"))
    val MASK_CLR   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off \n write 1 clear")(SymbolName(s"${regNamePre_}INT_MASK_CLR"))
    val STATUS = this.newReg("Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = triggers.map{ event =>
      val nm = event.getPartialName()
      val raw   = RAW.field(Bool(), AccessType.W1C,    resetValue = 0, doc = s"raw, default 0" )(SymbolName(s"${nm}_raw"))
                  FORCE.parasiteField(raw, AccessType.W1S,   resetValue = 0, doc = s"force, write 1 set, debug use" )
      val mask  = MASK_SET.field(Bool(), AccessType.W1S,    resetValue = 1, doc = s"mask, default 1, int off \n write 1 set" )(SymbolName(s"${nm}_mask"))
                  MASK_CLR.parasiteField(mask, AccessType.W1C,   resetValue = 1, doc = s"mask, default 1, int off \n write 1 clear" )
      val status= STATUS.field(Bool(), AccessType.RO,  resetValue = 0, doc = s"stauts default 0" )(SymbolName(s"${nm}_status"))
      raw.setWhen(event)
      status := raw && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }
  /*
    interrupt with Force/Mask/Status Register Interface
    * */
  protected def int_RMS(offset: BigInt,regNamePre: String, triggers: Bool*): Bool = {
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    require(triggers.size <= this.busDataWidth )
    val RAW    = this.newRegAt(offset,"Interrupt Raw status Register\n set when event \n clear when write 1")(SymbolName(s"${regNamePre_}INT_RAW"))
    val MASK   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n  status = raw && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = triggers.map{ event =>
      val nm = event.getPartialName()
      val raw   = RAW.field(Bool(), AccessType.W1C,    resetValue = 0, doc = s"raw, default 0" )(SymbolName(s"${nm}_raw"))
      val mask  = MASK.field(Bool(), AccessType.RW,    resetValue = 1, doc = s"mask, default 1, int off" )(SymbolName(s"${nm}_mask"))
      val status= STATUS.field(Bool(), AccessType.RO,  resetValue = 0, doc = s"stauts default 0" )(SymbolName(s"${nm}_status"))
      raw.setWhen(event)
      status := raw && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }

  /*
    interrupt with Mask/Status Register Interface
    * */
  protected def int_MS(offset: BigInt, regNamePre: String, int_levels: Bool*): Bool = {
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    require(int_levels.size <= this.busDataWidth )
    val MASK   = this.newRegAt(offset, "Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n status = int_level && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = int_levels.map{ level =>
      val nm = level.getPartialName()
      val mask  = MASK.field(Bool(), AccessType.RW,    resetValue = 1, doc = s"mask" )(SymbolName(s"${nm}_mask"))
      val status= STATUS.field(Bool(), AccessType.RO,  resetValue = 0, doc = s"stauts" )(SymbolName(s"${nm}_status"))
      status := level && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }
  /*
    interrupt with Mask_SET/Mask_CLR/Status Register Interface
    * */
  protected def int_2MS(offset: BigInt, regNamePre: String, int_levels: Bool*): Bool = {
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    require(int_levels.size <= this.busDataWidth )
    val MASK_SET   = this.newRegAt(offset, "Interrupt Mask_SET   Register\n1: int off\n0: int open\n default 1, int off\n set when write 1")(SymbolName(s"${regNamePre_}INT_MASK_SET"))
    val MASK_CLR   = this.newReg( "Interrupt Mask_CLR   Register\n1: int off\n0: int open\n default 1, int off\n clear when write 1")(SymbolName(s"${regNamePre_}INT_MASK_CLR"))
    val STATUS = this.newReg("Interrupt status Register\n status = int_level && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = int_levels.map{ level =>
      val nm = level.getPartialName()
      val mask  = MASK_SET.field(Bool(), AccessType.W1S,    resetValue = 1, doc = s"mask_set ,write 1 set" )(SymbolName(s"${nm}_mask"))
                  MASK_CLR.parasiteField(mask, AccessType.W1C,   resetValue = 1, doc = s"mask_clr, write 1 clear" )
      val status= STATUS.field(Bool(), AccessType.RO,  resetValue = 0, doc = s"stauts" )(SymbolName(s"${nm}_status"))
      status := level && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }
  def accept(vs : BusIfVisitor) = {
    preCheck()

    vs.begin(busDataWidth)

    for(reg <- orderdRegInsts) {
      reg.accept(vs)
    }

    vs.end()
  }

  private def readGenerator() = {
    when(askRead){
      switch (readAddress()) {
        RegInsts.foreach{(reg: RegInst) =>
          if(!reg.allIsNA){
            is(reg.addr){
              readData  := reg.readBits
              readError := Bool(reg.haveWO)
            }
          }
        }
        default{
          //Reserved Address Set False, True is too much strict for software
          readData  := readDefaultValue
          if(withStrb) {
            readError := False
          } else {
            val alignreadhit = readAddress.take(log2Up(wordAddressInc)).orR
            readError := Mux(alignreadhit, True,  False)
          }
        }
      }
    }.otherwise{
      //do not keep readData after read for the reason of security risk
      readData  := readDefaultValue
      readError := False
    }
  }
}
