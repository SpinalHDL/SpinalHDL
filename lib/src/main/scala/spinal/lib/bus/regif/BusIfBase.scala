package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3SlaveFactory}
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4SlaveFactory}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMSlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper, SizeMapping}

import language.experimental.macros
import scala.collection.mutable.ListBuffer

object BusInterface {
  def apply(factory: BusSlaveFactory, mapping: SizeMapping, regPre: String): BusIf = BusInterface(new BusSlaveFactoryAddressWrapper(factory, mapping.base), regPre = regPre)
  def apply(factory: BusSlaveFactory, mapping: SizeMapping): BusIf = BusInterface(new BusSlaveFactoryAddressWrapper(factory, mapping.base), regPre = null)
  def apply(factory: BusSlaveFactory, addr: BigInt, regPre: String): BusIf = BusInterface(new BusSlaveFactoryAddressWrapper(factory, addr), regPre = regPre)
  def apply(factory: BusSlaveFactory, addr: BigInt): BusIf = BusInterface(new BusSlaveFactoryAddressWrapper(factory, addr), regPre = null)
  def apply(factory: BusSlaveFactory, regPre: String): BusIf = new BusIf(factory, regPre = regPre)
  def apply(factory: BusSlaveFactory): BusIf = new BusIf(factory, regPre = null)
}

class BusIf(protected[regif] val factory: BusSlaveFactory, val regPre: String = null) extends Area {
  type B <: this.type
  private val mappedInsts = ListBuffer[MappedBase]()
  private def nextInstAddr: BigInt = {
    if (mappedInsts.nonEmpty) {
      mappedInsts.last.getAddr() + mappedInsts.last.getSize()
    } else 0
  }

  def getModuleName: String = getName() // TODO: Figure out

  def busDataWidth = factory.busDataWidth
  def wordAddressInc = factory.wordAddressInc

  private def checkLastNA(): Unit = mappedInsts.filter(_.isInstanceOf[RegInst]).map(_.asInstanceOf[RegInst]).foreach(_.checkLast)
  private def regNameUpdate(): Unit = {
    val words = "\\w*".r
    val pre = regPre match{
      case "" => ""
      case words(_*) => regPre + "_"
      case _ => SpinalError(s"${regPre} should be Valid naming : '[A-Za-z0-9_]+'")
    }
    mappedInsts.foreach(t => t.setName(s"${pre}${t.getName()}"))
  }

  private var isChecked: Boolean = false
  def preCheck(): Unit = {
    if(!isChecked){
//      checkLastNA()
      regNameUpdate()
      isChecked = true
    }
  }

  def newRegAt(address: BigInt, doc: String)(implicit symbol: SymbolName) = {
    assert(address % factory.wordAddressInc == 0, s"located Position not align by wordAddressInc: ${factory.wordAddressInc}")
    assert(address >= nextInstAddr, s"located Position conflict to Pre allocated Address: ${nextInstAddr}")
    creatReg(symbol.name, address, doc)
  }

  def newReg(doc: String)(implicit symbol: SymbolName) = {
    val res = creatReg(symbol.name, nextInstAddr, doc)
    res
  }

  def creatReg(name: String, addr: BigInt, doc: String) = {
    val ret = new RegInst(name, addr, doc, this)
    mappedInsts += ret
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
    val  res = creatReg(symbol.name, nextInstAddr, doc)
    res
  }

  @deprecated(message = "", since = "2022-12-31")
  def FactoryInterruptWithMask(regNamePre: String, triggers: Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There have no inputs Trigger signals")
      case x if x > factory.busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${factory.busDataWidth}")
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
  def interruptFactory(regNamePre: String, triggers: Bool*): Bool = interruptFactoryAt(nextInstAddr, regNamePre, triggers:_*)
  def interruptFactoryAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    require(triggers.size > 0)
    val groups = triggers.grouped(factory.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_RFMS(addrOffset, namePre, trigs:_*)
    }
    val intr = Vec(ret).asBits.orR
    val regNamePre_ = if (regNamePre != "") regNamePre+"_" else ""
    intr.setName(regNamePre_ + "intr", weak = true)
    intr
  }
  /*
    interrupt with Raw/Mask/Status 3 Register Interface
    **/
  def interruptFactoryNoForce(regNamePre: String, triggers: Bool*): Bool = interruptFactoryNoForceAt(nextInstAddr, regNamePre, triggers:_*)
  def interruptFactoryNoForceAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
    require(triggers.size > 0)
    val groups = triggers.grouped(factory.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_RMS(addrOffset, namePre, trigs:_*)
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
  def interruptLevelFactory(regNamePre: String, levels: Bool*): Bool = interruptLevelFactoryAt(nextInstAddr, regNamePre, levels:_*)
  def interruptLevelFactoryAt(addrOffset: BigInt, regNamePre: String, levels: Bool*): Bool = {
    require(levels.size > 0)
    val groups = levels.grouped(factory.busDataWidth).toList
    val ret = groups.zipWithIndex.map{case (trigs, i) =>
      val namePre = if (groups.size == 1) regNamePre else regNamePre + i
      int_MS(addrOffset, namePre, trigs:_*)
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
    require(triggers.size <= factory.busDataWidth )
    val RAW    = this.newRegAt(offset,"Interrupt Raw status Register\n set when event \n clear when write 1")(SymbolName(s"${regNamePre_}INT_RAW"))
    val FORCE  = this.newReg("Interrupt Force  Register\n for SW debug use")(SymbolName(s"${regNamePre_}INT_FORCE"))
    val MASK   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n status = (raw || force) && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = triggers.map{ event =>
      val nm = event.getPartialName()
      val force = FORCE.field(1 bit, AccessType.RW,   resetValue = 0, doc = s"force, default 0" )(SymbolName(s"${nm}_force")).lsb
      val raw   = RAW.field(1 bit, AccessType.W1C,    resetValue = 0, doc = s"raw, default 0" )(SymbolName(s"${nm}_raw")).lsb
      val mask  = MASK.field(1 bit, AccessType.RW,    resetValue = 1, doc = s"mask, default 1, int off" )(SymbolName(s"${nm}_mask")).lsb
      val status= STATUS.field(1 bit, AccessType.RO,  resetValue = 0, doc = s"stauts default 0" )(SymbolName(s"${nm}_status")).lsb
      raw.setWhen(event)
      status := (raw || force) && (!mask)
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
    require(triggers.size <= factory.busDataWidth )
    val RAW    = this.newRegAt(offset,"Interrupt Raw status Register\n set when event \n clear when write 1")(SymbolName(s"${regNamePre_}INT_RAW"))
    val MASK   = this.newReg("Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n  status = raw && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = triggers.map{ event =>
      val nm = event.getPartialName()
      val raw   = RAW.field(1 bit, AccessType.W1C,    resetValue = 0, doc = s"raw, default 0" )(SymbolName(s"${nm}_raw")).lsb
      val mask  = MASK.field(1 bit, AccessType.RW,    resetValue = 1, doc = s"mask, default 1, int off" )(SymbolName(s"${nm}_mask")).lsb
      val status= STATUS.field(1 bit, AccessType.RO,  resetValue = 0, doc = s"stauts default 0" )(SymbolName(s"${nm}_status")).lsb
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
    require(int_levels.size <= factory.busDataWidth )
    val MASK   = this.newRegAt(offset, "Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${regNamePre_}INT_MASK"))
    val STATUS = this.newReg("Interrupt status Register\n status = int_level && (!mask)")(SymbolName(s"${regNamePre_}INT_STATUS"))
    val ret = int_levels.map{ level =>
      val nm = level.getPartialName()
      val mask  = MASK.field(1 bit, AccessType.RW,    resetValue = 1, doc = s"mask" )(SymbolName(s"${nm}_mask")).lsb
      val status= STATUS.field(1 bit, AccessType.RO,  resetValue = 0, doc = s"stauts" )(SymbolName(s"${nm}_status")).lsb
      status := level && (!mask)
      status
    }.reduceLeft(_ || _)
    ret.setName(s"${regNamePre_.toLowerCase()}intr", weak = true)
    ret
  }

  def accept(vs : BusIfVisitor) = {
    preCheck()

    vs.begin(factory.busDataWidth)

    for(reg <- mappedInsts) {
      reg.accept(vs)
    }

    vs.end()
  }
}
