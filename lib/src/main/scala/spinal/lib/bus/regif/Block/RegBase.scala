package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.regif.{AccessType, BusIf, RegSlice}

import scala.collection.mutable.ListBuffer

//abstract class RegBase(name: String, addr: BigInt, doc: String, busif: BusIf) {
//  protected var _name = name
//  protected val fields = ListBuffer[Field]()
//  protected var fieldPtr: Int = 0
//  protected var Rerror: Boolean = false
abstract class RegBase(name: String, addr: BigInt, doc: String, busif: BusIf, sec: Secure = null, grp: GrpTag = null) extends RegSlice(name, addr, doc, size = busif.wordAddressInc, sec = sec,  grp = grp)(busif){
  def setName(name: String): RegBase

  def readErrorTag = Rerror

  val hitDoRead  = rdSecurePassage(busif.readAddress === U(addr) && busif.doRead)
  hitDoRead.setName(f"read_hit_0x${addr}%04x", weak = true)
  val hitDoWrite = wrSecurePassage(busif.writeAddress === U(addr) && busif.doWrite)
  hitDoWrite.setName(f"write_hit_0x${addr}%04x", weak = true)

  def haveWO = fields.filter(_.isWriteOnly).size != 0
  def readBits: Bits = {
    this.checkLast
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
