package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib._
import scala.collection.mutable._

trait MMSlaveFactoryBase extends Area{
  val askWrite: Bool
  val askRead: Bool
  val doWrite: Bool
  val doRead: Bool

  val readData: Bits
  val writeData: Bits
  val readError: Bool

  def readAddress(): UInt
  def writeAddress(): UInt

  def readHalt(): Unit
  def writeHalt(): Unit

  def busDataWidth: Int
  def wordAddressInc: Int = busDataWidth / 8
}

trait MMSlaveFactory extends MMSlaveFactoryBase {
  type B <: this.type
  private val RegInsts = ListBuffer[RegInst]()
  private var regPtr: Int = 0

  def newRegAt(address:Int, doc: String)(implicit symbol: SymbolName) = {
    assert(address % wordAddressInc == 0, s"located position not aligned by wordAddressInc:${wordAddressInc}")
    assert(address >= regPtr, s"located position in conflict with pre allocated address:${regPtr}")
    regPtr = address + wordAddressInc
    creatReg(symbol.name, address, doc)
  }

  def newReg(doc: String)(implicit symbol: SymbolName) = {
    val res = creatReg(symbol.name, regPtr, doc)
    regPtr += wordAddressInc
    res
  }

  def creatReg(name: String, addr: Long, doc: String) = {
    val ret = new RegInst(name, addr, doc, this)
    RegInsts += ret
    ret
  }

  def newRAM(name: String, addr: Long, size: Long, doc: String) = {
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

  def FactoryInterruptWithMask(regNamePre: String, triggers: Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There are no input trigger signals")
      case x if x > busDataWidth => SpinalError(s"Trigger signal numbers exceed bus data width ${busDataWidth}")
      case _ =>
    }
    val ENS    = newReg("Interrupt enable register")(SymbolName(s"${regNamePre}_ENABLES"))
    val MASKS  = newReg("Interrupt mask   register")(SymbolName(s"${regNamePre}_MASK"))
    val STATUS = newReg("Interrupt status register")(SymbolName(s"${regNamePre}_STATUS"))
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

  def accept(vs : MMSlaveFactoryVisitor) = {
    vs.begin(busDataWidth)

    for(reg <- RegInsts) {
      reg.accept(vs)
    }

    vs.end()
  }

  def readGenerator() = {
    when(doRead){
      switch (readAddress()) {
        RegInsts.foreach{(reg: RegInst) =>
          is(reg.addr){
            if(!reg.allIsNA){
              readData  := reg.readBits
              readError := Bool(reg.readErrorTag)
            }
          }
        }
        default{
          readData  := 0
          readError := True
        }
      }
    }
  }
}
