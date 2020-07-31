package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib._
import scala.collection.mutable._

trait MMSlaveFactoryBase extends Area {
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
  private val entries = ListBuffer[Entry]()

  component.addPrePopTask(() => {
    readGenerator()
  })

  def createReg(name: String, addr: Long, doc: String) = {
    val ret = new RegEntry(name, addr, doc, this)
    entries += ret
    ret
  }

  def createReadOnlyReg(name: String, addr: Long, doc: String) = {
    val ret = new ReadOnlyEntry(name, addr, doc, this)
    entries += ret
    ret
  }

  def createWriteOnlyReg(name: String, addr: Long, doc: String) = {
    val ret = new WriteOnlyRegEntry(name, addr, doc, this)
    entries += ret
    ret
  }

  def createClearReg(name: String, addr: Long, doc: String) = {
    val ret = new ClearRegEntry(name, addr, doc, this)
    entries += ret
    ret
  }

  def assignWriteData(that : Data) = {
    that.assignFromBits(writeData)
  }

  def createIrqRegs(name : String, addr : Long, triggers : Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There are no trigger signals.")
      case x if x > busDataWidth => SpinalError(s"Trigger signal number exceed bus width ${busDataWidth}")
      case _ =>
    }
    val ENS    = createReg("irq enable", addr, "Interrupt Enable Register")
    val MASKS  = createReg("irq mask", addr + 0x4, "Interrupt Mask   Register")
    val STATUS = createClearReg("irq status", addr + 0x8, "Interrupt status Register")
    val intWithMask = new ListBuffer[Bool]()
    triggers.foreach(trigger => {
      val en   = ENS.newField(1 bits, doc= "irq enable")(SymbolName(s"${name}_en"))(0)
      val mask = MASKS.newField(1 bits, doc= "irq mask")(SymbolName(s"${name}_mask"))(0)
      val stat = STATUS.newField(1 bits, doc= "irq status")(SymbolName(s"${name}_stat"))(0)
      when(trigger && en) {stat.set()}
      intWithMask +=  mask && stat
    })
    intWithMask.foldLeft(False)(_||_)
  }

  def accept(vs : MMSlaveFactoryVisitor) = {
    vs.begin(busDataWidth)

    for(reg <- entries) {
      reg.accept(vs)
    }

    vs.end()
  }

  def readGenerator() = {
    when(doRead){
      switch (readAddress()) {
        entries.foreach{(reg: Entry) =>
          reg.finish
          is(reg.getAddress){
            readData  := reg.readBits
            readError := Bool(reg.readErrorTag)
          }
        }
        default{
          readData  := 0x0
          readError := True
        }
      }
    }
  }
}
