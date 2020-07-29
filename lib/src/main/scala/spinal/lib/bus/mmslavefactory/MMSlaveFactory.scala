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
