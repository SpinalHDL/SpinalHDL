package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib._
import scala.collection.mutable._

trait MMSlaveFactoryBase extends Area {
  val writeReq: Bool
  val readReq: Bool
  val writeResp: Bool
  val readResp: Bool

  val readData: Bits
  val writeData: Bits

  def readRespond(data : Bits, error : Boolean): Unit
  def writeRespond(error : Boolean): Unit

  def readAddress(): UInt
  def writeAddress(): UInt

  def readAccept() : Unit
  def writeAccept() : Unit

  def busDataWidth: Int
  def wordAddressInc: Int = busDataWidth / 8
}

trait MMSlaveFactory extends MMSlaveFactoryBase {
  private val entries = ListBuffer[Entry]()
  private var nxtAddr: Long = 0

  component.addPrePopTask(() => {
    rwGenerator()
  })

  private def checkName(name: String) = {
    for(e <- entries) {
      if(e.getName().equals(name)) {
        throw new IllegalArgumentException(s"Register name ${name} is not unique.")
      }
    }
    if(!name.matches("[a-zA-Z_][a-zA-Z0-9_]*")) {
      throw new IllegalArgumentException(s"${name} is not a legal C literal.")
    }
  }

  def createReg(name: String, doc: String) = {
    checkName(name)
    val ret = new RegEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def createReadOnlyReg(name: String, doc: String) = {
    checkName(name)
    val ret = new ReadOnlyEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def createWriteOnlyReg(name: String, doc: String) = {
    checkName(name)
    val ret = new WriteOnlyRegEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def createReadStream(name: String, doc: String) = {
    checkName(name)
    val ret = new ReadStreamEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def createWriteStream(name: String, doc: String) = {
    checkName(name)
    val ret = new WriteStreamEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def createClearReg(name: String, doc: String) = {
    checkName(name)
    val ret = new ClearRegEntry(name, nxtAddr, doc, this)
    entries += ret
    nxtAddr += wordAddressInc
    ret
  }

  def assignWriteData(that : Data) = {
    that.assignFromBits(writeData)
  }

  def createIrqRegs(name : String, triggers : Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There are no trigger signals.")
      case x if x > busDataWidth => SpinalError(s"Trigger signal number exceed bus width ${busDataWidth}")
      case _ =>
    }
    val ENS    = createReg("irq_enable", "IRQ enable register")
    val MASKS  = createReg("irq_mask", "IRQ mask register")
    val STATUS = createClearReg("irq_status", "IRQ status register")
    val intWithMask = new ListBuffer[Bool]()
    triggers.foreach(trigger => {
      val en   = ENS.newField("enable", 1 bits, doc= "irq enable")(0)
      val mask = MASKS.newField("mask", 1 bits, doc= "irq mask")(0)
      val stat = STATUS.newField("status", 1 bits, doc= "irq status")(0)
      when(trigger && en) {stat.set()}
      intWithMask +=  mask && stat
    })
    intWithMask.foldLeft(False)(_||_)
  }

  def realignAddress(addr : Long) = {
    assert(addr >= nxtAddr, s"Address must be ${nxtAddr} or greater.")
    assert((addr % wordAddressInc) == 0, s"Address must be multiple of ${wordAddressInc}.")
    nxtAddr = addr
  }

  def accept(vs : MMSlaveFactoryVisitor) = {
    vs.begin(busDataWidth)

    for(reg <- entries) {
      reg.accept(vs)
    }

    vs.end()
  }

  def rwGenerator() = {
    when(readReq){
      switch (readAddress()) {
        entries.foreach{(reg: Entry) =>
          reg.finish
          is(reg.getAddress){
            reg.onReadReqIntern()
          }
        }
        default{
          readRespond(0x0, true)
        }
      }
    }
    when(writeReq){
      switch (writeAddress()) {
        entries.foreach{(reg: Entry) =>
          is(reg.getAddress){
            reg.onWriteReqIntern()
          }
        }
        default{
          writeRespond(true)
        }
      }
    }
  }
}
