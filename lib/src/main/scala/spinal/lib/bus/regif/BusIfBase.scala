package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb._
import RegIfDocument._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ListBuffer

trait BusIfAdapter extends Area{
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

trait BusIf extends BusIfAdapter {
  type B <: this.type
  private val RegInsts = ListBuffer[RegInst]()
  private var regPtr: Int = 0

  component.addPrePopTask(() => {
    readGenerator()
    document()(SymbolName("TurboRegBank"))
  })

  def newRegAt(address:Int, doc: String)(implicit symbol: SymbolName) = {
    assert(address % wordAddressInc == 0, s"located Position not align by wordAddressInc:${wordAddressInc}")
    assert(address >= regPtr, s"located Position conflict to Pre allocated Address:${regPtr}")
    creatReg(symbol.name, regPtr, doc)
    regPtr += wordAddressInc
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

  def document(docType: DocType = DocType.HTML)(implicit symbolName: SymbolName) = {
    docType match {
      case DocType.Json =>
      case DocType.Rst  =>
      case DocType.MarkDown =>
      case DocType.HTML => HTML(s"${symbolName.name}")
      case DocType.Docx =>
      case _ =>
    }
  }

  def HTML(moduleName:String) = {
    val body = RegInsts.map(_.trs).foldLeft("")(_+_)
    val html = DocTemplate.getHTML(moduleName, body)
    import java.io.PrintWriter
    val fp = new PrintWriter(s"tmp/${moduleName}.html")
    fp.write(html)
    fp.close
  }

  def readGenerator() = {
    switch (readAddress()(7 downto 0)) {
      when(doRead){
        RegInsts.foreach{(reg: RegInst) =>
          is(reg.addr){
            if(reg.allIsNA){
              readData  := 0
              readError := True
            } else {
              readData  := reg.readBits
              readError := Bool(reg.readErrorTag)
            }
          }
        }
      }
    }
  }
}

case class Apb3BusInterface(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, readSync: Boolean = true) extends BusIf{

  val readError = Bool().setAsReg()
  val readData  = Bits(bus.config.dataWidth bits).setAsReg()

  if(readSync) {
    readError.setAsReg()
    readData.setAsReg()
  } else {
    readError := False
    readData  := 0
  }

  bus.PREADY := True
  bus.PRDATA := readData
  if(bus.config.useSlaveError) bus.PSLVERROR := readError

  val askWrite  = (bus.PSEL(selId) && bus.PENABLE && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE).allowPruning()
  val doWrite   = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
  val doRead    = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()
  val writeData = bus.PWDATA

  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.config.dataWidth
}

