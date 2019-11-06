package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb._
import RegIfDocument._
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

  def readAddress(): UInt
  def writeAddress(): UInt

  def readHalt(): Unit
  def writeHalt(): Unit

  def busDataWidth: Int
  def wordAddressInc: Int = busDataWidth / 8
}

trait BusIf extends BusIfBase {
  type B <: this.type
  private val RegInsts = ListBuffer[RegInst]()
  private var regPtr: Int = 0

  def getModuleName: String

  component.addPrePopTask(() => {
    readGenerator()
    document(getModuleName)
  })

  def newRegAt(address:Int, doc: String)(implicit symbol: SymbolName) = {
    assert(address % wordAddressInc == 0, s"located Position not align by wordAddressInc:${wordAddressInc}")
    assert(address >= regPtr, s"located Position conflict to Pre allocated Address:${regPtr}")
    creatReg(symbol.name, address, doc)
    regPtr = address + wordAddressInc
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

  def document(docName: String, docType: DocType = DocType.HTML) = {
    docType match {
      case DocType.Json =>
      case DocType.Rst  =>
      case DocType.MarkDown =>
      case DocType.HTML => HTML(docName)
      case DocType.Docx =>
      case _ =>
    }
  }

  def interruptFacotry(regNamePre: String, triggers: Bool*): Bool = macro Macros.interruptFactoryImpl

  def FactoryInterruptWithMask(regPreName: String, triggers: Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There have no inputs Trrigger signals")
      case x if x > busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${busDataWidth}")
      case _ =>
    }
    val ens    = newReg("Interrupt Enable Reigsiter")(SymbolName(s"${regPreName}_ENABLES"))
    val masks  = newReg("Interrupt Mask   Reigsiter")(SymbolName(s"${regPreName}_MASK"))
    val status = newReg("Interrupt status Reigsiter")(SymbolName(s"${regPreName}_STATUS"))
    val intWithMask = new ListBuffer[Bool]()
    triggers.foreach(trigger => {
      val en   = ens.field(1 bits, AccessType.RW, doc= "int enable register")(SymbolName(s"_en"))(0)
      val mask = masks.field(1 bits, AccessType.RW, doc= "int mask register")(SymbolName(s"_mask"))(0)
      val stat = status.field(1 bits, AccessType.RC, doc= "int status register")(SymbolName(s"_stat"))(0)
      when(trigger && en) {stat.set()}
      intWithMask +=  mask && stat
    })
    intWithMask.foldLeft(False)(_||_)
  }

  private def HTML(docName: String) = {
    val pc = GlobalData.get.phaseContext
    def targetPath = s"${pc.config.targetDirectory}/${docName}.html"
    val body = RegInsts.map(_.trs).foldLeft("")(_+_)
    val html = DocTemplate.getHTML(docName, body)
    import java.io.PrintWriter
    val fp = new PrintWriter(targetPath)
    fp.write(html)
    fp.close
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
