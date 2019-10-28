package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb._
import RegIfDocument._
import spinal.lib.bus.misc.SizeMapping

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

  component.addPrePopTask(() => {
    readGenerator()
    document("RegIfExample")
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
    switch (readAddress()) {
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
