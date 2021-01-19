package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb._
import RegIfDocument._
import CHeads._
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
  val regPre: String

  component.addPrePopTask(() => {
    readGenerator()
    document(getModuleName)
    genCHead(getModuleName)
  })

  def newRegAt(address:Int, doc: String)(implicit symbol: SymbolName) = {
    assert(address % wordAddressInc == 0, s"located Position not align by wordAddressInc:${wordAddressInc}")
    assert(address >= regPtr, s"located Position conflict to Pre allocated Address:${regPtr}")
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

  def FIFO(doc: String)(implicit symbol: SymbolName) = {
    val  res = creatReg(symbol.name, regPtr, doc)
    regPtr += wordAddressInc
    res
  }

  def FactoryInterruptWithMask(regNamePre: String, triggers: Bool*): Bool = {
    triggers.size match {
      case 0 => SpinalError("There have no inputs Trigger signals")
      case x if x > busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${busDataWidth}")
      case _ =>
    }
    val ENS    = newReg("Interrupt Enable Reigsiter")(SymbolName(s"${regNamePre}_ENABLES"))
    val MASKS  = newReg("Interrupt Mask   Reigsiter")(SymbolName(s"${regNamePre}_MASK"))
    val STATUS = newReg("Interrupt status Reigsiter")(SymbolName(s"${regNamePre}_STATUS"))
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

//  @AutoInterrupt
//  def interruptFactory2(regNamePre: String, triggers: Bool*): Bool = {
//    triggers.size match {
//      case 0 => SpinalError("There have no inputs Trigger signals")
//      case x if x > busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${busDataWidth}")
//      case _ =>
//    }
//    False
//  }

  private def HTML(docName: String) = {
    val pc = GlobalData.get.phaseContext
    def targetPath = s"${pc.config.targetDirectory}/${docName}.html"
    val body = RegInsts.map(_.trs(regPre)).mkString("\n")
    val html = DocTemplate.getHTML(docName, body)
    import java.io.PrintWriter
    val fp = new PrintWriter(targetPath)
    fp.write(html)
    fp.close
  }

  def genCHead(cFileName: String) = {
    val pc = GlobalData.get.phaseContext
    def targetPath = s"${pc.config.targetDirectory}/${cFileName}.h"
    val maxRegNameWidth = RegInsts.map(_.name.length).max + regPre.size
    val heads   = RegInsts.map(_.cHeadDefine(maxRegNameWidth, regPre)).mkString("\n")
    val structs = RegInsts.map(_.cStruct(regPre)).mkString("\n")
    import java.io.PrintWriter
    val fp = new PrintWriter(targetPath)
    fp.write(heads)
    fp.write("\n\n" + structs)
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
