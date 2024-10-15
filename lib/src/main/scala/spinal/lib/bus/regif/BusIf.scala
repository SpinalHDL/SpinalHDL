package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.localbus.{MemBus, MinBus}
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

trait BusIf extends BusIfBase {
  val bus: Bundle

  type B <: this.type
  private val SliceInsts   = ListBuffer[RegSlice]()
  private var regPtr: BigInt = 0
  protected var readDefaultValue: BigInt = 0
  protected var secFailReadValue: BigInt = null

  protected var grpId: Int = 1
  protected def grpIdInc(): Unit = (grpId += 1)

  private val secureLogicDict = HashMap[Bool, Bool]()

  def getOrCreateSecLogic(key: Bool, logic: Bool): Bool = {
    secureLogicDict.getOrElseUpdate(key, logic)
  }

  def regSlicesNotReuse: List[RegSlice] = slices.filter(_.reuseTag.id == 0)
  def reuseGroups: Map[String, List[RegSlice]] = slices.filter(_.reuseTag.id != 0).groupBy(_.reuseTag.blockName)
  def reuseGroupsById: Map[String, Map[Int, List[RegSlice]]] = reuseGroups.map {case(name, slices) => (name, slices.groupBy(_.reuseTag.id)) }

  def repeatGroupsHead: Map[String, List[RegSlice]] = reuseGroupsById.map(t => t._1 -> t._2.head._2)
  def repeatGroupsBase: Map[String, List[RegSlice]] = reuseGroupsById.map(t => t._1 -> t._2.map(_._2.head).toList.sortBy(_.reuseTag.id))

  def busName = bus.getClass.getSimpleName
  def newGrpTag(name: String) = {
    val ret = GrpTag(grpId, name)
    this.grpIdInc()
    ret
  }

//  protected var partIdFlag: Boolean = false
  protected var blockId: Int = 1
  protected def blockIdInc(): Unit = (blockId += 1)
  def resetBlockTag(): Unit = {
    currentBlockTag = ReuseTag(0, "")
  }

  private var currentBlockTag = ReuseTag(0, "")

  def getCurrentBlockTag = currentBlockTag

  def newBlockTag(instName: String)(partName: String) = {
    val ret = ReuseTag(blockId, partName, regPtr, instName)
    this.blockIdInc()
    currentBlockTag = ret
    ret
  }

  def newBlockTagAt(addr: BigInt, instName: String)(partName: String) = {
    val ret = ReuseTag(blockId, partName, addr, instName)
    this.blockIdInc()
    currentBlockTag = ret
    ret
  }

  def RegAndFifos = SliceInsts.filter(!_.isInstanceOf[RamInst]).toList
  def RegInsts = SliceInsts.filter(_.isInstanceOf[RegInst]).map(_.asInstanceOf[RegInst])
  def RamInsts = SliceInsts.filter(_.isInstanceOf[RamInst]).map(_.asInstanceOf[RamInst])
  def FifoInsts = SliceInsts.filter(_.isInstanceOf[FifoInst]).map(_.asInstanceOf[FifoInst])

  def orderdRegInsts = SliceInsts.sortBy(_.addr)
  def getModuleName: String
  def setReservedAddressReadValue(value: BigInt) = readDefaultValue = value
  def getReservedAddressReadValue = readDefaultValue

  def setSecFailReadValue(value: BigInt) = secFailReadValue = value
  def getSecFailReadValue = secFailReadValue

  def defaultReadBits = B(readDefaultValue, busDataWidth bits)
  def secFailDefaultBits = B(Option(secFailReadValue).getOrElse(readDefaultValue), busDataWidth bits)
  def slices= SliceInsts.toList
  def hasBlock = SliceInsts.filter(_.reuseTag.id != 0).nonEmpty

  def docPath: String = GlobalData.get.phaseContext.config.targetDirectory

  val regPre: String

  private val regAddressHistory = ListBuffer[BigInt]()
  private val regAddressMap     = ListBuffer[SizeMapping]()
  def addressUsed(addr: BigInt) = regAddressHistory.contains(addr)
  def getAddrMap = regAddressHistory.toList.map("0x"+_.hexString()) ++ regAddressMap.map(_.toString)

  private def attachAddr(addr: BigInt) = {
    val ret = regAddressMap.filter(t => (addr <= t.end) && (addr >= t.base))
    if(regAddressHistory.contains(addr)){
      SpinalError(s"Address: ${regPtr.hexString(16)} already used before, check please!")
    } else if(!ret.isEmpty){
      SpinalError(s"${ret.head} overlap with 0x${addr.hexString()}")
    } else {
      regAddressHistory.append(addr)
    }
  }

  private def attachAddr(sizemap: SizeMapping) = {
    val ret = regAddressMap.filter(_.overlap(sizemap))
    val t = regAddressHistory.filter(t => (t <= sizemap.end) &&  (t >= sizemap.base))
    if(!ret.isEmpty){
      SpinalError(s"${ret.head} overlap with ${sizemap}")
    } else if(!t.isEmpty){
      SpinalError(s"0x${t.head.hexString()} overlap with ${sizemap}")
    } else {
      regAddressMap.append(sizemap)
    }
  }

  def getRegPtr(): BigInt = regPtr

  /*Attention: Should user make address no conflict them selves*/
  def regPtrReAnchorAt(pos: BigInt) = {
    require(pos % (busDataWidth/8) ==0, s"Address Postion need allign datawidth ${busDataWidth/8} byte")
    regPtr = pos
  }

  private def checkLastNA(): Unit = SliceInsts.foreach(_.checkLast)
  private def regNameUpdate(): Unit = {
    val words = "\\w*".r
    val pre = regPre match{
      case "" => ""
      case words(_*) => regPre + "_"
      case _ => SpinalError(s"${regPre} should be Valid naming : '[A-Za-z0-9_]+'")
    }
    RegInsts.foreach(t => t.setName(s"${pre}${t.getName()}"))
  }

  private var isChecked: Boolean = false
  def preCheck(): Unit = {
    if(!isChecked){
      checkLastNA()
      regNameUpdate()
      isChecked = true
    }
  }

  component.addPrePopTask(() => {
    this.readGenerator()
    if(withSecFireWall) this.writeErrorGenerator()
  })


  def regPart(name: String)(block : => Unit) = {
    this.newBlockTag(name)(name)
    block
    this.resetBlockTag()
  }


  def newRegAt(address: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName) = {
    addrAlignCheck(address)
    val reg = createReg(symbol.name, address, doc, sec, grp)
    regPtr = address + wordAddressInc
    reg
  }

  def newReg(doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName) = {
    val res = createReg(symbol.name, regPtr, doc, sec, grp)
    regPtr += wordAddressInc
    res
  }

  @deprecated("type error fix", "2024.06.03")
  def creatReg(name: String, addr: BigInt, doc: String, sec: Secure = null,  grp: GrpTag = null) = createReg(name, addr, doc, sec, grp)

  def createReg(name: String, addr: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null) = {
    val ret = new RegInst(name, addr, doc, this, sec, grp)
    SliceInsts += ret
    attachAddr(regPtr)
    ret
  }

  def newRAM(size: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName) = {
    val res = createRAM(symbol.name, regPtr, size, doc, sec, grp)
    regPtr += scala.math.ceil(size.toDouble/wordAddressInc).toLong * wordAddressInc
    res
  }

  def newRAMAt(address: BigInt, size: BigInt, doc: String, sec: Secure = null,  grp: GrpTag = null)(implicit symbol: SymbolName) = {
    addrAlignCheck(address)
    val res = createRAM(symbol.name, address, size, doc, sec, grp)
    regPtr = address + scala.math.ceil(size.toDouble/wordAddressInc).toLong * wordAddressInc
    res
  }

  def createRAM(name: String, addr: BigInt, size: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null) = {
    val ret = new RamInst(name, addr, size, doc, sec, grp)(this)
    SliceInsts += ret
    attachAddr(SizeMapping(addr, size))
    ret
  }

  def newWrFifo(doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName): WrFifoInst = {
    val res = createWrFifo(symbol.name, regPtr, doc, sec, grp)
    regPtr += wordAddressInc
    res
  }

  def newWrFifoAt(address: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName) = {
    addrAlignCheck(address)
    val res = createWrFifo(symbol.name, address, doc, sec, grp)
    regPtr = address + wordAddressInc
    res
  }

  def createWrFifo(name: String, addr: BigInt, Doc: String, sec: Secure = null, grp: GrpTag = null) = {
    val ret = new WrFifoInst(name, addr, Doc, sec, grp)( this)
    SliceInsts += ret
    attachAddr(addr)
    ret
  }

  def createRdFifo(name: String, addr: BigInt, Doc: String, sec: Secure = null, grp: GrpTag = null): RdFifoInst = {
    val ret = new RdFifoInst(name, addr, Doc, sec, grp)(this)
    SliceInsts += ret
    attachAddr(addr)
    ret
  }

  def newRdFifo(doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName): RdFifoInst = {
    val res = createRdFifo(symbol.name, regPtr, doc, sec, grp)
    regPtr += wordAddressInc
    res
  }

  def newRdFifoAt(address: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null)(implicit symbol: SymbolName): RdFifoInst = {
    addrAlignCheck(address)
    val res = createRdFifo(symbol.name, address, doc, sec, grp)
    regPtr = address + wordAddressInc
    res
  }

  def newGrp(maxSize: BigInt, doc: String, sec: Secure = null)(implicit  symbol: SymbolName) = {
    createGrp(symbol.name, regPtr, maxSize, doc, sec)
  }

  def newGrpAt(address: BigInt, maxSize: BigInt, doc: String, sec: Secure = null)(implicit  symbol: SymbolName) = {
    createGrp(symbol.name, address, maxSize, doc, sec)
  }

  def createGrp(name: String, addr: BigInt, maxSize: BigInt, doc: String, sec: Secure = null) = {
    val grp = this.newGrpTag(name)
    val ret = RegSliceGrp(addr, maxSize, doc, sec, grp)(this)
    ret
  }

  def accept(doc: BusIfDoc) = {
    preCheck()
    doc.generate(this)
  }

  def gen(doc: BusIfDoc) = {
    preCheck()
    doc.generate(this)
  }

  def addrAlignCheck(address: BigInt) = {
    if (_addrAlignCheck) assert(address % wordAddressInc == 0, s"located Position not align by wordAddressInc: ${wordAddressInc}")
  }

  def genBaseDocs(docname: String, prefix: String = "") = {
    this.accept(DocHtml(docname))
    this.accept(DocJson(docname))
    this.accept(DocRalf(docname))
    this.accept(DocCHeader(docname, prefix))
    this.accept(DocSVHeader(docname, prefix))
  }

  private def regReadPart() = {
    switch(readAddress()) {
      RegAndFifos.foreach{ (x: RegSlice) =>
        if(!x.allIsNA) x.readGenerator()
      }
      default {
        reg_rdata := readDefaultValue
        //Reserved Address Set False, True is too much strict for software
        if (withStrb) {
          reg_rderr := False
        } else {
          val alignreadhit = readAddress.take(log2Up(wordAddressInc)).orR
          reg_rderr := Mux(alignreadhit, True, False)
        }
      }
    }
  }

  private def regReadGenerator() = {
    when(askRead){
      regReadPart()
    }.otherwise{
      //do not keep readData after read for the reason of security risk
      reg_rdata := readDefaultValue
      reg_rderr := False
    }
  }

  private def readGenerator(): Unit = {
    this.regReadGenerator()
    val mux = WhenBuilder()
    RamInsts.foreach{ ram =>
      mux.when(ram.ram_rdvalid) {
        bus_rdata := ram.readBits
      }
    }
    mux.otherwise {
      bus_rdata := reg_rdata
    }
  }

  private def writeErrorGenerator(): Unit = {
    when(askWrite){
      switch(writeAddress()) {
        RegAndFifos.foreach { slice =>
          slice.wrErrorGenerator()
        }
        default {
          reg_wrerr := False
        }
      }
      RamInsts.foreach { ram =>
        ram.wrErrorGenerator()
      }
    }.otherwise{
      reg_wrerr := False
    }
  }
}
