/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
  * Created by PIC18F on 02.02.2015.
  */
trait MemWriteToReadKind{
  def writeToReadKind : String
}

trait MemTechnologyKind{
  def technologyKind : String
}

object dontCare extends MemWriteToReadKind{
  override def writeToReadKind: String = "dontCare"
}

object writeFirst extends MemWriteToReadKind {
  override def writeToReadKind: String = "writeFirst"
}

object readFirst extends MemWriteToReadKind {
  override def writeToReadKind: String = "readFirst"
}

object auto extends  MemTechnologyKind{
  override def technologyKind: String = "auto"
}


object ramBlock extends MemTechnologyKind {
  override def technologyKind: String = "ramBlock"
}

object distributedLut extends MemTechnologyKind {
  override def technologyKind: String = "distributedLut"
}

object registerFile extends MemTechnologyKind {
  override def technologyKind: String = "registerFile"
}


object Mem {
  def apply[T <: Data](wordType: T, wordCount: Int) = new Mem(wordType, wordCount)
  def apply[T <: Data](wordType: T, initialContent: Seq[T]) = new Mem(wordType, initialContent.length) init (initialContent)
  def apply[T <: Data](initialContent: Seq[T]) = new Mem(initialContent(0), initialContent.length) init (initialContent)
}

class MemWritePayload[T <: Data](dataType: T, addressWidth: Int) extends Bundle {
  val data = dataType.clone
  val address = UInt(addressWidth bit)
}

case class MemWriteOrReadSync(write : MemWriteOrRead_writePart,read : MemWriteOrRead_readPart)

class Mem[T <: Data](_wordType: T, val wordCount: Int) extends NodeWithVariableInputsCount  with Nameable with Widthable{
  var forceMemToBlackboxTranslation = false
  val _widths = wordType.flatten.map(t => t.getBitsWidth).toVector //Force to fix width of each wire

  def wordType: T = _wordType.clone

  var tech : MemTechnologyKind = auto
  def setTech(tech : MemTechnologyKind) = this.tech = tech

  val ports = ArrayBuffer[Any]()
  def getWritePorts() = ports.filter(_.isInstanceOf[MemWrite]).map(_.asInstanceOf[MemWrite])
  def getReadSyncPorts() = ports.filter(_.isInstanceOf[MemReadSync]).map(_.asInstanceOf[MemReadSync])
  def getReadAsyncPorts() = ports.filter(_.isInstanceOf[MemReadAsync]).map(_.asInstanceOf[MemReadAsync])
  def getMemWriteOrReadSyncPorts() = ports.filter(_.isInstanceOf[MemWriteOrReadSync]).map(_.asInstanceOf[MemWriteOrReadSync])

  override def calcWidth: Int = _widths.reduce(_ + _)//_wordType.flatten.map(_.getBitsWidth).reduceLeft(_ + _)

  def addressWidth = log2Up(wordCount)

  def setAsBlackBox(): this.type = {
    forceMemToBlackboxTranslation = true
    this
  }

  var initialContent: Seq[T] = null

  def init(initialContant: Seq[T]): this.type = {
    assert(initialContant.length == wordCount, s"The initial content if the rom doesn't fit with it word count, ${initialContant.length} vs $wordCount " + this.getScalaLocationLong)
    this.initialContent = initialContant
    this
  }

  def apply(address: UInt): T = {
    val ret = readAsync(address)

    ret.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
        assert(!conservative)
        write(address, that.asInstanceOf[T])
      }
    }
    ret
  }

  def addressType = UInt(addressWidth bit)

  def addressTypeAt(initialValue: BigInt) = U(initialValue, addressWidth bit)

  private def addPort(port : Node with Nameable) : Unit = {
    port.setPartialName("port" + ports.length,true)
    port.setRefOwner(this)
    ports += port
  }

  def readAsync(address: UInt, writeToReadKind: MemWriteToReadKind = dontCare): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt()
    addressBuffer := address
    val readPort = new MemReadAsync(this, addressBuffer, readBits, writeToReadKind)

    addressBuffer.setRefOwner(readPort)
    addressBuffer.setPartialName("address",true)

    readBits.setRefOwner(readPort)
    readBits.setPartialName("data",true)

    addPort(readPort)

    readBits.input = readPort
    readWord.assignFromBits(readBits)
    readWord
  }

  def readSync(address: UInt, enable: Bool = True, writeToReadKind: MemWriteToReadKind = dontCare, crossClock: Boolean = false): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt()
    addressBuffer := address

    val enableBuffer = Bool
    enableBuffer := enable
    val readPort = new MemReadSync(this, addressBuffer, readBits, enableBuffer.dontSimplifyIt(), writeToReadKind, ClockDomain.current)

    addressBuffer.setRefOwner(readPort)
    addressBuffer.setPartialName("address",true)

    readBits.setRefOwner(readPort)
    readBits.setPartialName("data",true)

    enableBuffer.setRefOwner(readPort)
    enableBuffer.setPartialName("enable",true)

    if (crossClock)
      readPort.addTag(crossClockDomain)

    addPort(readPort)

    readBits.input = readPort
    readWord.assignFromBits(readBits)
    readWord
  }

  def readSyncCC(address: UInt, enable: Bool = True, writeToReadKind: MemWriteToReadKind = dontCare): T = {
    readSync(address, enable, writeToReadKind, true)
  }

  def write(address: UInt, data: T, mask: Bits = null): Unit = {
    /*assert(mask == null, "Mem write mask currently not implemented by Spinal. You can either create a blackbox " +
      "or instantiate multiple memory instead")*/
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt()
    addressBuffer := address
    val dataBuffer = Bits(getWidth bit).dontSimplifyIt()
    dataBuffer := data.asBits

    val maskBuffer = if (mask != null) {
      val ret = Bits().dontSimplifyIt()
      ret := mask
      ret
    } else {
      null
    }

    val whenCond =  when.getWhensCond(this)
    val whenBuffer = Bool.dontSimplifyIt()
    whenBuffer := whenCond
    val writePort = new MemWrite(this, addressBuffer, dataBuffer, maskBuffer,whenBuffer, ClockDomain.current)
    inputs += writePort

    addressBuffer.setRefOwner(writePort)
    addressBuffer.setPartialName("address",true)

    dataBuffer.setRefOwner(writePort)
    dataBuffer.setPartialName("data",true)

    if(maskBuffer != null) {
      maskBuffer.setRefOwner(writePort)
      maskBuffer.setPartialName("mask", true)
    }

    whenBuffer.setRefOwner(writePort)
    whenBuffer.setPartialName("enable",true)

    addPort(writePort)
  }

  // ASIC friendly single port ram
  def writeOrReadSync(address: UInt, writeData: T, chipSelect: Bool, writeEnable: Bool, writeToReadKind: MemWriteToReadKind = dontCare, crossClock: Boolean = false): T = {
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt()
    addressBuffer := address
    chipSelect.dontSimplifyIt()
    writeEnable.dontSimplifyIt()


    val dataBuffer = Bits(getWidth bit).dontSimplifyIt()
    dataBuffer := writeData.asBits
    val writePort = new MemWriteOrRead_writePart(this, addressBuffer, dataBuffer, chipSelect, writeEnable, ClockDomain.current)
    inputs += writePort

    addressBuffer.setRefOwner(writePort)
    addressBuffer.setPartialName("address",true)

    dataBuffer.setRefOwner(writePort)
    dataBuffer.setPartialName("writeData",true)


    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()
    val readPort = new MemWriteOrRead_readPart(this, addressBuffer, readBits, chipSelect, writeEnable, writeToReadKind, ClockDomain.current)

    readBits.input = readPort
    readBits.setRefOwner(readPort)
    readBits.setPartialName("readData",true)

    readWord.assignFromBits(readBits)
    if (crossClock)
      readPort.addTag(crossClockDomain)


    writePort.readPart = readPort;
    readPort.writePart = writePort

    readPort.setPartialName("port" + ports.length,true)
    readPort.setRefOwner(this)
    writePort.setPartialName("port" + ports.length,true)
    writePort.setRefOwner(this)
    ports += MemWriteOrReadSync(writePort,readPort)
    //    addPort(readPort)
//    addPort(writePort)
    readWord
  }

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)


  private[core] def getMemSymbolWidth() : Int = {
    var symbolWidth = getWidth
    var symbolWidthSet = false
    this.onEachInput(_ match{
      case port : MemWrite => {
        if(port.getMask != null){
          val portSymbolWidth = getWidth/port.getMask.getWidth
          if(symbolWidthSet){
            if(symbolWidth != portSymbolWidth) SpinalError(s"Mem with different asspect ratio at\n${this.getScalaLocationLong}")
          }else{
            symbolWidth = portSymbolWidth
            symbolWidthSet = true
          }
        }
      }
      case _ =>
    })
    symbolWidth
  }
  private[core] def getMemSymbolCount() : Int = getWidth/getMemSymbolWidth

  def randBoot(): this.type = {
    addTag(spinal.core.randomBoot)
    this
  }
}

class MemReadAsync(mem_ : Mem[_], address_ : UInt, data: Bits, val writeToReadKind: MemWriteToReadKind) extends Node with Widthable with Nameable {
  if (writeToReadKind == readFirst) SpinalError("readFirst mode for asynchronous read is not allowed")

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  var address : Node = address_
  var mem     : Mem[_] = mem_

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(address,0)
    doThat(mem,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(address)
    doThat(mem)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => address = node
    case 1 => mem = node.asInstanceOf[Mem[_]]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(address,mem)
  override def getInput(id: Int): Node = id match{
    case 0 => address
    case 1 => mem
  }


  def getData = data
  def getAddress = address.asInstanceOf[UInt]
  def getMem = mem.asInstanceOf[Mem[_]]
  override def calcWidth: Int = getMem.getWidth
}


object MemReadSync {
  val getAddressId: Int = 4
  val getEnableId: Int = 5
  val getMemId: Int = 6
}

class MemReadSync(mem_ : Mem[_], address_ : UInt, data: Bits, enable_ : Bool, val writeToReadKind: MemWriteToReadKind, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with Nameable{
  var address : Node = address_
  var readEnable  : Node = enable_
  var mem     : Mem[_] = mem_

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemReadSync.getAddressId)
    doThat(readEnable,MemReadSync.getEnableId)
    doThat(mem,MemReadSync.getMemId)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(readEnable)
    doThat(mem)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemReadSync.getAddressId => address = node
    case MemReadSync.getEnableId => readEnable = node
    case MemReadSync.getMemId => mem = node.asInstanceOf[Mem[_]]
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + MemReadSync.getAddressId
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,readEnable,mem)
  override def getInput(id: Int): Node = id match{
    case MemReadSync.getAddressId => address
    case MemReadSync.getEnableId => readEnable
    case MemReadSync.getMemId => mem
    case _ => super.getInput(id)
  }



  override def getSynchronousInputs: List[Node] = getMem :: getAddress :: getReadEnable :: super.getSynchronousInputs

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false

  def getData = data

  def getMem = mem

  def getAddress = address.asInstanceOf[UInt]
  def getReadEnable = readEnable.asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth

  def useReadEnable: Boolean = {
    val lit = getReadEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }

  def sameAddressThan(write: MemWrite): Unit = {
    //Used by backed to symplify
    this.setInput(MemReadSync.getAddressId,write.getAddress)
  }


}


object MemWrite {
  val getAddressId: Int = 4
  val getDataId: Int = 5
  val getMaskId: Int = 6
  val getEnableId: Int = 7
}

class MemWrite(mem: Mem[_], address_ : UInt, data_ : Bits, mask_ : Bits, enable_ : Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with CheckWidth with Nameable{
  var address  : Node  = address_
  var data     : Node = data_
  var mask     : Node with Widthable=  mask_
  var writeEnable  : Node  = enable_



  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemWrite.getAddressId)
    doThat(data,MemWrite.getDataId)
    if(mask != null) doThat(mask,MemWrite.getMaskId)
    doThat(writeEnable,MemWrite.getEnableId)
  }

  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(data)
    if(mask != null) doThat(mask)
    doThat(writeEnable)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemWrite.getAddressId => address = node
    case MemWrite.getDataId => data = node
    case MemWrite.getMaskId => mask = node.asInstanceOf[Node with Widthable]
    case MemWrite.getEnableId => writeEnable = node
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + 3 + (if(mask != null) 1 else 0)
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,data,writeEnable) ++ (if(mask != null) List(mask) else Nil)
  override def getInput(id: Int): Node = id match{
    case MemWrite.getAddressId => address
    case MemWrite.getDataId => data
    case MemWrite.getMaskId => mask
    case MemWrite.getEnableId => writeEnable
    case _ => super.getInput(id)
  }



  override def getSynchronousInputs: List[Node] = {
    val base = getAddress :: getData :: getEnable :: super.getSynchronousInputs
    if(mask != null)
      mask :: base
    else
      base
  }

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false

  def getMem = mem
  def getAddress = address.asInstanceOf[UInt]
  def getData = data.asInstanceOf[Bits]
  def getEnable = writeEnable.asInstanceOf[Bool]

  def getMask: Bits = {
    if (mask.isInstanceOf[Bits])
      mask.asInstanceOf[Bits]
    else
      null
  }


  override def calcWidth: Int = getMem.calcWidth

  def useWriteEnable: Boolean = {
    val lit = getEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }

  override private[core] def checkInferedWidth: String = {
    if(getMask != null && getData.getWidth % getMask.getWidth != 0)
      return s"Memory write_data_width % write_data_mask_width != 0 at\n${this.getScalaLocationLong}"
    else
      null
  }
}

object MemWriteOrRead_writePart {
  val getAddressId: Int = 4
  val getDataId: Int = 5
  val getChipSelectId: Int = 6
  val getWriteEnableId: Int = 7
}

class MemWriteOrRead_writePart(mem: Mem[_], address_ : UInt, data_ : Bits, chipSelect_ : Bool, writeEnable_ : Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with Nameable{
  var address : Node  = address_
  var data     : Node = data_
  var chipSelect   : Node = chipSelect_
  var writeEnable  : Node  = writeEnable_


  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemWriteOrRead_writePart.getAddressId)
    doThat(data,MemWriteOrRead_writePart.getDataId)
    doThat(chipSelect,MemWriteOrRead_writePart.getChipSelectId)
    doThat(writeEnable,MemWriteOrRead_writePart.getWriteEnableId)
  }

  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(data)
    doThat(chipSelect)
    doThat(writeEnable)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemWriteOrRead_writePart.getAddressId => address = node
    case MemWriteOrRead_writePart.getDataId => data = node
    case MemWriteOrRead_writePart.getChipSelectId => chipSelect = node
    case MemWriteOrRead_writePart.getWriteEnableId => writeEnable = node
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + 4
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,data,chipSelect,writeEnable)
  override def getInput(id: Int): Node = id match{
    case MemWriteOrRead_writePart.getAddressId => address
    case MemWriteOrRead_writePart.getDataId => data
    case MemWriteOrRead_writePart.getChipSelectId => chipSelect
    case MemWriteOrRead_writePart.getWriteEnableId => writeEnable
    case _ => super.getInput(id)
  }

  var readPart: MemWriteOrRead_readPart = null

  override def getSynchronousInputs: List[Node] = getAddress :: getData :: getChipSelect :: getWriteEnable :: super.getSynchronousInputs

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false

  def getMem = mem
  def getAddress = address.asInstanceOf[UInt]
  def getData = data.asInstanceOf[Bits]
  def getChipSelect = chipSelect.asInstanceOf[Bool]
  def getWriteEnable = writeEnable.asInstanceOf[Bool]
  override def calcWidth: Int = getMem.calcWidth

  //  def useWriteEnable: Boolean = {
  //    val lit = getEnable.getLiteral[BoolLiteral]
  //    return lit == null || lit.value == false
  //  }
}


object MemWriteOrRead_readPart {
  val getAddressId: Int = 4
  val getChipSelectId: Int = 5
  val getWriteEnableId: Int = 6
  val getMemId: Int = 7
}

class MemWriteOrRead_readPart(mem_ : Mem[_], address_ : UInt, data_ : Bits, chipSelect_ : Bool, writeEnable_ : Bool, val writeToReadKind: MemWriteToReadKind, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with Nameable{

  var address : Node  = address_
  var chipSelect     : Node = chipSelect_
  var writeEnable   : Node = writeEnable_
  var mem  : Mem[_]  = mem_


  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)


  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemWriteOrRead_readPart.getAddressId)
    doThat(chipSelect,MemWriteOrRead_readPart.getChipSelectId)
    doThat(writeEnable,MemWriteOrRead_readPart.getWriteEnableId)
    doThat(mem,MemWriteOrRead_readPart.getMemId)
  }

  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(chipSelect)
    doThat(writeEnable)
    doThat(mem)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemWriteOrRead_readPart.getAddressId => address = node
    case MemWriteOrRead_readPart.getChipSelectId => chipSelect = node
    case MemWriteOrRead_readPart.getWriteEnableId => writeEnable = node
    case MemWriteOrRead_readPart.getMemId => mem = node.asInstanceOf[Mem[_]]
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + 4
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,chipSelect,writeEnable,mem)
  override def getInput(id: Int): Node = id match{
    case MemWriteOrRead_readPart.getAddressId => address
    case MemWriteOrRead_readPart.getChipSelectId => chipSelect
    case MemWriteOrRead_readPart.getWriteEnableId => writeEnable
    case MemWriteOrRead_readPart.getMemId => mem
    case _ => super.getInput(id)
  }



  var writePart: MemWriteOrRead_writePart = null

  override def getSynchronousInputs: List[Node] = getMem :: getAddress :: getChipSelect :: getWriteEnable :: super.getSynchronousInputs

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false

  def getData = data_

  def getMem = mem
  def getAddress = address.asInstanceOf[UInt]
  def getChipSelect = chipSelect.asInstanceOf[Bool]
  def getWriteEnable = writeEnable.asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth
}




//trait MemBlackBoxer{
//  def applyOn(that : Mem[_]): Unit
//}
//
//object MemBlackBoxer extends MemBlackBoxer{
//  override def applyOn(mem: Mem[_]): Unit = {
//    if(mem.getWritePorts() == 1 && mem.ports.length == 1 + mem.getReadAsyncPorts().length + mem.getReadSyncPorts().length){
//      val popComponent = if(Component.current != mem.component){
//        Component.push(mem.component)
//        true
//      } else false
//
//      val wr = mem.getWritePorts.head
//      for(rd <- mem.getReadAsyncPorts) {
//        val clockDomain = wr.getClockDomain
//        clockDomain.push()
//
//        val ram = new Ram_1c_1w_1ra(mem.getWidth, mem.wordCount, rd.writeToReadKind)
//        val enable = clockDomain.isClockEnableActive
//
//        ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
//        ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
//        ram.io.wr.data := wr.getData.allowSimplifyIt()
//
//        ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
//        rd.getData.allowSimplifyIt() := ram.io.rd.data
//
//        ram.setName(mem.getName())
//        clockDomain.pop()
//      }
//
//      for(rd <- mem.getReadSyncPorts()){
//        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
//          val clockDomain = wr.getClockDomain
//
//          clockDomain.push()
//
//          val ram = new Ram_1c_1w_1rs(mem.getWidth, mem.wordCount, rd.writeToReadKind)
//          val enable = clockDomain.isClockEnableActive
//
//          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
//          ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
//          ram.io.wr.data := wr.getData.allowSimplifyIt()
//
//          ram.io.rd.en := rd.getReadEnable.allowSimplifyIt() && enable
//          ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
//          rd.getData.allowSimplifyIt() := ram.io.rd.data
//
//          ram.generic.useReadEnable = {
//            val lit = ram.io.rd.en.getLiteral[BoolLiteral]
//            lit == null || lit.value == false
//          }
//
//          ram.setName(mem.getName())
//          clockDomain.pop()
//        }else{
//          ???
//        }
//      }
//
//      if(popComponent) Component.pop(mem.component)
//    }
//  }
//}
