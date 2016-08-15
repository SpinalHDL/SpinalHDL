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
trait ReadUnderWritePolicy{
  def readUnderWriteString : String
}

trait MemTechnologyKind{
  def technologyKind : String
}

object dontCare extends ReadUnderWritePolicy{
  override def readUnderWriteString: String = "dontCare"
}

object writeFirst extends ReadUnderWritePolicy {
  override def readUnderWriteString: String = "writeFirst"
}

object readFirst extends ReadUnderWritePolicy {
  override def readUnderWriteString: String = "readFirst"
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

case class MemWriteOrReadSync(write : MemReadWrite_writePart,read : MemReadWrite_readPart)

object AllowMixedWidth extends SpinalTag

class Mem[T <: Data](_wordType: T, val wordCount: Int) extends NodeWithVariableInputsCount  with Nameable with Widthable{
  var forceMemToBlackboxTranslation = false
  val _widths = wordType.flatten.map(t => t.getBitsWidth).toVector //Force to fix width of each wire

  def wordType: T = _wordType.clone

  var technology : MemTechnologyKind = auto
  def setTechnology(tech : MemTechnologyKind) = this.technology = tech

  val ports = ArrayBuffer[Any]()
  def getWritePorts() = ports.filter(_.isInstanceOf[MemWrite]).map(_.asInstanceOf[MemWrite])
  def getReadSyncPorts() = ports.filter(_.isInstanceOf[MemReadSync]).map(_.asInstanceOf[MemReadSync])
  def getReadAsyncPorts() = ports.filter(_.isInstanceOf[MemReadAsync]).map(_.asInstanceOf[MemReadAsync])
  def getMemWriteOrReadSyncPorts() = ports.filter(_.isInstanceOf[MemWriteOrReadSync]).map(_.asInstanceOf[MemWriteOrReadSync])

  override def calcWidth: Int = _widths.reduce(_ + _)//_wordType.flatten.map(_.getBitsWidth).reduceLeft(_ + _)

  def addressWidth = log2Up(wordCount)

  def generateAsBlackBox(): this.type = {
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


  def readAsync(address: UInt, readUnderWrite: ReadUnderWritePolicy = dontCare): T = {
    val readWord = wordType.clone()
    readAsyncImpl(address,readWord,readUnderWrite,false)
    readWord
  }

  def readAsyncMixedWidth(address: UInt, data : Data, readUnderWrite: ReadUnderWritePolicy = dontCare): Unit =  readAsyncImpl(address,data,readUnderWrite,true)

  def readAsyncImpl(address: UInt, data : Data,readUnderWrite : ReadUnderWritePolicy = dontCare,allowMixedWidth : Boolean): Unit = {
    val readBits = Bits(data.getBitsWidth bits)
    val addressBuffer = (if(allowMixedWidth) UInt() else UInt(addressWidth bits)).dontSimplifyIt() //Allow resized address when mixedMode is disable
    addressBuffer := address
    val readPort = new MemReadAsync(this, addressBuffer, readBits, readUnderWrite)
    if(allowMixedWidth) readPort.addTag(AllowMixedWidth)

    addressBuffer.setPartialName(readPort,"address",true)
    readBits.setPartialName(readPort,"data",true)

    addPort(readPort)

    readBits.input = readPort
    data.assignFromBits(readBits)
  }

  def readSync(address: UInt, enable: Bool = null, readUnderWrite: ReadUnderWritePolicy = dontCare, clockCrossing: Boolean = false): T = {
    val readWord = wordType.clone()
    readSyncImpl(address,readWord,enable,readUnderWrite,clockCrossing,false)
    readWord
  }

  def readSyncMixedWidth(address: UInt, data : Data, enable: Bool = null,readUnderWrite: ReadUnderWritePolicy = dontCare,clockCrossing: Boolean = false): Unit =  readSyncImpl(address,data,enable,readUnderWrite,clockCrossing,true)

  def readSyncImpl(address: UInt, data : Data, enable: Bool = null, readUnderWrite: ReadUnderWritePolicy = dontCare, clockCrossing: Boolean = false,allowMixedWidth : Boolean = false): Unit = {
    val readBits = Bits(data.getBitsWidth bits)

    val addressBuffer = (if(allowMixedWidth) UInt() else UInt(addressWidth bits)).dontSimplifyIt() //Allow resized address when mixedMode is disable
    addressBuffer := address

    val enableBuffer = Bool.dontSimplifyIt()
    enableBuffer := (if(enable != null) enable else True) //when.getWhensCond(this))
    val readPort = new MemReadSync(this, addressBuffer, readBits, enableBuffer, readUnderWrite, ClockDomain.current)
    if(allowMixedWidth) readPort.addTag(AllowMixedWidth)

    addressBuffer.setPartialName(readPort,"address",true)
    readBits.setPartialName(readPort,"data",true)
    enableBuffer.setPartialName(readPort,"enable",true)

    if (clockCrossing)
      readPort.addTag(crossClockDomain)

    addPort(readPort)

    readBits.input = readPort
    data.assignFromBits(readBits)
  }

  @deprecated
  def readSyncCC(address: UInt, enable: Bool = True, readUnderWrite: ReadUnderWritePolicy = dontCare): T = {
    readSync(address, enable, readUnderWrite, true)
  }


  def writeMixedWidth(address: UInt, data: Data,enable : Bool = null, mask: Bits = null): Unit = writeImpl(address,data,enable,mask,allowMixedWidth = true)
  def write(address: UInt, data: T,enable : Bool = null, mask: Bits = null) : Unit = writeImpl(address,data,enable,mask,allowMixedWidth = false)

  def writeImpl(address: UInt, data: Data,enable : Bool = null, mask: Bits = null,allowMixedWidth : Boolean = false) : Unit = {
    /*assert(mask == null, "Mem write mask currently not implemented by Spinal. You can either create a blackbox " +
      "or instantiate multiple memory instead")*/
    val addressBuffer = (if(allowMixedWidth) UInt() else UInt(addressWidth bits)).dontSimplifyIt() //Allow resized address when mixedMode is disable
    addressBuffer := address
    val dataBuffer = (if(allowMixedWidth) Bits() else Bits(getWidth bits)).dontSimplifyIt()
    dataBuffer := data.asBits

    val maskBuffer = if (mask != null) {
      val ret = Bits().dontSimplifyIt()
      ret := mask
      ret
    } else {
      null
    }

    val whenCond =  if(enable == null) when.getWhensCond(this) else enable
    val whenBuffer = Bool.dontSimplifyIt()
    whenBuffer := whenCond
    val writePort = new MemWrite(this, addressBuffer, dataBuffer, maskBuffer,whenBuffer, ClockDomain.current)
    if(allowMixedWidth) writePort.addTag(AllowMixedWidth)
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

  // Single port ram
  def readWriteSync (address: UInt,
                     data: T,
                     enable: Bool,
                     write: Bool,
                     mask: Bits = null,
                     readUnderWrite: ReadUnderWritePolicy = dontCare,
                     clockCrossing: Boolean = false): T = {
    readWriteSyncImpl(address,data,enable,write,mask,readUnderWrite,clockCrossing,false)
  }

  def readWriteSyncMixedWidth[U <: Data](address: UInt,
                               data: U,
                               enable: Bool,
                               write: Bool,
                               mask: Bits = null,
                               readUnderWrite: ReadUnderWritePolicy = dontCare,
                               clockCrossing: Boolean = false): U = {
    readWriteSyncImpl(address,data,enable,write,mask,readUnderWrite,clockCrossing,true)
  }

  def readWriteSyncImpl[U <: Data](address: UInt,
                                   data: U,
                                   enable: Bool,
                                   write: Bool,
                                   mask: Bits = null,
                                   readUnderWrite: ReadUnderWritePolicy = dontCare,
                                   clockCrossing: Boolean = false,
                                   allowMixedWidth : Boolean = false): U = {
    val addressBuffer = (if(allowMixedWidth) UInt() else UInt(addressWidth bits)).dontSimplifyIt() //Allow resized address when mixedMode is disable
    addressBuffer := address
    val dataBuffer = (if(allowMixedWidth) Bits() else Bits(getWidth bits)).dontSimplifyIt()
    dataBuffer := data.asBits

    val enableBuffer = Bool.dontSimplifyIt()
    enableBuffer := enable

    val writeBuffer = Bool.dontSimplifyIt()
    writeBuffer := write

    val maskBuffer = if (mask != null) {
      val ret = Bits().dontSimplifyIt()
      ret := mask
      ret
    } else {
      null
    }

    val writePort = new MemReadWrite_writePart(this, addressBuffer, dataBuffer,maskBuffer, enableBuffer, writeBuffer, ClockDomain.current)
    if(allowMixedWidth) writePort.addTag(AllowMixedWidth)
    inputs += writePort

    enableBuffer.setPartialName(writePort,"enable",true)
    writeBuffer.setPartialName(writePort,"write",true)
    addressBuffer.setPartialName(writePort,"address",true)
    dataBuffer.setPartialName(writePort,"writeData",true)

    if(maskBuffer != null) {
      maskBuffer.setPartialName(writePort,"mask", true)
    }

    val readBits = (if(allowMixedWidth) Bits() else Bits(getWidth bits)).dontSimplifyIt()
    val readWord = data.clone()
    val readPort = new MemReadWrite_readPart(this, addressBuffer, readBits, enableBuffer, writeBuffer, readUnderWrite, ClockDomain.current)
    if(allowMixedWidth) readPort.addTag(AllowMixedWidth)

    readBits.input = readPort
    readBits.setPartialName(readPort,"readData",true)

    readWord.assignFromBits(readBits)
    if (clockCrossing)
      readPort.addTag(crossClockDomain)


    writePort.readPart = readPort;
    readPort.writePart = writePort

    readPort.setPartialName(this,"port" + ports.length,true)
    writePort.setPartialName(this,"port" + ports.length,true)
    ports += MemWriteOrReadSync(writePort,readPort)

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
      case port : MemReadWrite_writePart => {
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

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClassIdentifier}[${getWidth} bits]"
}

class MemReadAsync(mem_ : Mem[_], address_ : UInt, data: Bits, val readUnderWrite: ReadUnderWritePolicy) extends Node with Widthable with CheckWidth with Nameable {
  if (readUnderWrite == readFirst) SpinalError("readFirst mode for asynchronous read is not allowed")

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  var address : Node with Widthable = address_
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
    case 0 => address = node.asInstanceOf[Node with Widthable]
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
  def getMem = mem
  override def calcWidth: Int = data.getWidth //getMem.getWidth >> (address.getWidth - mem.addressWidth)

  override private[core] def checkInferedWidth: Unit = {
    if(mem.getWidth != getWidth){
      if(!hasTag(AllowMixedWidth)) {
        PendingError(s"Read data width (${data.getWidth} bits) is not the same than the memory one ($mem) at\n${this.getScalaLocationLong}")
        return
      }
      if(mem.getWidth / getWidth * getWidth != mem.getWidth) {
        PendingError(s"The aspect ration between readed data and the memory should be a power of two. currently it's ${mem.getWidth}/${getWidth}. Memory : $mem, written at\n${this.getScalaLocationLong}")
        return
      }
    }

    if(address.getWidth != mem.addressWidth + log2Up(aspectRatio)) {
      PendingError(s"Address used to read $mem doesn't match the required width, ${address.getWidth} bits in place of ${mem.addressWidth + log2Up(aspectRatio)} bits\n${this.getScalaLocationLong}")
      return
    }
  }

  def aspectRatio = mem.getWidth/getWidth
}


object MemReadSync {
  val getAddressId: Int = 4
  val getEnableId: Int = 5
  val getMemId: Int = 6
}

class MemReadSync(mem_ : Mem[_], address_ : UInt, data: Bits, enable_ : Bool, val readUnderWrite: ReadUnderWritePolicy, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with CheckWidth with Nameable{
  var address : Node with Widthable = address_
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
    case MemReadSync.getAddressId => address = node.asInstanceOf[Node with Widthable]
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


  def useReadEnable: Boolean = {
    val lit = getReadEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }

  def sameAddressThan(write: MemWrite): Unit = {
    //Used by backed to symplify
    this.setInput(MemReadSync.getAddressId,write.getAddress)
  }

  override def calcWidth: Int = data.getWidth //getMem.getWidth >> (address.getWidth - mem.addressWidth)

  override private[core] def checkInferedWidth: Unit = {
    if(mem.getWidth != getWidth){
      if(!hasTag(AllowMixedWidth)) {
        PendingError(s"Read data width (${data.getWidth} bits) is not the same than the memory one ($mem) at\n${this.getScalaLocationLong}")
        return
      }
      if(mem.getWidth / getWidth * getWidth != mem.getWidth) {
        PendingError(s"The aspect ration between readed data and the memory should be a power of two. currently it's ${mem.getWidth}/${getWidth}. Memory : $mem, written at\n${this.getScalaLocationLong}")
        return
      }
    }

    if(address.getWidth != mem.addressWidth + log2Up(aspectRatio)) {
      PendingError(s"Address used to read $mem doesn't match the required width, ${address.getWidth} bits in place of ${mem.addressWidth + log2Up(aspectRatio)} bits\n${this.getScalaLocationLong}")
      return
    }

  }

  def aspectRatio = mem.getWidth/getWidth
}


object MemWrite {
  val getAddressId: Int = 4
  val getDataId: Int = 5
  val getMaskId: Int = 6
  val getEnableId: Int = 7
}

class MemWrite(mem: Mem[_], address_ : UInt, data_ : Bits, mask_ : Bits, enable_ : Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with CheckWidth with Nameable{
  var address  : Node with Widthable  = address_
  var data     : Node with Widthable = data_
  var mask     : Node with Widthable =  mask_
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
    case MemWrite.getAddressId => address = node.asInstanceOf[Node with Widthable]
    case MemWrite.getDataId => data = node.asInstanceOf[Node with Widthable]
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


  override def calcWidth: Int = data.getWidth

  def useWriteEnable: Boolean = {
    val lit = getEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }

  override private[core] def checkInferedWidth: Unit = {
    if(mem.getWidth != getWidth){
      if(!hasTag(AllowMixedWidth)) {
        PendingError(s"Write data width (${data.getWidth} bits) is not the same than the memory one ($mem) at\n${this.getScalaLocationLong}")
        return
      }
      if(mem.getWidth / getWidth * getWidth != mem.getWidth) {
        PendingError(s"The aspect ration between written data and the memory should be a power of two. currently it's ${mem.getWidth}/${getWidth}. Memory : $mem, written at\n${this.getScalaLocationLong}")
        return
      }
    }

    if(getMask != null && getData.getWidth % getMask.getWidth != 0) {
      PendingError(s"Memory write_data_width % write_data_mask_width != 0 at\n${this.getScalaLocationLong}")
      return
    }


    if(address.getWidth != mem.addressWidth + log2Up(aspectRatio)) {
      PendingError(s"Address used to write $mem doesn't match the required width, ${address.getWidth} bits in place of ${mem.addressWidth + log2Up(aspectRatio)} bits\n${this.getScalaLocationLong}")
      return
    }
  }

  def aspectRatio = mem.getWidth/getWidth
}

object MemReadWrite_writePart {
  val getAddressId: Int = 4
  val getDataId: Int = 5
  val getChipSelectId: Int = 6
  val getWriteEnableId: Int = 7
  val getMaskId: Int = 8
}

class MemReadWrite_writePart(mem: Mem[_], address_ : UInt, data_ : Bits, mask_ : Bits, chipSelect_ : Bool, writeEnable_ : Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with CheckWidth with Nameable{
  var address : Node with Widthable  = address_
  var data     : Node with Widthable = data_
  var mask     : Node with Widthable =  mask_
  var chipSelect   : Node = chipSelect_
  var writeEnable  : Node  = writeEnable_


  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemReadWrite_writePart.getAddressId)
    doThat(data,MemReadWrite_writePart.getDataId)
    doThat(chipSelect,MemReadWrite_writePart.getChipSelectId)
    doThat(writeEnable,MemReadWrite_writePart.getWriteEnableId)
    if(mask != null) doThat(mask,MemWrite.getMaskId)
  }

  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(data)
    doThat(chipSelect)
    doThat(writeEnable)
    if(mask != null) doThat(mask)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemReadWrite_writePart.getAddressId => address = node.asInstanceOf[Node with Widthable]
    case MemReadWrite_writePart.getDataId => data = node.asInstanceOf[Node with Widthable]
    case MemReadWrite_writePart.getChipSelectId => chipSelect = node
    case MemReadWrite_writePart.getWriteEnableId => writeEnable = node
    case MemReadWrite_writePart.getMaskId => mask = node.asInstanceOf[Node with Widthable]
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + 4 + (if(mask != null) 1 else 0)
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,data,chipSelect,writeEnable) ++ (if(mask != null) List(mask) else Nil)
  override def getInput(id: Int): Node = id match{
    case MemReadWrite_writePart.getAddressId => address
    case MemReadWrite_writePart.getDataId => data
    case MemReadWrite_writePart.getChipSelectId => chipSelect
    case MemReadWrite_writePart.getWriteEnableId => writeEnable
    case MemReadWrite_writePart.getMaskId => mask
    case _ => super.getInput(id)
  }

  var readPart: MemReadWrite_readPart = null

  override def getSynchronousInputs: List[Node] = {
    val base = getAddress :: getData :: getChipSelect :: getWriteEnable :: super.getSynchronousInputs
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
  def getChipSelect = chipSelect.asInstanceOf[Bool]
  def getWriteEnable = writeEnable.asInstanceOf[Bool]
  def getMask: Bits = {
    if (mask.isInstanceOf[Bits])
      mask.asInstanceOf[Bits]
    else
      null
  }

  override def calcWidth: Int = data.getWidth


  override private[core] def checkInferedWidth: Unit = {
    if(mem.getWidth != getWidth){
      if(!hasTag(AllowMixedWidth)) {
        PendingError(s"Write data width (${data.getWidth} bits) is not the same than the memory one ($mem) at\n${this.getScalaLocationLong}")
        return
      }
      if(mem.getWidth / getWidth * getWidth != mem.getWidth) {
        PendingError(s"The aspect ration between written data and the memory should be a power of two. currently it's ${mem.getWidth}/${getWidth}. Memory : $mem, written at\n${this.getScalaLocationLong}")
        return
      }
    }

    if(getMask != null && getData.getWidth % getMask.getWidth != 0) {
      PendingError(s"Memory write_data_width % write_data_mask_width != 0 at\n${this.getScalaLocationLong}")
      return
    }


    if(address.getWidth != mem.addressWidth + log2Up(aspectRatio)) {
      PendingError(s"Address used to write $mem doesn't match the required width, ${address.getWidth} bits in place of ${mem.addressWidth + log2Up(aspectRatio)} bits\n${this.getScalaLocationLong}")
      return
    }
  }

  def aspectRatio = mem.getWidth/getWidth
}


object MemReadWrite_readPart {
  val getAddressId: Int = 4
  val getChipSelectId: Int = 5
  val getWriteEnableId: Int = 6
  val getMemId: Int = 7
}

class MemReadWrite_readPart(mem_ : Mem[_], address_ : UInt, data_ : Bits, chipSelect_ : Bool, writeEnable_ : Bool, val readUnderWrite: ReadUnderWritePolicy, clockDomain: ClockDomain) extends SyncNode(clockDomain) with Widthable with CheckWidth with Nameable{

  var address : Node with Widthable  = address_
  var chipSelect     : Node = chipSelect_
  var writeEnable   : Node = writeEnable_
  var mem  : Mem[_]  = mem_


  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)


  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address,MemReadWrite_readPart.getAddressId)
    doThat(chipSelect,MemReadWrite_readPart.getChipSelectId)
    doThat(writeEnable,MemReadWrite_readPart.getWriteEnableId)
    doThat(mem,MemReadWrite_readPart.getMemId)
  }

  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(address)
    doThat(chipSelect)
    doThat(writeEnable)
    doThat(mem)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case MemReadWrite_readPart.getAddressId => address = node.asInstanceOf[Node with Widthable]
    case MemReadWrite_readPart.getChipSelectId => chipSelect = node
    case MemReadWrite_readPart.getWriteEnableId => writeEnable = node
    case MemReadWrite_readPart.getMemId => mem = node.asInstanceOf[Mem[_]]
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + 4
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(address,chipSelect,writeEnable,mem)
  override def getInput(id: Int): Node = id match{
    case MemReadWrite_readPart.getAddressId => address
    case MemReadWrite_readPart.getChipSelectId => chipSelect
    case MemReadWrite_readPart.getWriteEnableId => writeEnable
    case MemReadWrite_readPart.getMemId => mem
    case _ => super.getInput(id)
  }



  var writePart: MemReadWrite_writePart = null

  override def getSynchronousInputs: List[Node] = getMem :: getAddress :: getChipSelect :: getWriteEnable :: super.getSynchronousInputs

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false

  def getData = data_

  def getMem = mem
  def getAddress = address.asInstanceOf[UInt]
  def getChipSelect = chipSelect.asInstanceOf[Bool]
  def getWriteEnable = writeEnable.asInstanceOf[Bool]

  override def calcWidth: Int = writePart.getWidth


  override private[core] def checkInferedWidth: Unit = {
    if(mem.getWidth != getWidth){
      if(!hasTag(AllowMixedWidth)) {
        PendingError(s"Read data width (${getData.getWidth} bits) is not the same than the memory one ($mem) at\n${this.getScalaLocationLong}")
        return
      }
      if(mem.getWidth / getWidth * getWidth != mem.getWidth) {
        PendingError(s"The aspect ration between written data and the memory should be a power of two. currently it's ${mem.getWidth}/${getWidth}. Memory : $mem, read at\n${this.getScalaLocationLong}")
        return
      }
    }


    if(address.getWidth != mem.addressWidth + log2Up(aspectRatio)) {
      PendingError(s"Address used to read $mem doesn't match the required width, ${address.getWidth} bits in place of ${mem.addressWidth + log2Up(aspectRatio)} bits\n${this.getScalaLocationLong}")
      return
    }
  }

  def aspectRatio = mem.getWidth/getWidth
}


