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
trait MemWriteToReadKind {

}


object writeFirst extends MemWriteToReadKind {
  override def toString: String = "writeFirst"
}

object readFirst extends MemWriteToReadKind {
  override def toString: String = "readFirst"
}

object dontCare extends MemWriteToReadKind {
  override def toString: String = "dontCare"
}

object Mem {
  def apply[T <: Data](wordType: T, wordCount: Int) = new Mem(wordType, wordCount)
}


class Mem[T <: Data](val wordType: T, val wordCount: Int) extends Node with Nameable {
  var forceMemToBlackboxTranslation = false

  override def calcWidth: Int = wordType.flatten.map(_._2.calcWidth).reduceLeft(_ + _)
  def addressWidth = log2Up(wordCount)

  def setAsBlackBox: this.type = {
    forceMemToBlackboxTranslation = true
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
  def addressTypeAt(initialValue: BigInt) = u(initialValue, addressWidth bit)

  def readAsync(address: UInt, writeToReadKind: MemWriteToReadKind = dontCare): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    val readPort = new MemReadAsync(this, addressBuffer, readBits, writeToReadKind)
    readPort.compositeTagReady = readWord

    readBits.inputs(0) = readPort
    readWord.assignFromBits(readBits)

    readWord
  }

  def readSync(address: UInt, enable: Bool = True, writeToReadKind: MemWriteToReadKind = dontCare, crossClock: Boolean = false): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    val readPort = new MemReadSync(this, address, addressBuffer, readBits, enable.dontSimplifyIt, writeToReadKind, ClockDomain.current)
    readPort.compositeTagReady = readWord
    if(crossClock)
      readPort.addTag(crossClockDomain)

    readBits.inputs(0) = readPort
    readWord.assignFromBits(readBits)

    readWord
  }

  def readSyncCC(address: UInt, enable: Bool = True, writeToReadKind: MemWriteToReadKind = dontCare): T = {
    readSync(address, enable, writeToReadKind, true)
  }

  def write(address: UInt, data: T): Unit = {
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    val dataBuffer = Bits(getWidth bit).dontSimplifyIt
    dataBuffer := data.toBits
    val writePort = new MemWrite(this, address, addressBuffer, dataBuffer, when.getWhensCond(this).dontSimplifyIt, ClockDomain.current)
    inputs += writePort
  }

  //Asic friendly single port ram
  def writeOrReadSync(address: UInt, writeData: T, chipSelect : Bool,writeEnable : Bool, writeToReadKind: MemWriteToReadKind = dontCare,crossClock: Boolean = false): T = {
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    chipSelect.dontSimplifyIt
    writeEnable.dontSimplifyIt


    val dataBuffer = Bits(getWidth bit).dontSimplifyIt
    dataBuffer := writeData.toBits
    val writePort = new MemWriteOrRead_writePart(this, addressBuffer, dataBuffer, chipSelect,writeEnable, ClockDomain.current)
    inputs += writePort


    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()
    val readPort = new MemWriteOrRead_readPart(this, addressBuffer, readBits,  chipSelect,writeEnable, writeToReadKind, ClockDomain.current)
    readPort.compositeTagReady = readWord
    readBits.inputs(0) = readPort
    readWord.assignFromBits(readBits)
    if(crossClock)
      readPort.addTag(crossClockDomain)


    writePort.readPart = readPort;
    readPort.writePart = writePort

    readWord
  }
}

class MemReadAsync(mem: Mem[_], address: UInt, data: Bits, val writeToReadKind: MemWriteToReadKind) extends Node {
  if (writeToReadKind == readFirst) SpinalError("readFirst mode for asyncronous read is not alowed")

  inputs += address
  inputs += mem

  def getData = data
  def getAddress = inputs(0).asInstanceOf[UInt]
  def getMem = inputs(1).asInstanceOf[Mem[_]]

  override def calcWidth: Int = getMem.getWidth
}


object MemReadSync {
  def getAddressId: Int = 3
  def getEnableId: Int = 4
}

class MemReadSync(mem: Mem[_], val originalAddress: UInt, address: UInt, data: Bits, enable: Bool, val writeToReadKind: MemWriteToReadKind, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
  inputs += address
  inputs += enable
  inputs += mem

  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getMem :: getAddress :: getEnable :: Nil
  override def isUsingReset: Boolean = false

  def getData = data
  def getMem = mem
  def getAddress = inputs(MemReadSync.getAddressId).asInstanceOf[UInt]
  def getEnable = inputs(MemReadSync.getEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth

  def useReadEnable: Boolean = {
    val lit = getEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }

  def sameAddressThan(write: MemWrite): Unit = {
    //Used by backed to symplify
    inputs(MemReadSync.getAddressId) = write.getAddress
  }

  //  override def normalizeInputs: Unit = {
  //    Misc.normalizeResize(this, MemReadSync.getAddressId, getMem.addressWidth)
  //  }

}


object MemWrite {
  def getAddressId: Int = 3
  def getDataId: Int = 4
  def getEnableId: Int = 5
}

class MemWrite(mem: Mem[_], val originalAddress: UInt, address: UInt, data: Bits, enable: Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
  inputs += address
  inputs += data
  inputs += enable


  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getData :: getEnable :: Nil
  override def isUsingReset: Boolean = false

  def getMem = mem
  def getAddress = inputs(MemWrite.getAddressId).asInstanceOf[UInt]
  def getData = inputs(MemWrite.getDataId).asInstanceOf[Bits]
  def getEnable = inputs(MemWrite.getEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth

  def useWriteEnable: Boolean = {
    val lit = getEnable.getLiteral[BoolLiteral]
    return lit == null || lit.value == false
  }
}

object MemWriteOrRead_writePart {
  def getAddressId: Int = 3
  def getDataId: Int = 4
  def getChipSelectId: Int = 5
  def getWriteEnableId: Int = 6
}

class MemWriteOrRead_writePart(mem: Mem[_], address: UInt, data: Bits, chipSelect: Bool, writeEnable: Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
  inputs += address
  inputs += data
  inputs += chipSelect
  inputs += writeEnable

  var readPart: MemWriteOrRead_readPart = null

  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getData :: getChipSelect :: getWriteEnable :: Nil
  override def isUsingReset: Boolean = false

  def getMem = mem
  def getAddress = inputs(MemWriteOrRead_writePart.getAddressId).asInstanceOf[UInt]
  def getData = inputs(MemWriteOrRead_writePart.getDataId).asInstanceOf[Bits]
  def getChipSelect = inputs(MemWriteOrRead_writePart.getChipSelectId).asInstanceOf[Bool]
  def getWriteEnable = inputs(MemWriteOrRead_writePart.getWriteEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth

  //  def useWriteEnable: Boolean = {
  //    val lit = getEnable.getLiteral[BoolLiteral]
  //    return lit == null || lit.value == false
  //  }
}


object MemWriteOrRead_readPart {
  def getAddressId: Int = 3
  def getChipSelectId: Int = 4
  def getWriteEnableId: Int = 5
}

class MemWriteOrRead_readPart(mem: Mem[_], address: UInt, data: Bits, chipSelect: Bool, writeEnable: Bool, val writeToReadKind: MemWriteToReadKind, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
  inputs += address
  inputs += chipSelect
  inputs += writeEnable
  inputs += mem

  var writePart: MemWriteOrRead_writePart = null

  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getMem :: getAddress :: getChipSelect :: getWriteEnable :: Nil
  override def isUsingReset: Boolean = false

  def getData = data
  def getMem = mem
  def getAddress = inputs(MemWriteOrRead_readPart.getAddressId).asInstanceOf[UInt]
  def getChipSelect = inputs(MemWriteOrRead_readPart.getChipSelectId).asInstanceOf[Bool]
  def getWriteEnable = inputs(MemWriteOrRead_readPart.getWriteEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.calcWidth

  //  def useReadEnable: Boolean = {
  //    val lit = getEnable.getLiteral[BoolLiteral]
  //    return lit == null || lit.value == false
  //  }

}


//object MemReadOrWrite {
//  def getEnableId: Int = 3
//  def getWriteElseReadId: Int = 4
//  def getAddressId: Int = 5
//  def getWriteDataId: Int = 6
//
//}
//
//class MemWriteOrReadSync(mem: Mem[_], address: UInt, writeData: Bits, enable: Bool, writeElseRead: Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
//  inputs += enable
//  inputs += writeElseRead
//  inputs += address
//  inputs += writeData
//
//
//  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getWriteData :: getWriteEnable :: getMem :: getReadEnable :: Nil
//  override def isUsingReset: Boolean = false
//
//  def getMem = mem
//  def getAddress = inputs(MemReadOrWrite.getAddressId).asInstanceOf[UInt]
//  def getWriteData = inputs(MemReadOrWrite.getWriteDataId).asInstanceOf[Bits]
//  def getEnable = inputs(MemReadOrWrite.getEnableId).asInstanceOf[Bool]
//  def getWriteOrRead = inputs(MemReadOrWrite.getWriteElseReadId).asInstanceOf[Bool]
//
//  override def calcWidth: Int = getMem.calcWidth
//
//  def useWriteEnable: Boolean = {
//    val lit = getWriteEnable.getLiteral[BoolLiteral]
//    return lit == null || lit.value == false
//  }
//}


