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
  def addressTypeAt(initialValue: BigInt) = UInt(initialValue, addressWidth bit)

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

  def readSync(address: UInt, enable: Bool = Bool(true), writeToReadKind: MemWriteToReadKind = dontCare, crossClock: Boolean = false): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    val readPort = new MemReadSync(this,address, addressBuffer, readBits, enable.dontSimplifyIt, writeToReadKind, ClockDomain.current)
    readPort.compositeTagReady = readWord
    readPort.addTag(crossClockDomain)

    readBits.inputs(0) = readPort
    readWord.assignFromBits(readBits)

    readWord
  }

  def readSyncCC(address: UInt, enable: Bool = Bool(true), writeToReadKind: MemWriteToReadKind = dontCare): T = {
    readSync(address, enable, writeToReadKind, true)
  }

  def write(address: UInt, data: T): Unit = {
    val addressBuffer = UInt(addressWidth bit).dontSimplifyIt
    addressBuffer := address
    val dataBuffer = Bits(getWidth bit).dontSimplifyIt
    dataBuffer := data.toBits
    val writePort = new MemWrite(this,address, addressBuffer, dataBuffer, when.getWhensCond(this).dontSimplifyIt, ClockDomain.current)
    inputs += writePort
  }


  def writeReadSync(address: UInt, writeData: T, writeEnable: Bool, readEnable: Bool): Unit = {

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

class MemReadSync(mem: Mem[_],val originalAddress: UInt, address: UInt, data: Bits, enable: Bool, val writeToReadKind: MemWriteToReadKind, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
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

  def sameAddressThan(write : MemWrite): Unit ={ //Used by backed to symplify
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

class MemWrite(mem: Mem[_],val originalAddress: UInt, address: UInt, data: Bits, enable: Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
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


//object MemReadWrite {
//  def getAddressId: Int = 3
//  def getWriteDataId: Int = 4
//  def getWriteEnableId: Int = 5
//  def getReadEnableId: Int = 5
//}

//class MemReadWrite(mem: Mem[_], address: UInt, writeData: Bits, writeEnable: Bool, readEnable: Bool, clockDomain: ClockDomain) extends SyncNode(clockDomain) {
//  inputs += address
//  inputs += writeData
//  inputs += writeEnable
//
//
//  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getWriteData :: getWriteEnable :: getMem :: getReadEnable :: Nil
//  override def isUsingReset: Boolean = false
//
//  def getMem = mem
//  def getAddress = inputs(MemReadWrite.getAddressId).asInstanceOf[UInt]
//  def getWriteData = inputs(MemReadWrite.getWriteDataId).asInstanceOf[Bits]
//  def getWriteEnable = inputs(MemReadWrite.getWriteEnableId).asInstanceOf[Bool]
//  def getReadEnable = inputs(MemReadWrite.getReadEnableId).asInstanceOf[Bool]
//
//  override def calcWidth: Int = getMem.calcWidth
//
//  def useWriteEnable: Boolean = {
//    val lit = getWriteEnable.getLiteral[BoolLiteral]
//    return lit == null || lit.value == false
//  }
//}


