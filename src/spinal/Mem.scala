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

package spinal


import spinal.ImportMe._

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
  override def calcWidth: Int = wordType.getBitsWidth
  def addressWidth = log2Up(wordCount)

  def readAsync(address: UInt): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    val readPort = new MemReadAsync(this, address.dontSimplifyIt, readBits)

    readBits.inputs(0) = readPort
    readWord.fromBits(readBits)

    readWord
  }

  def readSync(address: UInt, enable: Bool = Bool(true)): T = {
    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    val readPort = new MemReadSync(this, address.dontSimplifyIt, readBits, enable.dontSimplifyIt)

    readBits.inputs(0) = readPort
    readWord.fromBits(readBits)

    readWord
  }


  def write(address: UInt, data: T): Unit = {

    val writePort = new MemWrite(this, address.dontSimplifyIt, data.toBits.dontSimplifyIt, when.getWhensCond(this).dontSimplifyIt)
    inputs += writePort
  }
}

class MemReadAsync(mem: Mem[_], address: UInt, data: Bits) extends Node {
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

class MemReadSync(mem: Mem[_], address: UInt, data: Bits, enable: Bool, clockDomain: ClockDomain = ClockDomain.current) extends DelayNode(clockDomain) {
  inputs += address
  inputs += enable
  inputs += mem


  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getEnable :: Nil
  override def isUsingReset: Boolean = false

  def getData = data
  def getMem = mem
  def getAddress = inputs(MemReadSync.getAddressId).asInstanceOf[UInt]
  def getEnable = inputs(MemReadSync.getEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.getWidth

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, MemReadSync.getAddressId, getMem.addressWidth)
  }

}


object MemWrite {
  def getAddressId: Int = 3
  def getDataId: Int = 4
  def getEnableId: Int = 5
}

class MemWrite(mem: Mem[_], address: UInt, data: Bits, enable: Bool, clockDomain: ClockDomain = ClockDomain.current) extends DelayNode(clockDomain) {
  inputs += address
  inputs += data
  inputs += enable


  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs ++= getAddress :: getData :: getEnable :: Nil
  override def isUsingReset: Boolean = false

  def getMem = mem
  def getAddress = inputs(MemWrite.getAddressId).asInstanceOf[UInt]
  def getData = inputs(MemWrite.getDataId).asInstanceOf[Bits]
  def getEnable = inputs(MemWrite.getEnableId).asInstanceOf[Bool]

  override def calcWidth: Int = getMem.getWidth

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, MemReadSync.getAddressId, getMem.addressWidth)
  }

}

class MemReadWrite extends Node {
  override def calcWidth: Int = ???
}



class Ram_1c_1w_1ra(wordWidth: Int, wordCount: Int) extends BlackBox {
  val generic = new Bundle {
    val wordCount = Number(Ram_1c_1w_1ra.this.wordCount)
    val wordWidth = Number(Ram_1c_1w_1ra.this.wordWidth)
  }

  val io = new Bundle {
    val clk = in.Bool()

    val wr = new Bundle {
      val en = in.Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  useCurrentClockDomain(io.clk)

  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data)
  }

  io.rd.data := mem.readAsync(io.rd.addr)
}

class Ram_1c_1w_1rs(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  val generic = new Bundle {
    val wordCount = Number(Ram_1c_1w_1rs.this.wordCount)
    val wordWidth = Number(Ram_1c_1w_1rs.this.wordWidth)
    val readToWriteKind = SString(writeToReadKind.toString)
  }

  val io = new Bundle {
    val clk = in.Bool()

    val wr = new Bundle {
      val en = in.Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en = in.Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  useCurrentClockDomain(io.clk)

  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data)
  }

  io.rd.data := mem.readSync(io.rd.addr, io.rd.en)
}
