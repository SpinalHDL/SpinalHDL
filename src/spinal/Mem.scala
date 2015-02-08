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

/**
 * Created by PIC18F on 02.02.2015.
 */
trait MemWriteToReadKind

object WriteFirst extends MemWriteToReadKind

object ReadFirst extends MemWriteToReadKind

object DontCare extends MemWriteToReadKind

class Mem[T <: Data](val wordType: T, val wordCount: Int) extends Node with Nameable {
  override def calcWidth: Int = wordType.getBitsWidth


  def read(address: UInt): T = {
    val readPort = new MemRead(this, address.dontSimplifyIt)

    val readBits = Bits(wordType.getBitsWidth bit)
    val readWord = wordType.clone()

    readBits.inputs(0) = readPort
    readWord.fromBits(readBits)

    readWord
  }

  def write(address: UInt, data: T): Unit = {

    val writePort = new MemWrite(this, address.dontSimplifyIt, data.toBits.dontSimplifyIt, when.getWhensCond.dontSimplifyIt)
    inputs += writePort
  }
}

class MemRead(mem: Mem[_], address: UInt) extends Node {
  inputs += address
  inputs += mem


  def getAddress = inputs(0)
  def getMem = inputs(1).asInstanceOf[Mem[_]]

  override def calcWidth: Int = getMem.getWidth

}

object MemWrite {
  def getAddressId: Int = 3
  def getDataId: Int = 4
  def getEnableId: Int = 5
}

class MemWrite(mem: Mem[_], address: UInt, data: Bits, enable: Bool, clockDomain: ClockDomain = ClockDomain.current) extends DelayNode {
  inputs += address
  inputs += data
  inputs += enable

  def getMem = mem
  def getAddress = inputs(MemWrite.getAddressId)
  def getData = inputs(MemWrite.getDataId)
  def getEnable = inputs(MemWrite.getEnableId)

  override def calcWidth: Int = data.getWidth

  override def normalizeInputs: Unit = {
    InputNormalize.memWriteImpl(this)
  }

}

class MemReadWrite extends Node {
  override def calcWidth: Int = ???
}

/*
val mem = Mem(Bits(2 bit),1024)
mem(28) := UInt(3) =>



 */

class RamDualPort(wordCount: Int, wordWidth: Int) extends BlackBox {
  val generic = new Bundle {
    val wordCount = Number(RamDualPort.this.wordCount)
    val wordWidth = Number(RamDualPort.this.wordWidth)
  }

  val io = new Bundle {
    val wr = new Bundle {
      val en = in.Bool()
      val addr = in Bits (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en = in.Bool()
      val addr = in Bits (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }

  }
}
