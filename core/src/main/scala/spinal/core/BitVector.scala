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

abstract class BitVector extends BaseType {
  private[core] var fixedWidth = -1

  def high = getWidth - 1
  def msb = this (high)
  def lsb = this (0)
  def range = 0 until getWidth
  def orR = this.asBits =/= 0
  def andR = this.asBits === (BigInt(1) << getWidth - 1)
  def xorR = this.asBools.reduce(_ ^ _)

  private[core] def isFixedWidth = fixedWidth != -1

  def setWidth(width: Int): this.type = {
    fixedWidth = width
    if (globalData.nodeAreInferringWidth) inferredWidth = fixedWidth
    this
  }

  override def clone: this.type = {
    val res = super.clone
    res.fixedWidth = this.fixedWidth
    res
  }

  private[core] override def normalizeInputs: Unit = InputNormalize.bitVectoreAssignement(this, 0, this.getWidth)

  def resize(width: Int): this.type

  private[core] override def calcWidth: Int = {
    if (isFixedWidth) return fixedWidth
    if (inputs(0) == null) return -1
    return inputs(0).getWidth
  }

  def asBools: Vec[Bool] = {
    val vec = ArrayBuffer[Bool]()
    val bitCount = getWidth
    if (bitCount == -1) SpinalError("Can't convert to bools a Bit that has unspecified width value")
    for (i <- 0 until bitCount) vec += this (i)
    Vec(vec)
  }

  //extract bit
  def apply(bitId: Int): Bool = {
    val extract = new ExtractBoolFixed(s"extract($prefix,i)", this, bitId)
    val bool = new Bool
    bool.setInput(extract)

    bool.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
        case that: Bool => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, that, bitId), true)
        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
      }
    }

    bool
  }


  //extract bit
  def apply(bitId: UInt): Bool = {
    val extract = new ExtractBoolFloating(s"extract($prefix,u)", this, bitId)
    val bool = new Bool
    bool.setInput(extract)

    bool.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
        case that: Bool => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, that, bitId), true)
        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
      }
    }

    bool
  }

  //extract bits     that(8,2)
  def extract(hi: Int, lo: Int): this.type = {
    if (hi - lo + 1 != 0) {
      val ret = addTypeNodeFrom(new ExtractBitsVectorFixed(s"extract($prefix,i,i)", this, hi, lo))
      ret.compositeAssign = new Assignable {
        override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
          case that: BitVector => BitVector.this.assignFrom(new RangedAssignmentFixed(BitVector.this, that, hi, lo), true)
          case that: DontCareNode => BitVector.this.assignFrom(new RangedAssignmentFixed(BitVector.this, new DontCareNodeFixed(BitVector.this, hi - lo + 1), hi, lo), true)
          case that: BitAssignmentFixed => BitVector.this.apply(lo + that.getBitId).assignFrom(that.getInput, true)
          case that: BitAssignmentFloating => BitVector.this.apply(lo + that.getBitId.asInstanceOf[UInt]).assignFrom(that.getInput, true)
          case that: RangedAssignmentFixed => BitVector.this.apply(lo + that.getHi, lo + that.getLo).assignFrom(that.getInput, true)
          case that: RangedAssignmentFloating => BitVector.this.apply(lo + that.getOffset.asInstanceOf[UInt], that.getBitCount).assignFrom(that.getInput, true)
        }
      }
      ret
    }
    else
      getZero
  }

  def extract(offset: UInt, bitCount: BitCount): this.type = {
    if (bitCount.value != 0) {
      val ret = addTypeNodeFrom(new ExtractBitsVectorFloating(s"extract($prefix,u,w)", this, offset, bitCount))

      ret.compositeAssign = new Assignable {
        override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
          case that: BitVector => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, that, offset, bitCount), true)
          case that: DontCareNode => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, new DontCareNodeFixed(BitVector.this, bitCount.value), offset, bitCount), true)
          case that: BitAssignmentFixed => BitVector.this.apply(offset + that.getBitId).assignFrom(that.getInput, true)
          case that: BitAssignmentFloating => BitVector.this.apply(offset + that.getBitId.asInstanceOf[UInt]).assignFrom(that.getInput, true)
          case that: RangedAssignmentFixed => BitVector.this.apply(offset + that.getLo, that.getHi - that.getLo + 1 bit).assignFrom(that.getInput, true)
          case that: RangedAssignmentFloating => BitVector.this.apply(offset + that.getOffset.asInstanceOf[UInt], that.getBitCount).assignFrom(that.getInput, true)
        }
      }

      ret
    }
    else
      getZero
  }

  //extract bits     that(5,7 bit)
  def apply(offset: Int, bitCount: BitCount): this.type = this.apply(bitCount.value + offset - 1, offset)

  def apply(offset: UInt, bitCount: BitCount): this.type = this.extract(offset, bitCount)

  def apply(hi: Int, lo: Int): this.type = this.extract(hi, lo)

  def apply(range: Range): this.type = this.extract(range.last, range.head)

  def setAllTo(value: Boolean) = {
    val litBt = weakClone
    litBt.inputs(0) = new BitsAllToLiteral(this, value)
    this := litBt
  }

  def setAll() = setAllTo(true)

  def clearAll() = setAllTo(false)

  private[core] override def addTypeNodeFrom(node: Node): this.type = {
    val typeNode = super.addTypeNodeFrom(node)
    typeNode.fixedWidth = -1
    typeNode
  }

  private[core] def prefix: String


  def getWidthNoInferation: Int = {
    if (inferredWidth != -1)
      inferredWidth
    else
      fixedWidth
  }
  def getWidthStringNoInferation : String = {
    val width = getWidthNoInferation
    if(width == -1) return "?"
    else width.toString
  }
  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClassIdentifier}[${getWidthStringNoInferation} bit]"

}