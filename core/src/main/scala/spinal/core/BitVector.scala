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

abstract class BitVector extends BaseType with Widthable with CheckWidth {
  private[core] var fixedWidth = -1
  type T <: BitVector

  def high = getWidth - 1
  def msb = this (high)
  def lsb = this (0)
  def range = 0 until getWidth
  def orR = this.asBits =/= 0
  def andR = this.asBits === ((BigInt(1) << getWidth) - 1)
  def xorR = this.asBools.reduce(_ ^ _)

  def rotateLeft(that: Int): T
  def rotateRight(that: Int): T

  def rotateLeft(that: UInt): T = {
    val thatWidth = widthOf(that)
    val thisWidth = widthOf(this)
    require(thatWidth <= log2Up(thisWidth))
    var result = cloneOf(this).asInstanceOf[T]
    result := this.asInstanceOf[T]
    for(i <- that.range){
      result \= (that(i) ? result.rotateLeft(1<<i).asInstanceOf[T] | result)
    }
    result
  }

  def rotateRight(that: UInt): T = {
    val thatWidth = widthOf(that)
    val thisWidth = widthOf(this)
    require(thatWidth <= log2Up(thisWidth))
    var result = cloneOf(this).asInstanceOf[T]
    result := this.asInstanceOf[T]
    for(i <- that.range){
      result \= (that(i) ? result.rotateRight(1<<i).asInstanceOf[T] | result)
    }
    result
  }

  private[core] def isFixedWidth = fixedWidth != -1
  private[core] def unfixWidth() = {
    fixedWidth = -1
    widthWhenNotInferred = -1
    inferredWidth = -1
  }

  def setWidth(width: Int): this.type = {
    if(width < 0){
      LocatedPendingError(s"Width of $this is set by a negative number")
    }
    fixedWidth = width
    if (globalData.nodeAreInferringWidth) inferredWidth = fixedWidth
    this
  }

  override def getBitsWidth: Int = getWidth

  override def clone: this.type = {
    val res = super.clone
    res.fixedWidth = this.fixedWidth
    res
  }

  private[core] override def normalizeInputs: Unit = InputNormalize.resizedOrUnfixedLit(this, 0, this.getWidth)

  def resize(width: Int): this.type



  private[core] override def calcWidth: Int = {
    if (isFixedWidth) return fixedWidth
    if (input == null) return -1
    return input.asInstanceOf[WidthProvider].getWidth
  }

  def asBools: Vec[Bool] = {
    val vec = ArrayBuffer[Bool]()
    val bitCount = getWidth
    if (bitCount == -1) SpinalError("Can't convert to bools a Bit that has unspecified width value")
    for (i <- 0 until bitCount) vec += this (i)
    Vec(vec)
  }

  def subdivideIn(sliceCount : SlicesCount) : Vec[T] = {
    require(this.getWidth % sliceCount.value == 0)
    val sliceWidth = widthOf(this)/sliceCount.value
    Vec((0 until sliceCount.value).map(i =>this(i*sliceWidth,sliceWidth bits).asInstanceOf[T]))
  }

  def subdivideIn(sliceWidth : BitCount) : Vec[T] = {
    require(this.getWidth % sliceWidth.value == 0)
    subdivideIn(this.getWidth / sliceWidth.value slices)
  }

  //extract bit
  def newExtract(bitId : Int,extract : ExtractBoolFixed): Bool = {
    extract.input = this
    extract.bitId = bitId
    val bool = new Bool
    bool.input = extract

    bool.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
        case that: Bool => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, that, bitId), true)
        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
      }
    }

    bool
  }


  //extract bit
  def newExtract(bitId: UInt,extract: ExtractBoolFloating): Bool = {
    extract.input = this
    extract.bitId = bitId
    val bool = new Bool
    bool.input = extract

    bool.compositeAssign = new Assignable {
      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
        case that: Bool => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, that, bitId), true)
        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
      }
    }

    bool
  }

  //extract bits     that(8,2)
  def newExtract(hi: Int, lo: Int,extract: ExtractBitsVectorFixed): this.type = {
    if (hi - lo + 1 != 0) {
      extract.input = this
      extract.hi = hi
      extract.lo = lo
      extract.checkHiLo
      val ret = wrapWithWeakClone(extract)
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
      getZeroUnconstrained
  }

  def newExtract(offset: UInt, size: Int,extract : ExtractBitsVectorFloating): this.type = {
    if (size != 0) {
      extract.input = this
      extract.size = size
      extract.offset = offset
      offset.dontSimplifyIt()
      val ret = wrapWithWeakClone(extract)
      ret.compositeAssign = new Assignable {
        override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
          case that: BitVector => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, that, offset, size bit), true)
          case that: DontCareNode => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, new DontCareNodeFixed(BitVector.this, size), offset, size bit), true)
          case that: BitAssignmentFixed => BitVector.this.apply(offset + that.getBitId).assignFrom(that.getInput, true)
          case that: BitAssignmentFloating => BitVector.this.apply(offset + that.getBitId.asInstanceOf[UInt]).assignFrom(that.getInput, true)
          case that: RangedAssignmentFixed => BitVector.this.apply(offset + that.getLo, that.getHi - that.getLo + 1 bit).assignFrom(that.getInput, true)
          case that: RangedAssignmentFloating => BitVector.this.apply(offset + that.getOffset.asInstanceOf[UInt], that.getBitCount).assignFrom(that.getInput, true)
        }
      }
      ret
    }
    else
      getZeroUnconstrained
  }

  def getZeroUnconstrained() : this.type

  def apply(bitId: Int) : Bool
  def apply(bitId: UInt): Bool
  def apply(offset: Int, bitCount: BitCount): this.type
  def apply(offset: UInt, bitCount: BitCount): this.type

  def apply(hi: Int, lo: Int): this.type = this.apply(lo, hi-lo+1 bit)
  def apply(range: Range): this.type = this.apply(range.high,range.low)

  def setAllTo(value: Boolean) : Unit = {
    val litBt = weakClone
    litBt.input = new BitsAllToLiteral(this, value)
    this := litBt
  }

  protected def getAllToBoolNode() : Operator.BitVector.AllByBool
  def setAllTo(value: Bool) : Unit = {
    val litBt = weakClone
    val node = getAllToBoolNode()
    node.input = value.asInstanceOf[node.T]
    litBt.input = node
    this := litBt
  }

  def setAll() = setAllTo(true)

  def clearAll() = setAllTo(false)

  private[core] override def wrapWithWeakClone(node: Node): this.type = {
    val typeNode = super.wrapWithWeakClone(node)
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

  override private[core] def checkInferedWidth: Unit = {
    val input = this.input
    if (input != null && input.component != null && this.getWidth != input.asInstanceOf[WidthProvider].getWidth) {
      PendingError(s"Assignment bit count mismatch. ${this} := ${input} at \n${ScalaLocated.long(getAssignementContext(0))}")
    }
  }


  override def assignDontCare(): this.type = {
    this.assignFrom(new DontCareNodeInfered(this), false)
    this
  }

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClassIdentifier}[${getWidthStringNoInferation} bits]"
}