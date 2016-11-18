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

import spinal.core.Operator.BitVector.AllByBool
/**
  * Created by PIC18F on 16.01.2015.
  */

trait BitsCast {
  @deprecated
  def asBits(that: Data): Bits = that.asBits
}

object BitsSet {
  def apply(bitCount: BitCount) = B((BigInt(1) << bitCount.value) - 1, bitCount)
}

trait BitsFactory {
  def Bits() = new Bits()

  def Bits(width: BitCount): Bits = Bits.setWidth(width.value)
}

class Bits extends BitVector with DataPrimitives[Bits] with BitwiseOp[Bits]{
  private[core] def prefix: String = "b"


  override type T = Bits

  override private[spinal] def _data: Bits = this

  def ===(that: MaskedLiteral): Bool = this.isEquals(that)
  def =/=(that: MaskedLiteral): Bool = this.isNotEquals(that)
  def ##(right: Bits): Bits = wrapBinaryOperator(right,new Operator.Bits.Cat)
  def |(right: Bits) : Bits = wrapBinaryOperator(right,new Operator.Bits.Or)
  def &(right: Bits) : Bits = wrapBinaryOperator(right,new Operator.Bits.And)
  def ^(right: Bits) : Bits = wrapBinaryOperator(right,new Operator.Bits.Xor)
  def unary_~(): Bits = wrapUnaryOperator(new Operator.Bits.Not)
  def >>(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftRightByInt(that))
  def <<(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftLeftByInt(that))
  def >>(that: UInt): Bits = wrapBinaryOperator(that,new Operator.Bits.ShiftRightByUInt)
  def <<(that: UInt): Bits = wrapBinaryOperator(that,new Operator.Bits.ShiftLeftByUInt)

  def |>>(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftRightByIntFixedWidth(that))
  def |<<(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftLeftByIntFixedWidth(that))
  def |>>(that: UInt): Bits = this >> that
  def |<<(that: UInt): Bits = wrapBinaryOperator(that,new Operator.Bits.ShiftLeftByUIntFixedWidth)




  override def rotateLeft(that: Int): Bits = {
    val width = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) ## this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): Bits = {
    val width = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) ## this(this.high downto thatMod)
  }

  def :=(rangesValue : Tuple2[Any,Any],_rangesValues: Tuple2[Any,Any]*) : Unit = {
    val rangesValues = rangesValue +: _rangesValues
    B.applyTupples(this,rangesValues)
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerBits)

  override protected def getAllToBoolNode(): AllByBool = new Operator.Bits.AllByBool(this)

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node = new ResizeBits
    node.input = this
    node.size = width
    node
  })

  /**
    * Resize by keeping MSB at the same place
    * If the final size is bigger than the original size, the leftmost bits are filled with zeroes
    * if the final size is smaller, only width MSB are kept
    * @param width Final width
    * @return Resized bits vector
    */
  def resizeLeft(width: Int): Bits = {
    val lengthDifference = width - this.getWidth
    if (lengthDifference >= 0) {
      this ## B(0, lengthDifference bits)
    } else {
      this(width - 1 downto 0)
    }
  }

  def asSInt: SInt = wrapCast(SInt(),new CastBitsToSInt)
  def asUInt: UInt = wrapCast(UInt(),new CastBitsToUInt)

  override def asBits: Bits = {
    val ret = new Bits()
    ret := this
    ret
  }

  override def assignFromBits(bits: Bits): Unit = this := bits

  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = this (hi, lo).assignFromBits(bits)

  private[core] override def isEquals(that: Any): Bool = {
    that match {
      case that: Bits => wrapLogicalOperator(that,new Operator.Bits.Equal)
      case that: MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how to compare $this with $that"); null
    }
  }

  private[core] override def isNotEquals(that: Any): Bool = {
    that match {
      case that: Bits => wrapLogicalOperator(that,new Operator.Bits.NotEqual)
      case that: MaskedLiteral => that =/= this
      case _ => SpinalError(s"Don't know how to compare $this with $that"); null
    }
  }

  def toDataType[T <: Data](dataType: T): T = {
    val ret = cloneOf(dataType)
    ret.assignFromBits(this)
    ret
  }




  def apply(bitId: Int) : Bool = newExtract(bitId,new ExtractBoolFixedFromBits)
  def apply(bitId: UInt): Bool = newExtract(bitId,new ExtractBoolFloatingFromBits)
  def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1,offset,new ExtractBitsVectorFixedFromBits).setWidth(bitCount.value)
  def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset,bitCount.value,new ExtractBitsVectorFloatingFromBits).setWidth(bitCount.value)

  override private[core] def weakClone: this.type = new Bits().asInstanceOf[this.type]
  override def getZero: this.type = B(0,this.getWidth bits).asInstanceOf[this.type]
  override def getZeroUnconstrained: this.type = B(0).asInstanceOf[this.type]
}