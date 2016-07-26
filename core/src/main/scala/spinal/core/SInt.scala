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
import spinal.core.Operator.SInt

/**
 * Created by PIC18F on 21.08.2014.
 */

trait SIntCast{
  def asSInt(that : Bool) : SInt = that.asSInt
  def asSInt(that : Bits) : SInt = that.asSInt
  def asSInt(that : UInt) : SInt = that.asSInt
  def toSInt(that : SFix) : SInt = that.toSInt
}

trait SIntFactory{
  def SInt() = new SInt()
  def SInt(width: BitCount): SInt = SInt.setWidth(width.value)
}

class SInt extends BitVector with Num[SInt] with MinMaxProvider with DataPrimitives[SInt] {
  private[core] def prefix : String = "s"


  override private[spinal] def _data: SInt = this

  def @@(that : SInt) : SInt = (this ## that).asSInt
  def @@(that : UInt) : SInt = (this ## that).asSInt
  def @@(that : Bool) : SInt = (this ## that).asSInt

  def ===(that : MaskedLiteral) : Bool = this.isEguals(that)
  def =/=(that : MaskedLiteral) : Bool = this.isNotEguals(that)

  override def +(right : SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Add)
  override def -(right : SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Sub)
  override def *(right : SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Mul)
  override def /(right : SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Div)
  override def %(right : SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Mod)

//  def +!(right: SInt): SInt = this.resize(this.getWidth + 1) + right.resize(right.getWidth + 1)
//  def -!(right: SInt): SInt = this.resize(this.getWidth + 1) - right.resize(right.getWidth + 1)

  def abs: UInt = Mux(this.msb,~this,this).asUInt + this.msb.asUInt
  def abs(enable : Bool): UInt = Mux(this.msb && enable,~this,this).asUInt + (this.msb && enable).asUInt

  def |(right: SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Or)
  def &(right: SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.And)
  def ^(right: SInt): SInt = wrapBinaryOperator(right,new Operator.SInt.Xor)
  def unary_~(): SInt = wrapUnaryOperator(new Operator.SInt.Not);
  def unary_-(): SInt = wrapUnaryOperator(new Operator.SInt.Minus);


  override def < (right : SInt): Bool = wrapLogicalOperator(right,new Operator.SInt.Smaller)
  override def > (right : SInt): Bool = right < this
  override def <=(right : SInt): Bool = wrapLogicalOperator(right,new Operator.SInt.SmallerOrEqual)
  override def >=(right : SInt): Bool = right <= this

  override def >>(that: Int): SInt = wrapConstantOperator(new Operator.SInt.ShiftRightByInt(that))
  override def <<(that: Int): SInt = wrapConstantOperator(new Operator.SInt.ShiftLeftByInt(that))
  def >>(that: UInt): SInt         = wrapBinaryOperator(that,new Operator.SInt.ShiftRightByUInt)
  def <<(that: UInt): SInt         = wrapBinaryOperator(that,new Operator.SInt.ShiftLeftByUInt)

  def :=(rangesValue : Tuple2[Any,Any],_rangesValues: Tuple2[Any,Any]*) : Unit = {
    val rangesValues = rangesValue +: _rangesValues
    S.applyTupples(this,rangesValues)
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerSInt)
  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: SInt =>  wrapLogicalOperator(that,new Operator.SInt.Equal)
      case that : MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: SInt =>  wrapLogicalOperator(that,new Operator.SInt.NotEqual)
      case that : MaskedLiteral => that =/= this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def asBits: Bits = wrapCast(Bits(),new CastSIntToBits)
  override def assignFromBits(bits: Bits) : Unit = this := bits.asSInt
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = this(hi,lo).assignFromBits(bits)

  def asUInt: UInt = wrapCast(UInt(),new CastSIntToUInt)


  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node = new ResizeSInt
    node.input = this
    node.size = width
    node
  })

  override def minValue: BigInt = -(BigInt(1) << (getWidth - 1))
  override def maxValue: BigInt = (BigInt(1) << (getWidth - 1)) - 1


  def apply(bitId: Int) : Bool = newExtract(bitId,new ExtractBoolFixedFromSInt)
  def apply(bitId: UInt): Bool = newExtract(bitId,new ExtractBoolFloatingFromSInt)
  def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1,offset,new ExtractBitsVectorFixedFromSInt)
  def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset,bitCount.value,new ExtractBitsVectorFloatingFromSInt)

  override private[core] def weakClone: this.type = new SInt().asInstanceOf[this.type]
  override def getZero: this.type = S(0,this.getWidth bits).asInstanceOf[this.type]
  override def getZeroUnconstrained: this.type = S(0).asInstanceOf[this.type]
  override protected def getAllToBoolNode(): AllByBool = new Operator.SInt.AllByBool(this)
}
