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

trait UIntCast{
  @deprecated
  def asUInt(that : Bool) : UInt = that.asUInt
  @deprecated
  def asUInt(that : Bits) : UInt = that.asUInt
  @deprecated
  def asUInt(that : SInt) : UInt = that.asUInt
  @deprecated
  def toUInt(that : UFix) : UInt = that.toUInt
}

trait UIntFactory{
  def UInt() = new UInt()
//  def UInt(width : Int): UInt = UInt.setWidth(width)
  def UInt(width: BitCount): UInt = UInt().setWidth(width.value)
}

class UInt extends BitVector with Num[UInt] with MinMaxProvider with DataPrimitives[UInt] with BitwiseOp[UInt]{
  private[core] def prefix : String = "u"

  override type T = UInt
  override def _data: UInt = this

  //TODO width assert
  def assignMask(maskedLiteral: MaskedLiteral): Unit ={
    var (literal,careAbout) = (maskedLiteral.value,maskedLiteral.careAbout)
    var offset = 0
    var value = careAbout.testBit(0)
    while(offset != maskedLiteral.width){
      var bitCount = 0
      while(offset + bitCount != maskedLiteral.width && careAbout.testBit(offset + bitCount) == value){
        bitCount += 1
      }
      if(value){
        this(offset,bitCount bit) := U((literal >> offset) & ((BigInt(1)<<bitCount)-1))
      }else{
        this(offset,bitCount bit).assignDontCare()
      }
      value = !value
      offset += bitCount
    }
  }

  def ===(that : MaskedLiteral) : Bool = this.isEguals(that)
  def =/=(that : MaskedLiteral) : Bool = this.isNotEguals(that)

  def @@(that : UInt) : UInt = (this ## that).asUInt
  def @@(that : Bool) : UInt = (this ## that).asUInt


  def twoComplement(enable : Bool): SInt = ((False ## Mux(enable,~this,this)).asUInt + enable.asUInt).asSInt

  override def +(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Add)
  override def -(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Sub)
  override def *(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Mul)
  override def /(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Div)
  override def %(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Mod)

//  def +!(right: UInt): UInt = this.resize(this.getWidth + 1) + right.resize(right.getWidth + 1)
//  def -!(right: UInt): UInt = this.resize(this.getWidth + 1) - right.resize(right.getWidth + 1)

  def |(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Or)
  def &(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.And)
  def ^(right: UInt): UInt = wrapBinaryOperator(right,new Operator.UInt.Xor)
  def unary_~(): UInt = wrapUnaryOperator(new Operator.UInt.Not);

  override def < (right: UInt): Bool = wrapLogicalOperator(right,new Operator.UInt.Smaller)
  override def > (right: UInt): Bool = right < this
  override def <=(right: UInt): Bool = wrapLogicalOperator(right,new Operator.UInt.SmallerOrEqual)
  override def >=(right: UInt): Bool = right <= this


  override def >>(that: Int): UInt = wrapConstantOperator(new Operator.UInt.ShiftRightByInt(that))
  override def <<(that: Int): UInt = wrapConstantOperator(new Operator.UInt.ShiftLeftByInt(that))
  def >>(that: UInt): UInt         = wrapBinaryOperator(that,new Operator.UInt.ShiftRightByUInt)
  def <<(that: UInt): UInt         = wrapBinaryOperator(that,new Operator.UInt.ShiftLeftByUInt)

  def |>>(that: Int): UInt  = wrapConstantOperator(new Operator.UInt.ShiftRightByIntFixedWidth(that))
  def |<<(that: Int): UInt  = wrapConstantOperator(new Operator.UInt.ShiftLeftByIntFixedWidth(that))
  def |>>(that: UInt): UInt = this >> that
  def |<<(that: UInt): UInt = wrapBinaryOperator(that,new Operator.UInt.ShiftLeftByUIntFixedWidth)


  override def rotateLeft(that: Int): UInt = {
    val width = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) @@ this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): UInt = {
    val width = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) @@ this(this.high downto thatMod)
  }

  def :=(rangesValue : Tuple2[Any,Any],_rangesValues: Tuple2[Any,Any]*) : Unit = {
    val rangesValues = rangesValue +: _rangesValues
    U.applyTupples(this,rangesValues)
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerUInt)
  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: UInt =>  wrapLogicalOperator(that,new Operator.UInt.Equal)
      case that : MaskedLiteral => that === this
      case that : Int => this === that
      case that : BigInt => this === that
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: UInt =>  wrapLogicalOperator(that,new Operator.UInt.NotEqual)
      case that : MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node = new ResizeUInt
    node.input = this
    node.size = width
    node
  })


  def asSInt: SInt = wrapCast(SInt(),new CastUIntToSInt)
  override def asBits: Bits = wrapCast(Bits(),new CastUIntToBits)
  override def assignFromBits(bits: Bits): Unit = this := bits.asUInt
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = this(hi,lo).assignFromBits(bits)

  def apply(bitId: Int) : Bool = newExtract(bitId,new ExtractBoolFixedFromUInt)
  def apply(bitId: UInt): Bool = newExtract(bitId,new ExtractBoolFloatingFromUInt)
  def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1,offset,new ExtractBitsVectorFixedFromUInt)
  def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset,bitCount.value,new ExtractBitsVectorFloatingFromUInt)

  override private[core] def weakClone: this.type = new UInt().asInstanceOf[this.type]
  override def getZero: this.type = U(0,this.getWidth bits).asInstanceOf[this.type]
  override def getZeroUnconstrained: this.type = U(0).asInstanceOf[this.type]
  override protected def getAllToBoolNode(): AllByBool = new Operator.UInt.AllByBool(this)
}

object UInt2D{
  def apply(commonBitCount: BitCount) : UInt2D = UInt2D(commonBitCount,commonBitCount)
}

case class UInt2D(xBitCount: BitCount,yBitCount: BitCount) extends Bundle {
  val x = UInt(xBitCount)
  val y = UInt(yBitCount)
}

