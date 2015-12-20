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

trait UIntCast{
  def toUInt(that : Bool) : UInt = that.toUInt
  def toUInt(that : Bits) : UInt = that.toUInt
  def toUInt(that : SInt) : UInt = that.toUInt
}

trait UIntFactory{
  def UInt() = new UInt()
//  def UInt(width : Int): UInt = UInt.setWidth(width)
  def UInt(width: BitCount): UInt = UInt.setWidth(width.value)
}

class UInt extends BitVector with Num[UInt] with MinMaxProvider {
  def prefix : String = "u"

  override def +(that: UInt): UInt = newBinaryOperator("u+u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryTakeOther);
  override def -(that: UInt): UInt = newBinaryOperator("u-u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryMinus(U.apply));
  override def *(that: UInt): UInt = newBinaryOperator("u*u", that, WidthInfer.cumulateInputWidth, InputNormalize.none,ZeroWidth.binaryInductZeroWithOtherWidth(U.apply));


  def |(that: UInt): UInt = newBinaryOperator("u|u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryTakeOther);
  def &(that: UInt): UInt = newBinaryOperator("u&u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryInductZeroWithOtherWidth(U.apply));
  def ^(that: UInt): UInt = newBinaryOperator("u^u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryTakeOther);
  def unary_~(): UInt = newUnaryOperator("~u",WidthInfer.inputMaxWidth,ZeroWidth.unaryZero);

  override def <(that: UInt): Bool = newLogicalOperator("u<u", that, InputNormalize.inputWidthMax,ZeroWidth.binaryUIntSmaller);
  override def >(that: UInt): Bool = that < this
  override def <=(that: UInt): Bool = newLogicalOperator("u<=u", that, InputNormalize.inputWidthMax,ZeroWidth.binaryUIntSmallerOrEgual);
  override def >=(that: UInt): Bool = that <= this


  override def >>(that: Int): UInt = newBinaryOperator("u>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none,ZeroWidth.shiftRightImpl);
  override def <<(that: Int): UInt = newBinaryOperator("u<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none,ZeroWidth.shiftLeftImpl(U.apply));
  def >>(that: UInt): UInt = newBinaryOperator("u>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none,ZeroWidth.shiftRightImpl);
  def <<(that: UInt): UInt = newBinaryOperator("u<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none,ZeroWidth.shiftLeftImpl(U.apply));

  override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,u,u)", sel, whenTrue, whenFalse)
  override def isEguals(that: Data): Bool = {
    that match {
      case that: UInt => newLogicalOperator("u==u", that, InputNormalize.inputWidthMax,ZeroWidth.binaryThatIfBoth(True));
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  override def isNotEguals(that: Data): Bool = {
    that match {
      case that: UInt => newLogicalOperator("u!=u", that, InputNormalize.inputWidthMax,ZeroWidth.binaryThatIfBoth(False));
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1


  override def resize(width: Int): this.type = newResize("resize(u,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width,ZeroWidth.resizeImpl(U.apply))



  def toSInt: SInt = new SInt().castFrom("u->s", this)
  override def toBits: Bits = new Bits().castFrom("u->b", this)
  override def assignFromBits(bits: Bits): Unit = this := bits.toUInt


  override def getZero: this.type = U(0).asInstanceOf[this.type]
}

object UInt2D{
  def apply(commonBitCount: BitCount) : UInt2D = UInt2D(commonBitCount,commonBitCount)
}

case class UInt2D(xBitCount: BitCount,yBitCount: BitCount) extends Bundle {
  val x = UInt(xBitCount)
  val y = UInt(yBitCount)
}

