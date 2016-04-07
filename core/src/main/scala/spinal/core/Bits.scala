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

/**
 * Created by PIC18F on 16.01.2015.
 */

trait BitsCast{
  def asBits(that : Data) : Bits = that.asBits
}

object BitsSet{
  def apply(bitCount: BitCount) = B((BigInt(1) << bitCount.value) - 1,bitCount)
}

trait BitsFactory{
  def Bits() = new Bits()
  def Bits(width: BitCount): Bits = Bits.setWidth(width.value)
}

class Bits extends BitVector {
  private[core] def prefix: String = "b"

  def ===(that : Bits) : Bool = this.isEguals(that)
  def =/=(that : Bits) : Bool = this.isNotEguals(that)
  def ===(that : MaskedLiteral) : Bool = this.isEguals(that)
  def =/=(that : MaskedLiteral) : Bool = this.isNotEguals(that)

  def ##(right: Bits): Bits = newBinaryOperator("b##b", right, WidthInfer.cumulateInputWidth, InputNormalize.none,ZeroWidth.binaryTakeOther)

  def |(that: Bits): Bits = newBinaryOperator("b|b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryTakeOther);
  def &(that: Bits): Bits = newBinaryOperator("b&b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryInductZeroWithOtherWidth(B.apply));
  def ^(that: Bits): Bits = newBinaryOperator("b^b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,ZeroWidth.binaryTakeOther);
  def unary_~(): Bits = newUnaryOperator("~b",WidthInfer.inputMaxWidth,ZeroWidth.unaryZero);

  def >>(that: Int): Bits = newBinaryOperator("b>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none,ZeroWidth.shiftRightImpl);
  def <<(that: Int): Bits = newBinaryOperator("b<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none,ZeroWidth.shiftLeftImpl(B.apply));
  def >>(that: UInt): Bits = newBinaryOperator("b>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none,ZeroWidth.shiftRightImpl);
  def <<(that: UInt): Bits = newBinaryOperator("b<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none,ZeroWidth.shiftLeftImpl(B.apply));
  def rotateLeft(that: UInt): Bits = newBinaryOperator("brotlu", that, WidthInfer.input0Width, InputNormalize.none,ZeroWidth.rotateImpl(B.apply));


  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,b,b)", sel, whenTrue, whenFalse)

  override def resize(width: Int): this.type = newResize("resize(b,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width,ZeroWidth.resizeImpl(B.apply))

  def asSInt: SInt = new SInt().castFrom("b->s", this)
  def asUInt: UInt = new UInt().castFrom("b->u", this)

  override def asBits: Bits = {
    val ret = new Bits()
    ret := this
    ret
  }
  override def assignFromBits(bits: Bits): Unit = this := bits
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = this(hi,lo).assignFromBits(bits)

  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: Bits => newLogicalOperator("b==b", that, InputNormalize.inputWidthMax,ZeroWidth.binaryThatIfBoth(True));
      case that : MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: Bits => newLogicalOperator("b!=b", that, InputNormalize.inputWidthMax,ZeroWidth.binaryThatIfBoth(False));
      case that : MaskedLiteral => that =/= this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  def toDataType[T <: Data](dataType : T) : T = {
    val ret = cloneOf(dataType)
    ret.assignFromBits(this)
    ret
  }



  override def getZero: this.type = B(0).asInstanceOf[this.type]
}