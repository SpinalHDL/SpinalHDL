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
 * Created by PIC18F on 21.08.2014.
 */

object SInt extends SIntFactory{
  def apply(that : Bool) : SInt = that.toSInt
  def apply(that : Bits) : SInt = that.toSInt
  def apply(that : UInt) : SInt = that.toSInt
}

class SIntFactory extends BitVectorFactory[SInt]{
  def apply() = new SInt()
}

class SInt extends BitVector with MinMaxProvider {
  override type SSelf = SInt
  def prefix : String = "s"

  def +(that: SInt): SInt = newBinaryOperator("s+s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth);
  def -(that: SInt): SInt = newBinaryOperator("s-s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth);
  def *(that: SInt): SInt = newBinaryOperator("s*s", that, WidthInfer.cumulateInputWidth,InputNormalize.none);

  def |(that: SInt): SInt = newBinaryOperator("s|s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth);
  def &(that: SInt): SInt = newBinaryOperator("s&s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth);
  def ^(that: SInt): SInt = newBinaryOperator("s^s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth);
  def unary_~(): SInt = newUnaryOperator("~s");
  def unary_-(): SInt = newUnaryOperator("-s");

  override def ===(that: SSelf): Bool = newLogicalOperator("s==s", that,InputNormalize.inputWidthMax);
  override def !==(that: SSelf): Bool = newLogicalOperator("s!=s", that,InputNormalize.inputWidthMax);
  def <(that: SInt): Bool = newLogicalOperator("s<s", that,InputNormalize.inputWidthMax);
  def >(that: SInt): Bool = that < this
  def <=(that: SInt): Bool = newLogicalOperator("s<=s", that,InputNormalize.inputWidthMax);
  def >=(that: SInt): Bool = that <= this


  def >>(that: Int): this.type = newBinaryOperator("s>>i", IntLiteral(that), WidthInfer.shiftRightWidth,InputNormalize.none);
  def <<(that: Int): this.type = newBinaryOperator("s<<i", IntLiteral(that), WidthInfer.shiftLeftWidth,InputNormalize.none);
  def >>(that: UInt): this.type = newBinaryOperator("s>>u", that, WidthInfer.shiftRightWidth,InputNormalize.none);
  def <<(that: UInt): this.type = newBinaryOperator("s<<u", that, WidthInfer.shiftLeftWidth,InputNormalize.none);

  override def \(that: SSelf) = super.\(that)
  override def :=(that: SSelf): Unit = super.:=(that)
  override def <>(that: SSelf): Unit = super.<>(that)

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,s,s)",sel,whenTrue,whenFalse)
  override def isEguals(that: Data): Bool = {
    that match{
      case that : SInt => this === that
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def toBits: Bits = new Bits().castFrom("s->b", this)
  override def assignFromBits(bits: Bits) : Unit = this := bits.toSInt

  def toUInt: UInt = new UInt().castFrom("s->u", this)


  override def resize(width: Int): this.type = newResize("resize(s,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width)


  override def minValue: BigInt = -(BigInt(1) << (getWidth - 1))
  override def maxValue: BigInt = (BigInt(1) << (getWidth - 1)) - 1


  override def getZero: this.type = s(0).asInstanceOf[this.type]
}
