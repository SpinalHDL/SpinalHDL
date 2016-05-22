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

class SInt extends BitVector with Num[SInt] with MinMaxProvider {
  private[core] def prefix : String = "s"

  def ===(that : SInt) : Bool = this.isEguals(that)
  def =/=(that : SInt) : Bool = this.isNotEguals(that)
  def ===(that : MaskedLiteral) : Bool = this.isEguals(that)
  def =/=(that : MaskedLiteral) : Bool = this.isNotEguals(that)

  override def +(that: SInt): SInt = newBinaryOperator("s+s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  override def -(that: SInt): SInt = newBinaryOperator("s-s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth,SymplifyNode.binaryMinus(S.apply));
  override def *(that: SInt): SInt = newBinaryOperator("s*s", that, WidthInfer.cumulateInputWidth,InputNormalize.none,SymplifyNode.binaryInductZeroWithOtherWidth(S.apply));
  override def /(that: SInt): SInt = newBinaryOperator("s/s", that, WidthInfer.input0Width, InputNormalize.none,SymplifyNode.signedDivImpl);
  override def %(that: SInt): SInt = newBinaryOperator("s%s", that, WidthInfer.input0Width, InputNormalize.none,SymplifyNode.signedModImpl);
  def abs: UInt = Mux(this.msb,~this,this).asUInt + this.msb.asUInt
  def abs(enable : Bool): UInt = Mux(this.msb && enable,~this,this).asUInt + (this.msb && enable).asUInt


  def |(that: SInt): SInt = newBinaryOperator("s|s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  def &(that: SInt): SInt = newBinaryOperator("s&s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth,SymplifyNode.binaryInductZeroWithOtherWidth(S.apply));
  def ^(that: SInt): SInt = newBinaryOperator("s^s", that, WidthInfer.inputMaxWidth,InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  def unary_~(): SInt = newUnaryOperator("~s",WidthInfer.inputMaxWidth,SymplifyNode.unaryZero);
  def unary_-(): SInt = newUnaryOperator("-s",WidthInfer.inputMaxWidth,SymplifyNode.unaryZero);


  override def <(that: SInt): Bool = newLogicalOperator("s<s", that,InputNormalize.inputWidthMax,SymplifyNode.binarySIntSmaller);
  override def >(that: SInt): Bool = that < this
  override def <=(that: SInt): Bool = newLogicalOperator("s<=s", that,InputNormalize.inputWidthMax,SymplifyNode.binarySIntSmallerOrEgual);
  override def >=(that: SInt): Bool = that <= this

  override def >>(that: Int): SInt = newBinaryOperator("s>>i", IntLiteral(that), WidthInfer.shiftRightWidth,InputNormalize.none,SymplifyNode.shiftRightImpl);
  override def <<(that: Int): SInt = newBinaryOperator("s<<i", IntLiteral(that), WidthInfer.shiftLeftWidth,InputNormalize.none,SymplifyNode.shiftLeftImpl(S.apply));
  def >>(that: UInt): SInt = newBinaryOperator("s>>u", that, WidthInfer.shiftRightWidth,InputNormalize.none,SymplifyNode.shiftRightImpl);
  def <<(that: UInt): SInt = newBinaryOperator("s<<u", that, WidthInfer.shiftLeftWidth,InputNormalize.none,SymplifyNode.shiftLeftImpl(S.apply));


  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,s,s)",sel,whenTrue,whenFalse)
  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: SInt => newLogicalOperator("s==s", that, InputNormalize.inputWidthMax,SymplifyNode.binaryThatIfBoth(True));
      case that : MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: SInt => newLogicalOperator("s!=s", that, InputNormalize.inputWidthMax,SymplifyNode.binaryThatIfBoth(False));
      case that : MaskedLiteral => that =/= this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def asBits: Bits = new Bits().castFrom("s->b", this)
  override def assignFromBits(bits: Bits) : Unit = this := bits.asSInt
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = this(hi,lo).assignFromBits(bits)

  def asUInt: UInt = new UInt().castFrom("s->u", this)


  //override def resize(width: Int): this.type = newResize("resize(s,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width,SymplifyNode.resizeImpl(S.apply))
  override def resize(width: Int): this.type = addTypeNodeFrom({
    val node = new ResizeSInt
    node.input = this
    node.size = width
    node
  })

  override def minValue: BigInt = -(BigInt(1) << (getWidth - 1))
  override def maxValue: BigInt = (BigInt(1) << (getWidth - 1)) - 1


  override def getZero: this.type = S(0).asInstanceOf[this.type]
}
