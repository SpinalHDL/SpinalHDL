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
  def asUInt(that : Bool) : UInt = that.asUInt
  def asUInt(that : Bits) : UInt = that.asUInt
  def asUInt(that : SInt) : UInt = that.asUInt
  def toUInt(that : UFix) : UInt = that.toUInt
}

trait UIntFactory{
  def UInt() = new UInt()
//  def UInt(width : Int): UInt = UInt.setWidth(width)
  def UInt(width: BitCount): UInt = UInt().setWidth(width.value)
}

class UInt extends BitVector with Num[UInt] with MinMaxProvider {
  private[core] def prefix : String = "u"

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

  def ===(that : UInt) : Bool = this.isEguals(that)
  def =/=(that : UInt) : Bool = this.isNotEguals(that)
  def ===(that : MaskedLiteral) : Bool = this.isEguals(that)
  def =/=(that : MaskedLiteral) : Bool = this.isNotEguals(that)

  def @@(that : UInt) = (this ## that).asUInt


  def twoComplement(enable : Bool): SInt = ((False ## Mux(enable,~this,this)).asUInt + enable.asUInt).asSInt

  override def +(that: UInt): UInt = newBinaryOperator("u+u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  override def -(that: UInt): UInt = newBinaryOperator("u-u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,SymplifyNode.binaryMinus(U.apply));
  override def *(that: UInt): UInt = newBinaryOperator("u*u", that, WidthInfer.cumulateInputWidth, InputNormalize.none,SymplifyNode.binaryInductZeroWithOtherWidth(U.apply));
  override def /(that: UInt): UInt = newBinaryOperator("u/u", that, WidthInfer.input0Width, InputNormalize.none,SymplifyNode.unsignedDivImpl);
  override def %(that: UInt): UInt = newBinaryOperator("u%u", that, WidthInfer.input0Width, InputNormalize.none,SymplifyNode.unsignedModImpl);

  def |(that: UInt): UInt = newBinaryOperator("u|u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  def &(that: UInt): UInt = newBinaryOperator("u&u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,SymplifyNode.binaryInductZeroWithOtherWidth(U.apply));
  def ^(that: UInt): UInt = newBinaryOperator("u^u", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth,SymplifyNode.binaryTakeOther);
  def unary_~(): UInt = newUnaryOperator(new OperatorUIntNot);

  override def <(that: UInt): Bool = newLogicalOperator("u<u", that, InputNormalize.inputWidthMax,SymplifyNode.binaryUIntSmaller);
  override def >(that: UInt): Bool = that < this
  override def <=(that: UInt): Bool = newLogicalOperator("u<=u", that, InputNormalize.inputWidthMax,SymplifyNode.binaryUIntSmallerOrEgual);
  override def >=(that: UInt): Bool = that <= this


  override def >>(that: Int): UInt = newBinaryOperator("u>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none,SymplifyNode.shiftRightImpl);
  override def <<(that: Int): UInt = newBinaryOperator("u<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none,SymplifyNode.shiftLeftImpl(U.apply));
  def >>(that: UInt): UInt = newBinaryOperator("u>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none,SymplifyNode.shiftRightImpl);
  def <<(that: UInt): UInt = newBinaryOperator("u<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none,SymplifyNode.shiftLeftImpl(U.apply));

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,u,u)", sel, whenTrue, whenFalse)
  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: UInt => newLogicalOperator("u==u", that, InputNormalize.inputWidthMax,SymplifyNode.binaryThatIfBoth(True));
      case that : MaskedLiteral => that === this
      case that : Int => this === that
      case that : BigInt => this === that
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: UInt => newLogicalOperator("u!=u", that, InputNormalize.inputWidthMax,SymplifyNode.binaryThatIfBoth(False));
      case that : MaskedLiteral => that === this
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1


  //override def resize(width: Int): this.type = newResize("resize(u,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width,SymplifyNode.resizeImpl(U.apply))
  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node = new ResizeUInt
    node.input = this
    node.size = width
    node
  })


  def asSInt: SInt = newCast(SInt(),new CastUIntToSInt)
  override def asBits: Bits = newCast(Bits(),new CastUIntToBits)
  override def assignFromBits(bits: Bits): Unit = this := bits.asUInt
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = this(hi,lo).assignFromBits(bits)

  def apply(bitId: Int) : Bool = newExtract(bitId,new ExtractBoolFixedFromUInt)
  def apply(bitId: UInt): Bool = newExtract(bitId,new ExtractBoolFloatingFromUInt)
  def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1,offset,new ExtractBitsVectorFixedFromUInt)
  def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset,bitCount.value,new ExtractBitsVectorFloatingFromUInt)

  override private[core] def weakClone: this.type = new UInt().asInstanceOf[this.type]
  override def getZero: this.type = U(0).asInstanceOf[this.type]
}

object UInt2D{
  def apply(commonBitCount: BitCount) : UInt2D = UInt2D(commonBitCount,commonBitCount)
}

case class UInt2D(xBitCount: BitCount,yBitCount: BitCount) extends Bundle {
  val x = UInt(xBitCount)
  val y = UInt(yBitCount)
}

