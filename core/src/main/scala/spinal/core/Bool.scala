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


trait BoolFactory {
  def Bool(): Bool = new Bool
  def Bool(value: Boolean): Bool = BoolLiteral(value, Bool)
}


class Bool extends BaseType with DataPrimitives[Bool]{
  override def getBitsWidth: Int = 1


  override private[spinal] def _data: Bool = this

  def &&(b: Bool): Bool = wrapLogicalOperator(b,new Operator.Bool.And)
  def ||(b: Bool): Bool = wrapLogicalOperator(b,new Operator.Bool.Or)
  def ^(b: Bool): Bool  = wrapLogicalOperator(b,new Operator.Bool.Xor)
  def unary_!(): Bool = wrapUnaryOperator(new Operator.Bool.Not)
  def &(b: Bool): Bool = this && b
  def |(b: Bool): Bool = this || b
  def set() = this := True
  def clear() = this := False
  def setWhen(cond : Bool) : Bool = {when(cond){this := True} ; this}
  def clearWhen(cond : Bool): Bool = {when(cond){this := False} ; this}

  def rise() = this && ! RegNext(this)
  def fall() = ! this && RegNext(this)
  def rise(initAt : Bool) = this && ! RegNext(this).init(initAt)
  def fall(initAt : Bool) = ! this && RegNext(this).init(initAt)

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerBool)

  //cond ? a | b syntax
  case class MuxBuilder[T <: Data](whenTrue : T){
    def |(whenFalse : T) : T = Mux(Bool.this,whenTrue,whenFalse)
  }
  def ?[T <: Data](whenTrue : T) = MuxBuilder(whenTrue)

  case class MuxBuilderEnum[T <: SpinalEnum](whenTrue : SpinalEnumCraft[T]){
    def |(whenFalse : SpinalEnumCraft[T]) : SpinalEnumCraft[T] = Mux(Bool.this,whenTrue,whenFalse)
    def |(whenFalse : SpinalEnumElement[T]) : SpinalEnumCraft[T] = Mux(Bool.this,whenTrue,whenFalse())
  }
  def ?[T <: SpinalEnum](whenTrue : SpinalEnumElement[T]) = MuxBuilderEnum(whenTrue())
  def ?[T <: SpinalEnum](whenTrue : SpinalEnumCraft[T])   = MuxBuilderEnum(whenTrue)

  private[core] override def isEguals(that: Any): Bool = {
    that match {
      case that: Bool => wrapLogicalOperator(that,new Operator.Bool.Equal);
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  private[core] override def isNotEguals(that: Any): Bool = {
    that match {
      case that: Bool => wrapLogicalOperator(that,new Operator.Bool.NotEqual);
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def asBits: Bits = wrapCast(Bits(),new CastBoolToBits)

  override def assignFromBits(bits: Bits): Unit = this := bits(0)

  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = {
    assert(hi == 0, "assignFromBits hi != 0")
    assert(low == 0, "assignFromBits low != 0")
    assignFromBits(bits)
  }

  def asUInt: UInt = asBits.asUInt

  def asSInt: SInt = asBits.asSInt

  def asUInt(bitCount: BitCount): UInt = asBits.asUInt.resize(bitCount.value)

  def asBits(bitCount: BitCount): Bits = asBits.resize(bitCount.value)


  //Create a new instance of the same datatype without any configuration (width, direction)
  override private[core] def weakClone: this.type = new Bool().asInstanceOf[this.type]

  override def getZero: this.type = False.asInstanceOf[this.type]
}
