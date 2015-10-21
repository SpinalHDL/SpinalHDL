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

trait BoolCast{
  def toBool(that : Bits) : Bool = that(0)
  def toBool(that : UInt) : Bool = that(0)
  def toBool(that : SInt) : Bool = that(0)
}

trait BoolFactory {
  def Bool : Bool = new Bool
  def Bool(value : Boolean) : Bool = BoolLiteral(value,Bool)
}



class Bool extends BaseType {
  override def calcWidth : Int = 1



  def ^(b: Bool): Bool = newLogicalOperator("B^B", b,InputNormalize.none,ZeroWidth.none)
  def &&(b: Bool): Bool = newLogicalOperator("&&", b,InputNormalize.none,ZeroWidth.none)
  def ||(b: Bool): Bool = newLogicalOperator("||", b,InputNormalize.none,ZeroWidth.none)
  def unary_!(): Bool = newUnaryOperator("!",WidthInfer.inputMaxWidth,ZeroWidth.none)

  def &(b: Bool): Bool = this && b
  def |(b: Bool): Bool = this || b


  def set() = this := True
  def clear() = this := False

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,B,B)",sel,whenTrue,whenFalse)

  override def isEguals(that: Data): Bool = {
    that match{
      case that : Bool => newLogicalOperator("B==B", that, InputNormalize.none,ZeroWidth.none);
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }
  override def isNotEguals(that: Data): Bool = {
    that match{
      case that : Bool => newLogicalOperator("B!=B", that, InputNormalize.none,ZeroWidth.none);
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def toBits : Bits = new Bits().castFrom("B->b",this)
  override def assignFromBits(bits: Bits) : Unit = this := bits(0)

  def toUInt : UInt = toBits.toUInt
  def toSInt : SInt = toBits.toSInt


  override def getZero: this.type = False.asInstanceOf[this.type]
}
