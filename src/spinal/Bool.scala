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

package spinal

/**
 * Created by PIC18F on 21.08.2014.
 */

object Bool extends BoolFactory{

}

class BoolFactory {
  def apply() : spinal.Bool = new Bool()
  def apply(value : Boolean) : spinal.Bool = BoolLiteral(value,apply())
}



class Bool extends BaseType {
  override type SSelf = Bool

  override def calcWidth : Int = 1

  def ==(that: Bool): Bool = newLogicalOperator("B==B", that, InputNormalize.none);
  def !=(that: Bool): Bool = newLogicalOperator("B!=B", that, InputNormalize.none);


  def &&(b: Bool): Bool = newLogicalOperator("&&", b,InputNormalize.none)
  def ||(b: Bool): Bool = newLogicalOperator("||", b,InputNormalize.none)
  def unary_!(): Bool = newUnaryOperator("!")

  //def := (bool : Bool): Unit = assignFrom(bool)

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,B,B)",sel,whenTrue,whenFalse)

  override def isEguals(that: Data): Bool = {
    that match{
      case that : Bool => this == that
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def toBits : Bits = new Bits().castFrom("B->b",this)
  override def fromBits(bits: Bits) : Unit = this := bits(0)

  def toUInt : UInt = toBits.toUInt




}
