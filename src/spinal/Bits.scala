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
 * Created by PIC18F on 16.01.2015.
 */

object Bits extends BitsFactory{

}

class BitsFactory extends BitVectorFactory[Bits]{
  def apply() = new Bits()
}

class Bits extends BitVector{

  def ## (right: Bits): Bits = newBinaryOperator("##",right,WidthInfer.cumulateInputWidth,InputNormalize.none)


  def |(that: Bits): Bits = newBinaryOperator("b|b", that, WidthInfer.inputMaxWidthl,InputNormalize.nodeWidth);
  def &(that: Bits): Bits = newBinaryOperator("b&b", that, WidthInfer.inputMaxWidthl,InputNormalize.nodeWidth);
  def ^(that: Bits): Bits = newBinaryOperator("b^b", that, WidthInfer.inputMaxWidthl,InputNormalize.nodeWidth);
  def ~(that: Bits): Bits = newBinaryOperator("~b", that, WidthInfer.inputMaxWidthl,InputNormalize.none);

  def ==(that: Bits): Bool = newLogicalOperator("b==b", that,InputNormalize.inputWidthMax);
  def !=(that: Bits): Bool = newLogicalOperator("b!=b", that,InputNormalize.inputWidthMax);

  def >>(that: Int): this.type = newBinaryOperator("b>>i", IntLiteral(that), WidthInfer.shiftRightWidth,InputNormalize.none);
  def <<(that: Int): this.type = newBinaryOperator("b<<i", IntLiteral(that), WidthInfer.shiftLeftWidth,InputNormalize.none);
  def >>(that: UInt): this.type = newBinaryOperator("b>>u", that, WidthInfer.shiftRightWidth,InputNormalize.none);
  def <<(that: UInt): this.type = newBinaryOperator("b<<u", that, WidthInfer.shiftLeftWidth,InputNormalize.none);

  def :=(bits: Bits): Unit = assignFrom(bits)

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,b,b)",sel,whenTrue,whenFalse)

  override def resize(width: Int): this.type = newResize("resize(b,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width)

  def toSInt: SInt = new SInt().castFrom("b->s", this)
  def toUInt: UInt = new UInt().castFrom("b->u", this)
  override def toBits : Bits = {
    val ret = new Bits()
    ret.assignFrom(this)
    ret
  }



 /* override def castThatInSame(that : BaseType): this.type ={
    that.toBits.asInstanceOf[ this.type]
  }*/
}
