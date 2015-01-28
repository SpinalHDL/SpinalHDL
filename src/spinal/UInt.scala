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

object UInt extends UIntFactory{

}

class UIntFactory extends BitVectorFactory[UInt]{
  def apply() = new UInt()
}

class UInt extends BitVector with MinMaxProvider {
  def +(that: UInt): UInt = newBinaryOperator("u+u", that, WidthInfer.inputMaxWidthl, InputNormalize.nodeWidth);
  def -(that: UInt): UInt = newBinaryOperator("u-u", that, WidthInfer.inputMaxWidthl, InputNormalize.nodeWidth);
  def *(that: UInt): UInt = newBinaryOperator("u*u", that, WidthInfer.cumulateInputWidth, InputNormalize.none);

  def |(that: UInt): UInt = newBinaryOperator("u|u", that, WidthInfer.inputMaxWidthl, InputNormalize.nodeWidth);
  def &(that: UInt): UInt = newBinaryOperator("u&u", that, WidthInfer.inputMaxWidthl, InputNormalize.nodeWidth);
  def ^(that: UInt): UInt = newBinaryOperator("u^u", that, WidthInfer.inputMaxWidthl, InputNormalize.nodeWidth);
  def ~(that: UInt): UInt = newBinaryOperator("~u", that, WidthInfer.inputMaxWidthl, InputNormalize.none);

  def ==(that: UInt): Bool = newLogicalOperator("u==u", that, InputNormalize.inputWidthMax);
  def !=(that: UInt): Bool = newLogicalOperator("u!=u", that, InputNormalize.inputWidthMax);
  def <(that: UInt): Bool = newLogicalOperator("u<u", that, InputNormalize.inputWidthMax);
  def >(that: UInt): Bool = that < this
  def <=(that: UInt): Bool = newLogicalOperator("u<=u", that, InputNormalize.inputWidthMax);
  def >=(that: UInt): Bool = that <= this


  def >>(that: Int): this.type = newBinaryOperator("u>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none);
  def <<(that: Int): this.type = newBinaryOperator("u<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none);
  def >>(that: UInt): this.type = newBinaryOperator("u>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none);
  def <<(that: UInt): this.type = newBinaryOperator("u<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none);

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,u,u)", sel, whenTrue, whenFalse)
  override def isEguals(that: Data): Bool = {
    that match{
      case that : UInt => this == that
      case _ => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1

  def :=(uint: UInt): Unit = assignFrom(uint)

  override def resize(width: Int): this.type = newResize("resize(u,i)", this :: new IntLiteral(width) :: Nil, WidthInfer.intLit1Width)


 // implicit def dd(value: Int) = UInt(value)

  override def toBits: Bits = new Bits().castFrom("u->b", this)

}

/*
   def ===(that: Object) : Boolean = super.==(that)
def ==(that: Object): Bool = that match {
   case that : UInt => newLogicalOperator("u==u", that);
 }*/