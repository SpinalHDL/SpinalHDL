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

abstract class BitVectorLiteralFactory[T <: BitVector] {
  def apply(): T
  def apply(value: BigInt): T = BitsLiteral(value, -1, this())
  def apply(value: BigInt, width: BitCount): T = BitsLiteral(value, width.value, this().setWidth(width.value))
}

object B extends BitVectorLiteralFactory[Bits] {
  def apply() = new Bits()
}

object U extends BitVectorLiteralFactory[UInt] {
  def apply() = new UInt()
}

object S extends BitVectorLiteralFactory[SInt] {
  def apply() = new SInt()
}


trait Literal extends Node {
  override def clone: this.type = ???
  def getBitsStringOn(bitCount : Int) : String


}

object BitsLiteral {

  def apply[T <: Node](value: BigInt, specifiedBitCount: Int, on: T): T = {
    val valueBitCount = value.bitLength + (if (on.isInstanceOf[SInt] && value != 0) 1 else 0)
    var bitCount = specifiedBitCount
    if (!on.isInstanceOf[SInt] && value < 0) throw new Exception("literal value is negative and can be represented")
    if (bitCount != -1) {
      if (valueBitCount > bitCount) throw new Exception("literal width specification is to little")
    } else {
      bitCount = valueBitCount
    }
    on.inputs(0) = new BitsLiteral(value, bitCount, on)
    on
  }
}

class BitsLiteral(val value: BigInt, val bitCount: Integer, val kind: Node) extends Literal {
  def calcWidth: Int = bitCount
  if(globalData.nodeAreInferringWidth) inferredWidth = bitCount

  override def clone(): this.type = new BitsLiteral(value, bitCount, kind).asInstanceOf[this.type]
  override def getBitsStringOn(bitCount: Int): String = {
    def makeIt(fillWidth : Boolean) : String = {
      val str = (if(value > 0) value else ((BigInt(1) << bitCount) + value)).toString(2)
      val filling = if(fillWidth) "1" else "0"
      return (filling * (bitCount - str.length) + str).takeRight(bitCount)
    }

    kind match{
      case b : Bits => makeIt(false)
      case u : UInt => makeIt(false)
      case s : SInt => makeIt(value < 0)
    }
  }
}


object BoolLiteral {
  def apply(value: Boolean, on: Bool): Bool = {
    on.inputs(0) = new BoolLiteral(value)
    on
  }
}

class BoolLiteral(val value: Boolean) extends Literal {
  def calcWidth: Int = 1
  override def clone(): this.type = new BoolLiteral(value).asInstanceOf[this.type]
  override def getBitsStringOn(bitCount: Int): String = {
    assert(bitCount == 1)
    (if(value) "1" else "0")
  }
}


object IntLiteral {
  def apply(value: BigInt): IntLiteral = {
    return new IntLiteral(value)
  }
}

class IntLiteral(val value: BigInt) extends Literal with MinMaxProvider {
  def calcWidth: Int = value.bitLength + (if (value < 0) 1 else 0)

  def minValue: BigInt = value
  def maxValue: BigInt = value



  override def clone(): this.type = new IntLiteral(value).asInstanceOf[this.type]

  override def getBitsStringOn(bitCount: Int): String = ???
}
