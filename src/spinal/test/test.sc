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



BigInt(1).bitLength
BigInt(2).bitLength
BigInt(3).bitLength
BigInt(4).bitLength
BigInt(-3).bitLength
BigInt(-4).bitLength
BigInt(-5).bitLength
val a = List(1, 2, 3)
val b = List(4)
(a, b).zipped.size
object UInt {
  def apply(init: Int) = {
    val uint = new UInt
    uint.init = init
    uint
  }

}

trait IODirection;
object IN extends IODirection;
object OUT extends IODirection {
  def apply(u: UInt) = {

  }

  def -(u: UInt) = {
    u.asOutput()
  }

  object UInt {
    def apply(init: Int) = {
      val uint = new UInt
      uint.init = init
      uint
    }
    def apply(width: bitRange) = {
      val uint = new UInt
      uint.width = width.width
      uint
    }
    def apply(init: Int,width: bitRange) = {
      val uint = new UInt
      uint.width = width.width
      uint.init = init
      uint
    }
  }

}

class bitRange(val width: Int) {}

class UInt {
  var width = -1
  var init = -1


  def :=(u: BigInt) = {
    init = u.toInt
    this
  }
  def asOutput() = println("asOutput")

  override def toString = s"width:$width init:$init"
}


case class IntBuilder(i: Int) {
  /*object bits {
    def apply(number: Int) = new bitRange(number)
  }*/

  def bit = new bitRange(i)

}

implicit def dd(value: Int) = new IntBuilder(value)
//implicit def dd(value: OUT) = UInt(2)

val width = 2 bit;
UInt(2) asOutput


val myOutput = OUT UInt (5,3 bit)

OUT (new UInt)