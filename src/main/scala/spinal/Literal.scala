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

abstract class Literal extends Node {

}

object BitsLiteral{
  def apply[T <: Node](value: BigInt,specifiedBitCount : Int,on : T): T ={
    val valueBitCount = value.bitLength + (if(on.isInstanceOf[SInt]) 1 else 0)
    var bitCount = specifiedBitCount
    if(!on.isInstanceOf[SInt] && value < 0) throw new Exception("literal value is negative and can be represented")
    if(bitCount != -1){
      if(valueBitCount > bitCount) throw new Exception("literal width specification is to little")
    }else{
      bitCount = valueBitCount
    }
    on.inputs(0) = new BitsLiteral(value,bitCount,on)
    on
  }
}

class BitsLiteral(val value: BigInt,val bitCount : Integer,val kind : Node) extends Literal{
  def calcWidth: Int = bitCount
}


object BoolLiteral{
  def apply(value: Boolean,on : Bool): Bool ={
    on.inputs(0) = new BoolLiteral(value)
    on
  }
}
class BoolLiteral(val value: Boolean) extends Literal{
  def calcWidth: Int = 1
}


object IntLiteral {
  def apply(value : BigInt): IntLiteral ={
    return new IntLiteral(value)
  }
}
class IntLiteral(val value: BigInt) extends Literal with MinMaxProvider{
  def calcWidth: Int = 0

  def minValue : BigInt = value
  def maxValue : BigInt = value
}

object SStringLiteral {
  def apply(value : String): SStringLiteral ={
    return new SStringLiteral(value)
  }
}
class SStringLiteral(val value: String) extends Literal {
  def calcWidth: Int = 0

}

