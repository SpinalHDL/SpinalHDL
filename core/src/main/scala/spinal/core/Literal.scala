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

class TagDefault(val default : Tuple2[Any,Any],val litFacto : (BigInt,Int) => BitVector) extends SpinalTag

abstract class BitVectorLiteralFactory[T <: BitVector] {
  def apply(): T
  def apply(value: BigInt): T = BitsLiteral(value, -1, this())
  def apply(value: BigInt, width: BitCount): T = BitsLiteral(value, width.value, this().setWidth(width.value))
  def apply(value: String): T = bitVectorStringParser(this,value,isSigned)
  def isSigned : Boolean

  private[core] def newInstance(bitCount : BitCount) : T
  def apply(bitCount : BitCount,rangesValue : Tuple2[Any,Any], _rangesValues: Tuple2[Any,Any]*) : T = this.aggregate(bitCount,rangesValue +: _rangesValues)

  private def aggregate(bitCount : BitCount,rangesValues: Seq[Tuple2[Any,Any]]) : T = {
    val ret : T = this.newInstance(bitCount.value bit)

    rangesValues.foreach(_ match{
      case (`default`,value : Boolean) =>  {
        ret.setAllTo(value)
        ret.addTag(new TagDefault(default -> value,(x:BigInt,y:Int) => this.apply(x,y bit)))
      }
      case (`default`,value : Bool) =>{
        for(i <- 0 until bitCount.value)
          ret(i) := value
        ret.addTag(new TagDefault(default -> value,(x:BigInt,y:Int) => this.apply(x,y bit)))
      }
      case _ =>
    })

    rangesValues.foreach(_ match{
      case (pos : Int,value : Boolean) => ret(pos) := Bool(value)
      case (pos : Int,value : Bool) => ret(pos) := value //literal extraction
      case (range : Range,value : Bool) => {
        for(i <- range)
          ret(i) := value
      }
      case (range : Range,value : Boolean) => ret(range) := apply(if(! value) BigInt(0) else (BigInt(1) << range.size)-1)
      case (range : Range,value : String) => ret(range) := apply(value)
      case (range : Range,value : T) => ret(range) := value
      case (`default`,value : Boolean) =>
      case (`default`,value : Bool) =>
    })
    ret
  }

  def apply(rangesValue : Tuple2[Any,Any],_rangesValues: Tuple2[Any,Any]*) : T = {
    val rangesValues = rangesValue +: _rangesValues
    var hig = -1
    rangesValues.foreach(_ match{
      case (pos : Int,_) => {
        hig = Math.max(hig,pos)
      }
      case (range : Range,_) => {
        hig = Math.max(hig,range.last)
      }
      case _ =>
    })

    this.aggregate(hig + 1 bit,rangesValues)
  }
}

object B extends BitVectorLiteralFactory[Bits] {
  def apply() : Bits = new Bits()
  override private[core] def newInstance(bitCount: BitCount): Bits = Bits(bitCount)

  override def isSigned: Boolean = false
}

object U extends BitVectorLiteralFactory[UInt] {
  def apply() : UInt = new UInt()
  override private[core] def newInstance(bitCount: BitCount): UInt = UInt(bitCount)
  override def isSigned: Boolean = false
}

object S extends BitVectorLiteralFactory[SInt] {
  def apply() : SInt = new SInt()
  override private[core] def newInstance(bitCount: BitCount): SInt = SInt(bitCount)
  override def isSigned: Boolean = true
}


trait Literal extends Node {
  override def clone: this.type = ???
  private[core] def getBitsStringOn(bitCount : Int) : String

  override def getInput(id: Int): Node = ???
  override def getInputs: Iterator[Node] = Iterator()
  override def getInputsCount: Int = 0
  override def onEachInput(doThat: (Node) => Unit): Unit = {}
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {}
  override def setInput(id: Int, node: Node): Unit = ???
}

object BitsLiteral {

  def apply[T <: BaseType](value: BigInt, specifiedBitCount: Int, on: T): T = {
    val valueBitCount = value.bitLength + (if (on.isInstanceOf[SInt] && value != 0) 1 else 0)
    var bitCount = specifiedBitCount
    if (!on.isInstanceOf[SInt] && value < 0) throw new Exception("literal value is negative and can be represented")
    if (bitCount != -1) {
      if (valueBitCount > bitCount) throw new Exception("literal width specification is to small")
    } else {
      bitCount = valueBitCount
    }
    on.input = new BitsLiteral(value, bitCount,specifiedBitCount != -1, on)
    on
  }
}

class BitsLiteral(val value: BigInt, val bitCount: Integer,val hasSpecifiedBitCount : Boolean, val kind: Node) extends Literal {
  def calcWidth: Int = bitCount
  if(globalData.nodeAreInferringWidth) inferredWidth = bitCount

  override def clone(): this.type = new BitsLiteral(value, bitCount,hasSpecifiedBitCount, kind).asInstanceOf[this.type]
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
  
  
  def minimalValueBitWidth : Int = {
    value.bitLength + (if(isSignedKind && value != 0) 1 else 0)
  }

  def isSignedKind = kind.isInstanceOf[SInt]
}

class BitsAllToLiteral(val theConsumer : Node,val value: Boolean) extends Literal {
  def calcWidth: Int = theConsumer.getWidth
  override def getBitsStringOn(bitCount: Int): String = (if(value) "1" else "0" ) * getWidth
}

object BoolLiteral {
  def apply(value: Boolean, on: Bool): Bool = {
    on.input = new BoolLiteral(value)
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




class STime(value : Double){
  def decompose: (Double,String) = {
    if(value > 3600.0) return (value/3600.0,"hr")
    if(value > 60.0) return (value/60.0,"min")
    if(value > 1.0) return (value/1.0,"sec")
    if(value > 1.0e-3) return (value/1.0e-3,"ms")
    if(value > 1.0e-6) return (value/1.0e-6,"us")
    if(value > 1.0e-9) return (value/1.0e-9,"ns")
    if(value > 1.0e-12) return (value/1.0e-12,"ps")
    (value/1.0e-15,"fs")
  }
}

