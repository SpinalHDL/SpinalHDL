///*
// * SpinalHDL
// * Copyright (c) Dolu, All rights reserved.
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 3.0 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library.
// */
//
package spinal.core

import scala.StringBuilder
import scala.collection.mutable

//
///**
// * Created by PIC18F on 21.08.2014.
// */
//
//class TagDefault(val default : Tuple2[Any,Any],val litFacto : (BigInt,Int) => BitVector) extends SpinalTag{override def driverShouldNotChange = true}
//
//
//
//
abstract class BitVectorLiteralFactory[T <: BitVector] {
  def apply(): T
  def apply(value: Int): T = this(BigInt(value))
  def apply(value: Int, width: BitCount): T = this(BigInt(value),width)
  def apply(value: BigInt): T = getFactory(value, -1, this())
  def apply(value: BigInt, width: BitCount): T = getFactory(value, width.value, this().setWidth(width.value))
  def apply(value: String): T = bitVectorStringParser(this,value,isSigned)
  def getFactory : (BigInt,Int,T) => T
  def isSigned : Boolean

  private[core] def newInstance(bitCount : BitCount) : T
  def apply(bitCount : BitCount,rangesValue : Tuple2[Any,Any], _rangesValues: Tuple2[Any,Any]*) : T = this.aggregate(bitCount,rangesValue +: _rangesValues)

  private def aggregate(bitCount : BitCount,rangesValues: Seq[Tuple2[Any,Any]]) : T = {
    val ret : T = this.newInstance(bitCount.value bit)
    applyTuples(ret,rangesValues)
    ret
  }

  def applyTuples(on : T, rangesValues : Seq[Tuple2[Any,Any]]) : Unit = {
    //Apply default
    rangesValues.foreach(_ match{
      case (`default`,value : Boolean) =>  on.setAllTo(value)
      case (`default`,value : Bool)    => on.setAllTo(value)
      case _ =>
    })

    //Apply specific access in order
    for(e <- rangesValues) e match{
      case (pos : Int,value : Boolean) => on(pos) := Bool(value)
      case (pos : Int,value : Bool) => on(pos) := value //literal extraction
      case (range : Range,value : Bool) => {
        for(i <- range)
          on(i) := value
      }
      case (range : Range,value : Boolean) => on(range) := apply(
        if(! value)
          BigInt(0)
        else {
          if(!on.isInstanceOf[SInt])
            (BigInt(1) << range.size) - 1
          else
            BigInt(-1)

        }
      )
      case (range : Range,value : String) => on(range) := apply(value)
      case (range : Range,value : BitVector) if value.getClass == on.getClass => on(range) := value.asInstanceOf[T]
      case (`default`,value : Boolean) =>
      case (`default`,value : Bool) =>
    }
  }

  def apply(rangesValue : Tuple2[Any,Any],_rangesValues: Tuple2[Any,Any]*) : T = {
    val rangesValues = rangesValue +: _rangesValues
    var hig = -1
    rangesValues.foreach(_ match{
      case (pos : Int,_) => {
        hig = Math.max(hig,pos)
      }
      case (range : Range,_) => {
        hig = Math.max(hig,range.high)
      }
      case (`default`,_) => {
        val where = ScalaLocated.long
        GlobalData.get.pendingErrors += (() => s"You can't use default -> ??? in this case. \nBut you can use it in the following case : myBits := (???,default -> ???, ???)\n${where}")
      }
      case _ =>
    })

    this.aggregate(hig + 1 bit,rangesValues)
  }
}

object B extends BitVectorLiteralFactory[Bits] {
  def apply() : Bits = new Bits()
  def apply(that: Data) : Bits = that.asBits
  override private[core] def newInstance(bitCount: BitCount): Bits = Bits(bitCount)
  override def isSigned: Boolean = false

  override def getFactory: (BigInt, Int, Bits) => Bits = {
    BitsLiteral.apply[Bits]
  }
}

object U extends BitVectorLiteralFactory[UInt] {
  def apply() : UInt = new UInt()
  def apply(that: Bool): UInt = that.asUInt
  def apply(that: Bits): UInt = that.asUInt
  def apply(that: SInt): UInt = that.asUInt
  def apply(that: UFix) : UInt = that.toUInt
  override private[core] def newInstance(bitCount: BitCount): UInt = UInt(bitCount)
  override def isSigned: Boolean = false
  override def getFactory: (BigInt, Int, UInt) => UInt = UIntLiteral.apply[UInt]
}

object S extends BitVectorLiteralFactory[SInt] {
  def apply() : SInt = new SInt()
  def apply(that : Bool) : SInt = that.asSInt
  def apply(that : Bits) : SInt = that.asSInt
  def apply(that : UInt) : SInt = that.asSInt
  def apply(that : SFix) : SInt = that.toSInt
  override private[core] def newInstance(bitCount: BitCount): SInt = SInt(bitCount)
  override def isSigned: Boolean = true
  override def getFactory: (BigInt, Int, SInt) => SInt = SIntLiteral.apply[SInt]
}


trait Literal extends Expression {
  override def clone: this.type = ???
  final override def foreachExpression(func: (Expression) => Unit): Unit = {}
  final override def remapExpressions(func: (Expression) => Expression): Unit = {}
  private[core] def getBitsStringOn(bitCount : Int, poisonSymbol : Char) : String
  private[core] def getBitsStringOnNoPoison(bitCount : Int) : String = {
    require(!hasPoison)
    getBitsStringOn(bitCount,'?')
  }
  def getValue() : BigInt
  def hasPoison() : Boolean

//  override def addAttribute(attribute: Attribute): Literal.this.type = addTag(attribute)
}

object BitsLiteral {
  def apply(value: BigInt, poisonMask : BigInt, specifiedBitCount: Int): BitsLiteral = {
    val valueBitCount = value.bitLength
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth = Math.max(poisonBitCount,valueBitCount)
    var bitCount = specifiedBitCount
    if (value < 0) throw new Exception("literal value is negative and can be represented")
    if (bitCount != -1) {
      if (minimalWidth > bitCount) throw new Exception("literal width specification is to small")
    } else {
      bitCount = minimalWidth
    }
    BitsLiteral(value, poisonMask, bitCount,specifiedBitCount != -1)
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int,on : T) : T ={
    on.assignFrom(apply(value, null, specifiedBitCount))
    on
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int) : BitsLiteral  = apply(value, null, specifiedBitCount)


  def apply(value: BigInt, poisonMask : BigInt, bitCount: Int ,hasSpecifiedBitCount : Boolean) = {
    val ret = new BitsLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}

object UIntLiteral {
  def apply(value: BigInt, poisonMask : BigInt, specifiedBitCount: Int): UIntLiteral = {
    val valueBitCount = value.bitLength
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth = Math.max(poisonBitCount,valueBitCount)
    var bitCount = specifiedBitCount
    if (value < 0) throw new Exception("literal value is negative and can be represented")
    if (bitCount != -1) {
      if (minimalWidth > bitCount) throw new Exception("literal width specification is to small")
    } else {
      bitCount = minimalWidth
    }
    UIntLiteral(value, poisonMask, bitCount,specifiedBitCount != -1)
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int,on : T):T={
    on.assignFrom(apply(value, null, specifiedBitCount))
    on
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int) : UIntLiteral  = apply(value, null, specifiedBitCount)


  def apply(value: BigInt, poisonMask : BigInt, bitCount: Int ,hasSpecifiedBitCount : Boolean) = {
    val ret = new UIntLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}

object SIntLiteral{
  def apply(value: BigInt, poisonMask : BigInt, specifiedBitCount: Int): SIntLiteral = {
    val valueBitCount = value.bitLength + (if (value != 0) 1 else 0)
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth = Math.max(poisonBitCount,valueBitCount)
    var bitCount = specifiedBitCount
    if (bitCount != -1) {
      if (minimalWidth > bitCount ) throw new Exception("literal width specification is to small")
    } else {
      bitCount = minimalWidth
    }
    SIntLiteral(value, poisonMask, bitCount,specifiedBitCount != -1)
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int,on : T):T={
    on.assignFrom(apply(value,null,specifiedBitCount))
    on
  }
  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int) : SIntLiteral = apply(value, null, specifiedBitCount)


  def apply(value: BigInt, poisonMask : BigInt, bitCount: Int ,hasSpecifiedBitCount : Boolean) = {
    val ret = new SIntLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}


abstract class BitVectorLiteral() extends Literal with WidthProvider {
  var value: BigInt = null
  var poisonMask : BigInt = null
  var bitCount: Int = -1
  var hasSpecifiedBitCount : Boolean = true

  override def getWidth: Int = bitCount
  override def getValue(): BigInt = if(hasPoison) throw new Exception("Poisoned value") else value


  override def hasPoison() = poisonMask != null && poisonMask != 0

  //TODO BigInt.toString(2) is probably very slow
  override def getBitsStringOn(bitCount: Int, poisonSymbol : Char): String = {
    def makeIt(fillWith : Boolean) : String = {
      val str = new StringBuilder()
      val unsignedValue = if(value >= 0) value else ((BigInt(1) << bitCount) + value)
      val unsignedLength = unsignedValue.bitLength
      val poisonLength = if(poisonMask != null) poisonMask.bitLength else 0

      assert(bitCount >= unsignedLength)
      assert(bitCount >= poisonLength)

      val filling = if(fillWith) '1' else '0'
      for(i <- 0 until bitCount-unsignedLength) str += filling

      for(i <-  unsignedLength - 1 to 0 by - 1){
        str += (if(unsignedValue.testBit(i)) '1' else '0')
      }

      for(i <- poisonLength-1 to 0 by -1 if(poisonMask.testBit(i))){
        str(bitCount-i-1) = poisonSymbol
      }

      str.toString()
    }

    makeIt(isSignedKind && value < 0)
  }


  def minimalValueBitWidth : Int = {
    val pureWidth = value.bitLength + (if(isSignedKind && value != 0) 1 else 0)
    if(hasPoison) Math.max(poisonMask.bitLength,pureWidth) else pureWidth
  }

  def isSignedKind : Boolean
}

class BitsLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeBits
  override def isSignedKind: Boolean = false
  override def clone(): this.type = BitsLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "B\"xxx\""
}

class UIntLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeUInt
  override def isSignedKind: Boolean = false
  override def clone(): this.type = UIntLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "U\"xxx\""
}

class SIntLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeSInt
  override def isSignedKind: Boolean = true
  override def clone(): this.type = SIntLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "S\"xxx\""
}

//class BitsAllToLiteral(val theConsumer : Node,val value: Boolean) extends Literal with Widthable {
//  override def calcWidth: Int = theConsumer.asInstanceOf[WidthProvider].getWidth
//  override def getBitsStringOn(bitCount: Int): String = (if(value) "1" else "0" ) * bitCount
//  override def getValue(): BigInt = if(value) (BigInt(1) << getWidth) - 1 else 0
//}
//
//
//
object BoolLiteral {
  def apply(value: Boolean, on: Bool): Bool = {
    on.assignFrom(new BoolLiteral(value))
    on
  }
}

class BoolLiteral(val value: Boolean) extends Literal {
  override def getTypeObject = TypeBool
  override def opName: String = "Bool(x)"


  override def normalizeInputs: Unit = {}

  override def clone(): this.type = new BoolLiteral(value).asInstanceOf[this.type]

  override def getValue(): BigInt = if(value) 1 else 0
  override def getBitsStringOn(bitCount: Int, poisonSymbol : Char): String = {
    assert(bitCount == 1)
    (if(value) "1" else "0")
  }
  override def hasPoison() = false
}

class BoolPoison() extends Literal {
  override def getValue(): BigInt = throw new Exception("Poison have no values")
  override def getTypeObject = TypeBool
  override def opName: String = "Bool(?)"
  override def hasPoison() = true
  override def normalizeInputs: Unit = {}

  override def clone(): this.type = new BoolPoison().asInstanceOf[this.type]

  override def getBitsStringOn(bitCount: Int, poisonSymbol : Char): String = {
    assert(bitCount == 1)
    poisonSymbol.toString()
  }
}

//
//
//
//
////class STime(value : Double){
////  def decompose: (Double,String) = {
////    if(value > 3600.0) return (value/3600.0,"hr")
////    if(value > 60.0) return (value/60.0,"min")
////    if(value > 1.0) return (value/1.0,"sec")
////    if(value > 1.0e-3) return (value/1.0e-3,"ms")
////    if(value > 1.0e-6) return (value/1.0e-6,"us")
////    if(value > 1.0e-9) return (value/1.0e-9,"ns")
////    if(value > 1.0e-12) return (value/1.0e-12,"ps")
////    (value/1.0e-15,"fs")
////  }
////}
//
