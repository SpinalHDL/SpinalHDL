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

import scala.collection.mutable.ArrayBuffer

class EnumLiteral[T <: SpinalEnum](val enum: SpinalEnumElement[T],val encoding: SpinalEnumEncoding) extends Literal {
  override def calcWidth: Int = encoding.getWidth(enum.parent)

  override def clone : this.type = new EnumLiteral(enum,encoding).asInstanceOf[this.type]

  private[core] override def getBitsStringOn(bitCount: Int): String = {
    val str = encoding.getValue(enum).toString(2)
    return "0" * (bitCount - str.length) + str
  }
}

class SpinalEnumCraft[T <: SpinalEnum](val blueprint: T,val encoding: SpinalEnumEncoding) extends BaseType {

  private[core] def assertSameType(than: SpinalEnumCraft[_]): Unit =
    if (blueprint != than.blueprint) SpinalError("Enum is assigned by a incompatible enum")

  def :=(that: SpinalEnumElement[T]): Unit = new DataPimper(this) := that.craft()
  def ===(that: SpinalEnumElement[T]): Bool = this === (that.craft())
  def =/=(that: SpinalEnumElement[T]): Bool = this =/= (that.craft())

  @deprecated("Use =/= instead")
  def !==(that: SpinalEnumElement[T]): Bool = this =/= that

  def assignFromAnotherEncoding(spinalEnumCraft: SpinalEnumCraft[T]) = {
    val c = this.clone
    val cast = new CastEnumToEnum(c)
    cast.input = spinalEnumCraft
    c.input = cast
    this := c
  }


  override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match{
    case that : SpinalEnumCraft[T] => {
      val assignWith = if (this.encoding == that.encoding){
        that
      }else{
        val cloned = this.clone
        cloned.assignFromAnotherEncoding(that)
        cloned
      }
      super.assignFromImpl(assignWith, conservative)
    }
  }

  override def isEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  wrapLogicalOperator(that,new Operator.Enum.Equal);
      case that : SpinalEnumElement[_] if that.parent == blueprint =>  wrapLogicalOperator(that(),new Operator.Enum.Equal);
      case _ => SpinalError("Incompatible test")
    }
  }
  override def isNotEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  wrapLogicalOperator(that,new Operator.Enum.NotEqual);
      case that :SpinalEnumElement[_] if that.parent == blueprint => wrapLogicalOperator(that(),new Operator.Enum.NotEqual);
      case _ => SpinalError("Incompatible test")
    }
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerEnum)
  override def asBits: Bits = wrapCast(Bits(),new CastEnumToBits)

  override def assignFromBits(bits: Bits): Unit = {
    val c = this.clone
    val cast = new CastBitsToEnum(c)
    cast.input = bits
    c.input = cast
    this := c
  }

  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = {
    assert(lo == 0,"Enumeration can't be partially assigned")
    assert(hi == getWidth-1,"Enumeration can't be partially assigned")
    assignFromBits(bits)
  }

  override def calcWidth: Int = encoding.getWidth(blueprint)
  override def clone: this.type = {
    val res = new SpinalEnumCraft(blueprint,encoding).asInstanceOf[this.type]
   // res.dir = this.dir
    res
  }

  def init(enumElement: SpinalEnumElement[T]): this.type = {
    this.initImpl(enumElement())
  }

  private[core] def getParentName = blueprint.getName()

  override def getZero: this.type = {
    val ret = clone
    ret.assignFromBits(B(0))
    ret
  }
  private[core] override def weakClone: this.type = new SpinalEnumCraft(blueprint,encoding).asInstanceOf[this.type]
}


class SpinalEnumElement[T <: SpinalEnum](val parent: T, val position: Int) extends Nameable {
  def ===(that: SpinalEnumCraft[T]): Bool = {
    that === this
  }
  def =/=(that: SpinalEnumCraft[T]): Bool = {
    that =/= this
  }

  def apply() : SpinalEnumCraft[T] = craft()
  def craft(): SpinalEnumCraft[T] = {
    val ret = parent.craft().asInstanceOf[SpinalEnumCraft[T]]
    ret.setInputWrap(0) = new EnumLiteral(this,this.parent.defaultEncoding)
    ret
  }

  def toBits: Bits = craft().asBits
}

trait SpinalEnumEncoding extends Nameable{
  def getWidth(enum : SpinalEnum) : Int
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt
  def isNative : Boolean
}

object native extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.values.length)
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return element.position
  }
  override def isNative = true
  setWeakName("native")
}

object sequancial extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.values.length)
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return element.position
  }

  override def isNative = false
  setWeakName("sequancial")
}



object oneHot extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = enum.values.length
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return BigInt(1) << element.position
  }
  override def isNative = true
  setWeakName("one_hot")
}

object Encoding{
  def apply[X <: SpinalEnum](name : String)(spec : (SpinalEnumElement[X],BigInt)*) : SpinalEnumEncoding = {
    val map : Map[SpinalEnumElement[X],BigInt] = spec.toMap
    list(name)(map)
  }
  def list[X <: SpinalEnum](name : String)(spec : Map[SpinalEnumElement[X],BigInt]) : SpinalEnumEncoding = {
    if(spec.size != spec.head._1.blueprint.values.size){
      SpinalError("All elements of the enumeration should be mapped")
    }
    return new SpinalEnumEncoding {
      val width = log2Up(spec.values.foldLeft(BigInt(0))((a,b) => if(a > b) a else b) + 1)
      override def getWidth(enum: SpinalEnum): Int = width

      override def isNative: Boolean = false
      def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
        return spec(element.asInstanceOf[SpinalEnumElement[X]])
      }
      setWeakName(name)
    }
  }
}

class EnumFsm(defaultEncoding : SpinalEnumEncoding = native) extends SpinalEnum
class EnumData(defaultEncoding : SpinalEnumEncoding = sequancial) extends SpinalEnum

class SpinalEnum(var defaultEncoding : SpinalEnumEncoding = native) extends Nameable {
  type T = SpinalEnumCraft[this.type]
  def apply() = craft()
  def apply(encoding: SpinalEnumEncoding) = craft(encoding)

  val values = ArrayBuffer[SpinalEnumElement[this.type]]()

  def newElement() : SpinalEnumElement[this.type] = newElement(null)
  def newElement(name: String): SpinalEnumElement[this.type] = {
    val v = new SpinalEnumElement(this,values.size).asInstanceOf[SpinalEnumElement[this.type]]
    if (name != null) v.setName(name)
    values += v
    v
  }

  def craft(enumEncoding: SpinalEnumEncoding): SpinalEnumCraft[this.type] = new SpinalEnumCraft[this.type](this,enumEncoding)
  def craft(): SpinalEnumCraft[this.type] = craft(defaultEncoding)
}
