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

class EnumLiteral[T <: SpinalEnum](val enum: SpinalEnumElement[T]) extends Literal with  InferableEnumEncodingImpl{
  override def clone : this.type = {
    val ret = new EnumLiteral(enum).asInstanceOf[this.type]
    ret.copyEncodingConfig(this)
    ret
  }

  private[core] override def getBitsStringOn(bitCount: Int): String = {
    val str = encoding.getValue(enum).toString(2)
    return "0" * (bitCount - str.length) + str
  }

  override def getDefinition: SpinalEnum = enum.parent

  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enum.parent.defaultEncoding
}

class SpinalEnumCraft[T <: SpinalEnum](val blueprint: T/*, encoding: SpinalEnumEncoding*/) extends BaseType with InferableEnumEncodingImpl with DataPrimitives[SpinalEnumCraft[T]]{
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = blueprint.defaultEncoding
  override def getDefinition: SpinalEnum = blueprint


  override private[spinal] def _data: SpinalEnumCraft[T] = this

  private[core] def assertSameType(than: SpinalEnumCraft[_]): Unit =
    if (blueprint != than.blueprint) SpinalError("Enum is assigned by a incompatible enum")

  def :=(that: SpinalEnumElement[T]): Unit = new DataPimper(this) := that.craft()
  def ===(that: SpinalEnumElement[T]): Bool = this === (that.craft())
  def =/=(that: SpinalEnumElement[T]): Bool = this =/= (that.craft())

  @deprecated("Use =/= instead")
  def !==(that: SpinalEnumElement[T]): Bool = this =/= that


  override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match{
    case that : SpinalEnumCraft[T] => {
      super.assignFromImpl(that, conservative)
    }
  }

  override def isEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  wrapLogicalOperator(that,new Operator.Enum.Equal(blueprint));
      case that : SpinalEnumElement[_] if that.parent == blueprint =>  wrapLogicalOperator(that(),new Operator.Enum.Equal(blueprint));
      case _ => SpinalError("Incompatible test")
    }
  }
  override def isNotEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  wrapLogicalOperator(that,new Operator.Enum.NotEqual(blueprint));
      case that :SpinalEnumElement[_] if that.parent == blueprint => wrapLogicalOperator(that(),new Operator.Enum.NotEqual(blueprint));
      case _ => SpinalError("Incompatible test")
    }
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse,new MultiplexerEnum(blueprint))
  override def asBits: Bits = wrapCast(Bits(),new CastEnumToBits)

  override def assignFromBits(bits: Bits): Unit = {
    val c = cloneOf(this)
    val cast = new CastBitsToEnum(this.blueprint)
    cast.input = bits.asInstanceOf[cast.T]
    c.input = cast
    this := c
  }

  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = {
    assert(lo == 0,"Enumeration can't be partially assigned")
    assert(hi == getBitsWidth-1,"Enumeration can't be partially assigned")
    assignFromBits(bits)
  }

  override def getBitsWidth: Int = encoding.getWidth(blueprint)

  override def clone: this.type = {
    val res = new SpinalEnumCraft(blueprint).asInstanceOf[this.type]
    res.copyEncodingConfig(this)
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
  private[core] override def weakClone: this.type = {
    val ret = new SpinalEnumCraft(blueprint).asInstanceOf[this.type]
    ret
  }

  override private[core] def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }
}


class SpinalEnumElement[T <: SpinalEnum](val parent: T, val position: Int) extends Nameable {
  def ===(that: SpinalEnumCraft[T]): Bool = {
    that === this
  }
  def =/=(that: SpinalEnumCraft[T]): Bool = {
    that =/= this
  }

  def apply() : SpinalEnumCraft[T] = craft()
  def apply(encoding : SpinalEnumEncoding) : SpinalEnumCraft[T] = craft(encoding)

  def craft(): SpinalEnumCraft[T] = {
    val ret = parent.craft(inferred).asInstanceOf[SpinalEnumCraft[T]]
    ret.input = new EnumLiteral(this)
    ret
  }

  def craft(encoding : SpinalEnumEncoding): SpinalEnumCraft[T] = {
    val ret = parent.craft(encoding).asInstanceOf[SpinalEnumCraft[T]]
    val lit = new EnumLiteral(this)
    lit.fixEncoding(encoding)
    ret.input = lit
    ret
  }
  def asBits: Bits = craft().asBits
}

trait SpinalEnumEncoding extends Nameable{
  def getWidth(enum : SpinalEnum) : Int
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt
  def isNative : Boolean
}

object inferred extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = ???
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = ???
  override def isNative: Boolean = ???
}

object native extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.values.length)
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return element.position
  }
  override def isNative = true
  setWeakName("native")
}

object binarySequancial extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.values.length)
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return element.position
  }

  override def isNative = false
  setWeakName("binary_sequancial")
}



object binaryOneHot extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = enum.values.length
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]) : BigInt = {
    return BigInt(1) << element.position
  }
  override def isNative = false
  setWeakName("binary_one_hot")
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
class EnumData(defaultEncoding : SpinalEnumEncoding = binarySequancial) extends SpinalEnum

class SpinalEnum(var defaultEncoding : SpinalEnumEncoding = native) extends Nameable {
  assert(defaultEncoding != inferred,"Enum definition should not have 'inferred' as default encoding")
  type C = SpinalEnumCraft[this.type]
  type E = SpinalEnumElement[this.type]
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

  def craft(enumEncoding: SpinalEnumEncoding): SpinalEnumCraft[this.type] = {
    val ret = new SpinalEnumCraft[this.type](this)
    if(enumEncoding != `inferred`) ret.fixEncoding(enumEncoding)
    ret
  }
  def craft(): SpinalEnumCraft[this.type] = craft(defaultEncoding)
}
