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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class EnumLiteral[T <: SpinalEnum](val enum: SpinalEnumElement[T]) extends Literal {
  override def calcWidth: Int = ??? //TODO

  override def clone : this.type = new EnumLiteral(enum).asInstanceOf[this.type]

  private[core] override def getBitsStringOn(bitCount: Int): String = {
    ???
//    assert(bitCount == enum.parent.getWidth)
//    val str = enum.id.toString(2)
//    return "0" * (bitCount - str.length) + str
  }
}

class SpinalEnumCraft[T <: SpinalEnum](val blueprint: T,encoding: SpinalEnumEncoding) extends BaseType {

  private[core] def assertSameType(than: SpinalEnumCraft[_]): Unit = if (blueprint != than.blueprint) SpinalError("Enum is assigned by a incompatible enum")


  def :=(that: SpinalEnumElement[T]): Unit = new DataPimper(this) := that.craft()
  def ===(that: SpinalEnumElement[T]): Bool = this === (that.craft())
  def =/=(that: SpinalEnumElement[T]): Bool = this =/= (that.craft())
  @deprecated("Use =/= instead")
  def !==(that: SpinalEnumElement[T]): Bool = this =/= that


  override def isEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  newLogicalOperator("e==e", that, InputNormalize.none,ZeroWidth.none);
      case that : SpinalEnumElement[_] if that.parent == blueprint =>  newLogicalOperator("e==e", that(), InputNormalize.none,ZeroWidth.none);
      case _ => SpinalError("Uncompatible test")
    }
  }
  override def isNotEguals(that: Any): Bool = {
    that match{
      case that : SpinalEnumCraft[_] if that.blueprint == blueprint =>  newLogicalOperator("e!=e", that, InputNormalize.none,ZeroWidth.none);
      case that :SpinalEnumElement[_] if that.parent == blueprint => newLogicalOperator("e!=e", that(), InputNormalize.none,ZeroWidth.none);
      case _ => SpinalError("Uncompatible test")
    }
  }

  private[core] override def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = Multiplex("mux(B,e,e)", sel, whenTrue, whenFalse)
  override def toBits: Bits = new Bits().castFrom("e->b", this)
  override def assignFromBits(bits: Bits): Unit = enumCastFrom("b->e", bits, (node) => this.getWidth)
  override def assignFromBits(bits: Bits,hi : Int,lo : Int): Unit = {
    assert(lo == 0,"Enumeration can't be partialy assigned")
    assert(hi == getWidth-1,"Enumeration can't be partialy assigned")
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
    ret.inputs(0) = new EnumLiteral(this)
    ret
  }

  def toBits: Bits = new Bits().castFrom("e->b", craft)
}

trait SpinalEnumEncoding{
  def getWidth(enum : SpinalEnum) : Int
  def getValue(element: SpinalEnumElement) : BigInt
}
0
object sequancial extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = enum.values.length
  def getValue(element: SpinalEnumElement) : BigInt = {
    return element.position
  }
}

object oneHot extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = enum.values.length
  def getValue(element: SpinalEnumElement) : BigInt = {
    return BigInt(1) << element.position
  }
}


class SpinalEnum(defaultEncoding : SpinalEnumEncoding = null) extends Nameable {
  def apply() = craft


  def values = ArrayBuffer[SpinalEnumElement[this.type]]()

  def Value(name: String): SpinalEnumElement[this.type] = {
    val v = new SpinalEnumElement(this,values.size).asInstanceOf[SpinalEnumElement[this.type]]
    if (name != null) v.setName(name)
    values += v
    v
  }

  def ordered() : SpinalEnumElement[this.type] = Value(null)

  def craft(): SpinalEnumCraft[this.type] = new SpinalEnumCraft[this.type](this,defaultEncoding)
}
