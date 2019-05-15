/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import spinal.core.internals._
import scala.collection.mutable.ArrayBuffer


/**
  * Definition of an element of the enumeration
  *
  * @param spinalEnum parent of the element (SpinalEnum)
  * @param position position of the element
  */
class SpinalEnumElement[T <: SpinalEnum](val spinalEnum: T, val position: Int) extends Nameable {

  def ===(that: SpinalEnumCraft[T]): Bool = that === this
  def =/=(that: SpinalEnumCraft[T]): Bool = that =/= this

  def apply(): SpinalEnumCraft[T] = craft()
  def apply(encoding: SpinalEnumEncoding): SpinalEnumCraft[T] = craft(encoding)

  def craft(): SpinalEnumCraft[T] = {
    val ret = spinalEnum.craft(inferred).asInstanceOf[SpinalEnumCraft[T]].setAsTypeNode()
    ret.assignFrom(new EnumLiteral(this))
    ret
  }

  def craft(encoding: SpinalEnumEncoding): SpinalEnumCraft[T] = {
    val ret = spinalEnum.craft(encoding).asInstanceOf[SpinalEnumCraft[T]]
    val lit = new EnumLiteral(this)

    lit.fixEncoding(encoding)
    ret.assignFrom(lit)
    ret
  }

  def asBits: Bits = craft().asBits
}


/**
 * Base class for creating enumeration
 *
 * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Enum Enumeration Documentation]]
 *
 * @example {{{
 *         object MyEnum extends SpinalEnum(binarySequential){
 *           val s1, s2, s3, s4 = newElement()
 *         }
 *         }}}
 *
 * SpinalEnum contains a list of SpinalEnumElement that is the definition of an element. SpinalEnumCraft is the
 * hardware representation of the the element.
 *
 * @param defaultEncoding encoding of the enum
 */
class SpinalEnum(var defaultEncoding: SpinalEnumEncoding = native) extends Nameable with ScalaLocated {

  assert(defaultEncoding != inferred, "Enum definition should not have 'inferred' as default encoding")

  type C = SpinalEnumCraft[this.type]
  type E = SpinalEnumElement[this.type]

  /** Contains all elements of the enumeration */
  @dontName val elements = ArrayBuffer[SpinalEnumElement[this.type]]()

  def apply() = craft()
  def apply(encoding: SpinalEnumEncoding) = craft(encoding)

  def craft(): SpinalEnumCraft[this.type] = craft(defaultEncoding)
  def craft(enumEncoding: SpinalEnumEncoding): SpinalEnumCraft[this.type] = {
    val ret = new SpinalEnumCraft[this.type](this)
    if(enumEncoding != `inferred`) ret.fixEncoding(enumEncoding)
    ret
  }

  /** Create a new Element */
  def newElement(): SpinalEnumElement[this.type] = newElement(null)

  def newElement(name: String): SpinalEnumElement[this.type] = {
    val v = new SpinalEnumElement(this,elements.size).asInstanceOf[SpinalEnumElement[this.type]]
    if (name != null) v.setName(name)
    elements += v
    v
  }
}


/**
  * Hardware representation of an enumeration
  */
class SpinalEnumCraft[T <: SpinalEnum](val spinalEnum: T) extends BaseType with InferableEnumEncodingImpl with DataPrimitives[SpinalEnumCraft[T]] {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "EnumCraft"

  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = spinalEnum.defaultEncoding

  override private[core] def canSymplifyIt = super.canSymplifyIt && (this.encodingChoice == InferableEnumEncodingImplChoiceUndone)

  override def getDefinition: SpinalEnum = spinalEnum

  private[spinal] override def _data: SpinalEnumCraft[T] = this

  private[core] def assertSameType(than: SpinalEnumCraft[_]): Unit = if (spinalEnum != than.spinalEnum) SpinalError("Enum is assigned by a incompatible enum")

  def :=(that: SpinalEnumElement[T]): Unit = new DataPimper(this) := that.craft()
  def ===(that: SpinalEnumElement[T]): Bool = this === that.craft()
  def =/=(that: SpinalEnumElement[T]): Bool = this =/= that.craft()

  @deprecated("Use =/= instead","???")
  def !==(that: SpinalEnumElement[T]): Bool = this =/= that

  private[core] override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = that match{
    case that : SpinalEnumCraft[T]          => super.assignFromImpl(that, target, kind)
    case that : Expression with EnumEncoded => super.assignFromImpl(that, target, kind)
    //    case that : DontCareNodeEnum => super.assignFromImpl(that, conservative)
  }

  override def isEquals(that: Any): Bool = {
    that match{
      case that: SpinalEnumCraft[_] if that.spinalEnum == spinalEnum    => wrapLogicalOperator(that, new Operator.Enum.Equal(spinalEnum));
      case that: SpinalEnumElement[_] if that.spinalEnum == spinalEnum  => wrapLogicalOperator(that(), new Operator.Enum.Equal(spinalEnum));
      case _                                                            => SpinalError("Incompatible test")
    }
  }
  override def isNotEquals(that: Any): Bool = {
    that match{
      case that: SpinalEnumCraft[_] if that.spinalEnum == spinalEnum    => wrapLogicalOperator(that, new Operator.Enum.NotEqual(spinalEnum));
      case that: SpinalEnumElement[_] if that.spinalEnum == spinalEnum  => wrapLogicalOperator(that(), new Operator.Enum.NotEqual(spinalEnum));
      case _                                                            => SpinalError("Incompatible test")
    }
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerEnum(spinalEnum)
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerEnum(spinalEnum)

  override def asBits: Bits = wrapCast(Bits(), new CastEnumToBits)

  override def assignFromBits(bits: Bits): Unit = {
    val c    = cloneOf(this)
    val cast = new CastBitsToEnum(this.spinalEnum)

    cast.input = bits.asInstanceOf[cast.T]
    c.assignFrom(cast)

    this := c
  }

  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = {
    assert(lo == 0, "Enumeration can't be partially assigned")
    assert(hi == getBitsWidth-1, "Enumeration can't be partially assigned")
    assignFromBits(bits)
  }

  override def getBitsWidth: Int = encoding.getWidth(spinalEnum)

  override def clone: this.type = {
    val res = new SpinalEnumCraft(spinalEnum).asInstanceOf[this.type]
    res.copyEncodingConfig(this)
    res
  }

  def init(enumElement: SpinalEnumElement[T]): this.type = {
    this.init(enumElement()).asInstanceOf[this.type]
  }

  /** Return the name of the parent */
  private[core] def getParentName: String = spinalEnum.getName()

  override def getZero: this.type = {
    val ret = clone
    ret.assignFromBits(B(0, getEncoding.getWidth(spinalEnum) bits))
    ret
  }

  private[core] override def weakClone: this.type = {
    val ret = new SpinalEnumCraft(spinalEnum).asInstanceOf[this.type]
    ret
  }

  override def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }

  override def assignDontCare(): this.type = {
    this.assignFrom(new EnumPoison(spinalEnum))
    this
  }

  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastEnum(this.spinalEnum, delay))
}


/**
  * Node representation which contains the value of an SpinalEnumElement
  */
class EnumLiteral[T <: SpinalEnum](val enum: SpinalEnumElement[T]) extends Literal with InferableEnumEncodingImpl {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "E"

  override def clone: this.type = {
    val ret = new EnumLiteral(enum).asInstanceOf[this.type]
    ret.copyEncodingConfig(this)
    ret
  }

  override def getValue(): BigInt = encoding.getValue(enum)

  private[core] override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    val str = encoding.getValue(enum).toString(2)
    "0" * (bitCount - str.length) + str
  }
  override def hasPoison() = false

  override def getDefinition: SpinalEnum = enum.spinalEnum

  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = enum.spinalEnum.defaultEncoding
}


class EnumPoison(val enum: SpinalEnum) extends Literal with InferableEnumEncodingImpl {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "E?"

  override def clone: this.type = {
    val ret = new EnumPoison(enum).asInstanceOf[this.type]
    ret.copyEncodingConfig(this)
    ret
  }

  override def getValue(): BigInt = throw new Exception("EnumPoison has no value")

  private[core] override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    val str = poisonSymbol.toString * encoding.getWidth(enum)
    "0" * (bitCount - str.length) + str
  }

  override def getDefinition: SpinalEnum = enum
  override def hasPoison() = true
  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = enum.defaultEncoding
}



/**
  * Trait to define an encoding
  */
trait SpinalEnumEncoding extends Nameable with ScalaLocated{
  /** Return the width of the encoding  */
  def getWidth(enum: SpinalEnum): Int
  /** Return the value of the encoding */
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt
  def getElement[T <: SpinalEnum](element: BigInt, enum : T): SpinalEnumElement[T]

  def isNative: Boolean
}


/**
  * Inferred encoding
  */
object inferred extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = ???
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = ???
  override def getElement[T <: SpinalEnum](element: BigInt, enum : T): SpinalEnumElement[T] = ???
  override def isNative: Boolean = ???
}


/**
  * Native encoding
  */
object native extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.elements.length)
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = element.position
  override def getElement[T <: SpinalEnum](element: BigInt, enum : T): SpinalEnumElement[T] = enum.elements(element.toInt)

  override def isNative = true
  setWeakName("native")
}


/**
  * Binary Sequential
  * @example{{{ 000, 001, 010, 011, 100, 101, .... }}}
  */
object binarySequential extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = log2Up(enum.elements.length)
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = element.position
  override def getElement[T <: SpinalEnum](element: BigInt, enum : T): SpinalEnumElement[T] = enum.elements(element.toInt)
  override def isNative = false
  setWeakName("binary_sequential")
}


/**
  * Binary One hot encoding
  * @example{{{ 001, 010, 100 }}}
  */
object binaryOneHot extends SpinalEnumEncoding{
  override def getWidth(enum: SpinalEnum): Int = enum.elements.length
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = BigInt(1) << element.position
  override def getElement[T <: SpinalEnum](element: BigInt, enum : T): SpinalEnumElement[T] = enum.elements(element.bitLength-1)
  override def isNative = false
  setWeakName("binary_one_hot")
}


/**
  * Used to create a custom encoding
  *
  * @example {{{
  *   object BR extends SpinalEnum{
  *     val NE, EQ, J, JR = newElement()
  *     defaultEncoding = SpinalEnumEncoding("opt")(
  *         EQ -> 0,
  *         NE -> 1,
  *         J  -> 2,
  *         JR -> 3 )
  *   }
  * }}}
  *
  */
object SpinalEnumEncoding{

  def apply[X <: SpinalEnum](name: String)(spec: (SpinalEnumElement[X], BigInt)*): SpinalEnumEncoding = {
    val map: Map[SpinalEnumElement[X], BigInt] = spec.toMap
    list(name)(map)
  }

  def apply(name: String, spec: BigInt => BigInt): SpinalEnumEncoding = apply(spec).setName(name)

  def apply(spec: BigInt => BigInt): SpinalEnumEncoding = new SpinalEnumEncoding {
    override def getWidth(enum: SpinalEnum): Int = log2Up(enum.elements.map(getValue(_)).max)
    override def isNative: Boolean = false
    override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = spec(element.position)
    override def getElement[T <: SpinalEnum](element: BigInt, enum: T) = ???
  }

  def list[X <: SpinalEnum](name: String)(spec: Map[SpinalEnumElement[X], BigInt]): SpinalEnumEncoding = list(spec).setName(name)

  def list[X <: SpinalEnum](spec: Map[SpinalEnumElement[X], BigInt]): SpinalEnumEncoding = {
    if(spec.size != spec.head._1.spinalEnum.elements.size){
      SpinalError("All elements of the enumeration should be mapped")
    }

    return new SpinalEnumEncoding {
      val width = log2Up(spec.values.foldLeft(BigInt(0))((a, b) => if(a > b) a else b) + 1)
      override def getWidth(enum: SpinalEnum): Int = width
      override def isNative: Boolean = false
      override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = {
        return spec(element.asInstanceOf[SpinalEnumElement[X]])
      }
      override def getElement[T <: SpinalEnum](element: BigInt, enum: T) = ???
    }
  }
}
