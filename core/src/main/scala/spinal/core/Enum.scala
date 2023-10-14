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
import spinal.idslplugin.Location

import scala.collection.mutable.ArrayBuffer


/**
  * Definition of an element of the enumeration
  *
  * @param spinalEnum parent of the element (SpinalEnum)
  * @param position position of the element
  */
class SpinalEnumElement[T <: SpinalEnum](val spinalEnum: T, val position: Int) extends Nameable {

  def getSignature() : Any = List(position, getName(""))

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
 * @param defaultEncoding encoding of the senum
 */
class SpinalEnum(var defaultEncoding: SpinalEnumEncoding = native) extends Nameable with ScalaLocated {

  assert(defaultEncoding != inferred, "Enum definition should not have 'inferred' as default encoding")

  type C = SpinalEnumCraft[this.type]
  type E = SpinalEnumElement[this.type]

  var forcedPrefixEnable : Option[Boolean] = None
  var forcedGlobalEnable : Option[Boolean] = None

  private[core] def isPrefixEnable = forcedPrefixEnable.getOrElse(GlobalData.get.config.enumPrefixEnable)
  private[core] def isGlobalEnable = forcedGlobalEnable.getOrElse(GlobalData.get.config.enumGlobalEnable)

  /** Contains all elements of the enumeration */
  @dontName val elements = ArrayBuffer[SpinalEnumElement[this.type]]()

  def getSignature() : Any = List(getName(""), defaultEncoding.getSignature(), isPrefixEnable, isGlobalEnable, elements.map(_.getSignature()).toList)

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

  def rawElementName() = {
    forcedPrefixEnable = Some(false)
  }

  def setLocal() = {
    forcedGlobalEnable = Some(false)
  }
  def setGlobal() = {
    forcedGlobalEnable = Some(true)
  }
}


/**
  * Hardware representation of an enumeration
  */
class SpinalEnumCraft[T <: SpinalEnum](var spinalEnum: SpinalEnum) extends BaseType with InferableEnumEncodingImpl  with BaseTypePrimitives[SpinalEnumCraft[T]]  with DataPrimitives[SpinalEnumCraft[T]] {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "EnumCraft"

  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = spinalEnum.defaultEncoding

  override private[core] def canSymplifyIt = super.canSymplifyIt && (this.encodingChoice == InferableEnumEncodingImplChoiceUndone)

  override def getDefinition: SpinalEnum = spinalEnum
  override def swapEnum(e: SpinalEnum) = spinalEnum = e

  private[spinal] override def _data: SpinalEnumCraft[T] = this

  private[core] def assertSameType(than: SpinalEnumCraft[_]): Unit = if (spinalEnum != than.spinalEnum) SpinalError("Enum is assigned by a incompatible enum")

  def :=(that: SpinalEnumElement[T]): Unit = new DataPimper(this) := that.craft()
  def ===(that: SpinalEnumElement[T]): Bool = this === that.craft()
  def =/=(that: SpinalEnumElement[T]): Bool = this =/= that.craft()

  @deprecated("Use =/= instead","???")
  def !==(that: SpinalEnumElement[T]): Bool = this =/= that

  protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = that match{
    case that : SpinalEnumCraft[T]          => super.assignFromImpl(that, target, kind)
    case that : Expression with EnumEncoded => super.assignFromImpl(that, target, kind)
    //    case that : DontCareNodeEnum => super.assignFromImpl(that, conservative)
  }

  override def isEqualTo(that: Any): Bool = {
    that match{
      case that: SpinalEnumCraft[_] if that.spinalEnum == spinalEnum    => wrapLogicalOperator(that, new Operator.Enum.Equal(spinalEnum));
      case that: SpinalEnumElement[_] if that.spinalEnum == spinalEnum  => wrapLogicalOperator(that(), new Operator.Enum.Equal(spinalEnum));
      case _                                                            => SpinalError("Incompatible test")
    }
  }
  override def isNotEqualTo(that: Any): Bool = {
    that match{
      case that: SpinalEnumCraft[_] if that.spinalEnum == spinalEnum    => wrapLogicalOperator(that, new Operator.Enum.NotEqual(spinalEnum));
      case that: SpinalEnumElement[_] if that.spinalEnum == spinalEnum  => wrapLogicalOperator(that(), new Operator.Enum.NotEqual(spinalEnum));
      case _                                                            => SpinalError("Incompatible test")
    }
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerEnum(spinalEnum)
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerEnum(spinalEnum)

  override def asBits: Bits = wrapCast(new Bits(), new CastEnumToBits)

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

  override def getBitsWidth: Int = encoding match {
    case null => SpinalError("Trying to get the bits width of a enumeration which has no fixed encoding, it has to be fixed ex : myEnum.fixEncoding(native)")
    case _ => encoding.getWidth(spinalEnum)
  }

  override def clone: this.type = {
    val res = new SpinalEnumCraft(spinalEnum).asInstanceOf[this.type]
    res.copyEncodingConfig(this)
    res.asInstanceOf[this.type]
  }

  def init(enumElement: SpinalEnumElement[T]): this.type = {
    this.init(enumElement()).asInstanceOf[this.type]
  }

  /** Return the name of the parent */
  private[core] def getParentName: String = spinalEnum.getName()

  override def getZero: this.type = {
    val ret = clone
    ret.assignFromBits(B(0, getEncoding.getWidth(spinalEnum) bits))
    ret.asInstanceOf[this.type]
  }

  private[core] override def weakClone: this.type = {
    val ret = new SpinalEnumCraft(spinalEnum).asInstanceOf[this.type]
    ret.asInstanceOf[this.type]
  }

  override def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }

  override def assignDontCare(): this.type = {
    this.assignFrom(new EnumPoison(spinalEnum))
    this
  }

  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastEnum(this.spinalEnum, delay))

  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = this.assignFrom(new Operator.Formal.RandomExpEnum(this.spinalEnum, kind))
}


/**
  * Node representation which contains the value of an SpinalEnumElement
  */
class EnumLiteral[T <: SpinalEnum](var senum: SpinalEnumElement[_ <: SpinalEnum]) extends Literal with InferableEnumEncodingImpl {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "E"

  override def clone: this.type = {
    val ret = new EnumLiteral(senum).asInstanceOf[this.type]
    ret.copyEncodingConfig(this)
    ret.asInstanceOf[this.type]
  }

  override def getValue(): BigInt = encoding.getValue(senum)

  private[core] override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    val str = encoding.getValue(senum).toString(2)
    "0" * (bitCount - str.length) + str
  }
  override def hasPoison() = false

  override def getDefinition: SpinalEnum = senum.spinalEnum
  override def swapEnum(e: SpinalEnum) = senum = e.elements(senum.position)

  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = senum.spinalEnum.defaultEncoding
}


class EnumPoison(var senum: SpinalEnum) extends Literal with InferableEnumEncodingImpl {

  override def getTypeObject: Any = TypeEnum

  override def opName: String = "E?"

  override def clone: this.type = {
    val ret = new EnumPoison(senum).asInstanceOf[this.type]
    ret.copyEncodingConfig(this)
    ret.asInstanceOf[this.type]
  }

  override def getValue(): BigInt = throw new Exception("EnumPoison has no value")

  private[core] override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    val str = poisonSymbol.toString * encoding.getWidth(senum)
    "0" * (bitCount - str.length) + str
  }

  override def getDefinition: SpinalEnum = senum
  override def swapEnum(e: SpinalEnum) = senum = e
  override def hasPoison() = true
  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = senum.defaultEncoding
}



/**
  * Trait to define an encoding
  */
trait SpinalEnumEncoding extends Nameable with ScalaLocated{
  /** Return the width of the encoding  */
  def getWidth(senum: SpinalEnum): Int
  /** Return the value of the encoding */
  def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt
  def getElement[T <: SpinalEnum](element: BigInt, senum : T): SpinalEnumElement[T]

  def isNative: Boolean = false

  def getSignature() : Any = this
}


/**
  * Inferred encoding
  */
object inferred extends SpinalEnumEncoding{
  override def getWidth(senum: SpinalEnum): Int = ???
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = ???
  override def getElement[T <: SpinalEnum](element: BigInt, senum : T): SpinalEnumElement[T] = ???
  override def isNative: Boolean = ???
}


/**
  * Native encoding
  */
object native extends SpinalEnumEncoding{
  override def getWidth(senum: SpinalEnum): Int = log2Up(senum.elements.length)
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = element.position
  override def getElement[T <: SpinalEnum](element: BigInt, senum : T): SpinalEnumElement[T] = senum.elements(element.toInt)

  override def isNative = true
  setName("native")
}


/**
  * Binary Sequential
  * @example{{{ 000, 001, 010, 011, 100, 101, .... }}}
  */
object binarySequential extends SpinalEnumEncoding{
  override def getWidth(senum: SpinalEnum): Int = log2Up(senum.elements.length)
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = element.position
  override def getElement[T <: SpinalEnum](element: BigInt, senum : T): SpinalEnumElement[T] = senum.elements(element.toInt)
  setName("seq")
}


/**
  * Binary One hot encoding
  * @example{{{ 001, 010, 100 }}}
  */
object binaryOneHot extends SpinalEnumEncoding{
  override def getWidth(senum: SpinalEnum): Int = senum.elements.length
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = BigInt(1) << element.position
  override def getElement[T <: SpinalEnum](element: BigInt, senum : T): SpinalEnumElement[T] = senum.elements(element.bitLength-1)
  setName("oh")
}

/**
 * Gray encoding (sequentially assigned)
 * @example{{{ 000, 001, 011, 010, ... }}}
 * @note If used in FSM it is not ensured that only gray encoding preserving
 *       transitions are done. If that is needed e.g. for CDC reasons, the
 *       transitions must be checked manually.
 */
object graySequential extends SpinalEnumEncoding {
  override def getWidth(e: SpinalEnum) = log2Up(e.elements.length)
  override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = Gray.encode(element.position)
  override def getElement[T <: SpinalEnum](value: BigInt, enums: T): SpinalEnumElement[T] = enums.elements(Gray.decode(value).toInt)
  setName("graySeq")
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

  def apply(name: String, spec: Int => BigInt): SpinalEnumEncoding = apply(spec).setName(name)

  def apply(spec: Int => BigInt): SpinalEnumEncoding = new SpinalEnumEncoding {
    override def getWidth(senum: SpinalEnum): Int = log2Up(senum.elements.map(getValue(_)).max+1)
    override def isNative: Boolean = false
    override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = spec(element.position)
    override def getElement[T <: SpinalEnum](element: BigInt, senum: T) = ???
  }

  def list[X <: SpinalEnum](name: String)(spec: Map[SpinalEnumElement[X], BigInt]): SpinalEnumEncoding = list(spec).setName(name)

  def list[X <: SpinalEnum](spec: Map[SpinalEnumElement[X], BigInt]): SpinalEnumEncoding = {
    if(spec.size != spec.head._1.spinalEnum.elements.size){
      SpinalError("All elements of the enumeration should be mapped")
    }

    return new SpinalEnumEncoding {
      val width = log2Up(spec.values.foldLeft(BigInt(0))((a, b) => if(a > b) a else b) + 1)
      val specInv = spec.map(_.swap)
      override def getWidth(senum: SpinalEnum): Int = width
      override def isNative: Boolean = false
      override def getValue[T <: SpinalEnum](element: SpinalEnumElement[T]): BigInt = {
        return spec(element.asInstanceOf[SpinalEnumElement[X]])
      }
      override def getElement[T <: SpinalEnum](element: BigInt, senum: T) = specInv(element).asInstanceOf[SpinalEnumElement[T]]
    }
  }
}
