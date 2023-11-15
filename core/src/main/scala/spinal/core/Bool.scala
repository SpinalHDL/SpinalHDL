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

import spinal.core.internals.Operator.Formal
import spinal.core.internals._
import spinal.idslplugin.Location

/** Bool factory used for instance by the IODirection to create a in/out Bool() */
trait BoolFactory {
  @deprecated("Use `Bool()` (with braces) instead")
  def Bool: Bool = Bool()

  /** Create a new Bool */
  def Bool(u: Unit = ()): Bool = new Bool

  /** Create a new Bool with a value */
  def Bool(value: Boolean)(implicit loc: Location): Bool = BoolLiteral(value, Bool().setAsTypeNode())
}

/**
  * The Bool type corresponds to a boolean value (True or False)
  *
  * @example {{{
  *     val myBool = Bool()
  *     myBool := False
  *     myBool := Bool(false)
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Bool Bool Documentation]]
  */
class Bool extends BaseType with DataPrimitives[Bool]  with BaseTypePrimitives[Bool]  with BitwiseOp[Bool]{

  override def getTypeObject = TypeBool

  override def getBitsWidth: Int = 1

  override def opName: String = "Bool"

  private[spinal] override def _data: Bool = this

  /**
    * Logical AND
    * @example{{{ val result = myBool1 && myBool2 }}}
    * @return a Bool assign with the AND result
    */
  def &&(b: Bool): Bool = wrapLogicalOperator(b, new Operator.Bool.And)
  override def &(b: Bool): Bool = this && b

  /**
    * Logical OR
    * @example{{{ val result = myBool1 || myBool2 }}}
    * @return a Bool assign with the OR result
    */
  def ||(b: Bool): Bool = wrapLogicalOperator(b, new Operator.Bool.Or)
  override def |(b: Bool): Bool = this || b

  override def ^(b: Bool): Bool  = wrapLogicalOperator(b, new Operator.Bool.Xor)

  /**
    * Logical NOT
    * @example{{{ val result = !myBool1 }}}
    * @return a Bool assign with the NOT result
    */
  def unary_! : Bool = wrapUnaryOperator(new Operator.Bool.Not)

  override def unary_~ : Bool = ! this

  /** this is assigned to True */
  def set(): Unit = this := True
  /** this is assigned to False */
  def clear(): Unit = this := False

  /** Set to True.
    *
    * Direct call to `set()` to reduce the number of nodes in the netlist.
    */
  override def setAll(): this.type = {
    this.set()
    this
  }

  /** Set to False.
    *
    * Direct call to `clear()` to reduce the number of nodes in the netlist.
    */
  override def clearAll(): this.type = {
    this.clear()
    this
  }

  /**
    * this is assigned to True when cond is True
    * @example{{{ myBool.setWhen(cond) }}}
    * @param cond a Bool condition
    * @return this is assigned to True when cond is True
    */
  def setWhen(cond: Bool)(implicit loc: Location): Bool   = { when(cond){ this := True }; this }
  /** this is assigned to False when cond is True */
  def clearWhen(cond: Bool)(implicit loc: Location): Bool = { when(cond){ this := False }; this }

  /**
   * this is assigned to True when cond is True and the current value of this is False. Useful for
   * coding a simple boolean state machine. riseWhen() is typically paired with fallWhen() but also works
   * together with setWhen() and clearWhen().
   *
   * @example{{{ val active = RegInit(False) riseWhen(request) fallWhen(acknowledge) }}}
   * @param cond a Bool condition
   * @return this is rising when cond is True
   * */
  def riseWhen(cond: Bool)(implicit loc: Location): Bool = setWhen((!this) && cond)
  /** this is assigned to False when cond is True and the current value of this is True. see riseWhen() */
  def fallWhen(cond: Bool)(implicit loc: Location): Bool = clearWhen((this) && cond)

  /**
   * this is inverted when cond is True
   * @example{{{ mybool.toggleWhen(request) }}}
   * @param cond a Bool condition
   * @return this is inverted when cond is True
   */
  def toggleWhen(cond: Bool)(implicit loc: Location): Bool = { when(cond){ this := !this }; this }

  /**
    * Rising edge detection of this with an initial value
    * @example{{{ val res = myBool.rise(False) }}}
    * @param initAt the initial value
    * @return a Bool
    */
  def rise(initAt: Bool): Bool = this && ! RegNext(this).init(initAt)
  /** Rising edge detection */
  def rise(): Bool = this && ! RegNext(this)

  /**
    * Falling edge detection of this with an initial value
    * @example{{{ val res = myBool.fall(False) }}}
    * @param initAt the initial value
    * @return a Bool
    */
  def fall(initAt: Bool): Bool = ! this && RegNext(this).init(initAt)
  /** Falling edge detection */
  def fall(): Bool = ! this && RegNext(this)

  /**
    * Edge detection of this with an initial value
    * @example{{{ val res = myBool.edge(False) }}}
    * @param initAt the initial value
    * @return a Bool
    */
  def edge(initAt: Bool): Bool = this ^ RegNext(this).init(initAt)
  /** Edge detection */
  def edge(): Bool = this ^ RegNext(this)

  /**
    * Detect all edges (falling, rising, toogling)
    * @example{{{
    *         val res = myBool.edges()
    *         when(res.fall){...}
    *         when(res.rise){...}
    *         when(res.toggle){...}
    *         }}}
    * @param initAt the initial value
    * @return a BoolEdges
    */
  def edges(initAt: Bool): BoolEdges = {
    val ret = BoolEdges()
    val old = RegNext(this) init(initAt)

    ret.rise   := !old && this
    ret.fall   := old && !this
    ret.toggle := old =/= this

    ret
  }

  /** Edge detection without intial value */
  def edges(): BoolEdges = {
    val ret = BoolEdges()
    val old = RegNext(this)

    ret.rise   := !old && this
    ret.fall   := old && !this
    ret.toggle := old =/= this

    ret
  }

  override def assignFromBits(bits: Bits): Unit = {
    assert(widthOf(bits) == 1 || bits.hasTag(tagAutoResize))
    this := bits(0)
  }

  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = {
    assert(hi == 0, "Expect hi == 0 in assignFromBits")
    assert(low == 0, "Expect low == 0 in assignFromBits")
    assignFromBits(bits)
  }

  /**
    * Cast a Bool to an UInt
    * @example{{{ myUInt := myBool.asUInt }}}
    * @return an UInt data
    */
  def asUInt: UInt = asBits.asUInt

  /**
    * Cast a Bool to an SInt
    * @example{{{ mySInt := myBool.asSInt }}}
    * @return a SInt data
    */
  def asSInt: SInt = asBits.asSInt
  def asSInt(bitCount: BitCount): SInt = asBits.asSInt.resize(bitCount.value)

  /**
    * Cast a Bool to an UInt of a given width
    * @example{{{ myUInt := myBool.asUInt(8 bits) }}}
    * @param bitCount the width of the UInt
    * @return an UInt data of a given length initialize to this
    */
  def asUInt(bitCount: BitCount): UInt = asBits.asUInt.resize(bitCount.value)

  /**
    * Cast a Bool to an Bits of a given width
    * @param bitCount the width of the Bits
    * @return a Bits data of a given length initialize to this
    */
  def asBits(bitCount: BitCount): Bits = asBits.resize(bitCount.value)

  override def asBits: Bits = wrapCast(Bits(), new CastBoolToBits)

  private[core] override def isEqualTo(that: Any): Bool = {
    that match {
      case that: Bool => wrapLogicalOperator(that,new Operator.Bool.Equal);
      case _          => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  private[core] override def isNotEqualTo(that: Any): Bool = {
    that match {
      case that: Bool => wrapLogicalOperator(that,new Operator.Bool.NotEqual);
      case _          => SpinalError(s"Don't know how compare $this with $that"); null
    }
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerBool
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerBool

  private[core] override def weakClone: this.type = new Bool().asInstanceOf[this.type]

  override def getZero: this.type = False.asInstanceOf[this.type]

  /**
    * Class used to write conditional operation on Data value
    * @example {{{ val res = myBool ? myBits1 | myBits2 }}}
    */
  class MuxBuilder[T <: Data](whenTrue: T){
    def |(whenFalse: T): T = Mux(Bool.this, whenTrue, whenFalse)
    def otherwise(whenFalse: T): T = Mux(Bool.this, whenTrue, whenFalse)
  }

  /** Conditional operation for Data value */
  def ?[T <: Data](whenTrue: T) = new MuxBuilder(whenTrue)

  /**
    * Class used to write conditional operation on Enumeration value
    * @example {{{ val res = myBool ? myEnum1 | myEnum2 }}}
    * @note implicit conversion is used to send SpinalEnumElement
    */
  case class MuxBuilderEnum[T <: SpinalEnum](whenTrue: SpinalEnumCraft[T]){
    def |(whenFalse: SpinalEnumCraft[T]): SpinalEnumCraft[T] = Mux(Bool.this, whenTrue, whenFalse)
  }

  /** Conditional operation for Enumeration value */
  def ?[T <: SpinalEnum](whenTrue: SpinalEnumCraft[T]) = MuxBuilderEnum(whenTrue)

  override def assignDontCare(): this.type = {
    this.assignFrom(new BoolPoison())
    this
  }

  def ===(that: MaskedBoolean): Bool = that === this
  /** BitVector is not equal to MaskedLiteral */
  def =/=(that: MaskedBoolean): Bool = that =/= this

  def init(value : Boolean) : Bool = this.init(Bool(value))

  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastBool(delay))

  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = this.assignFrom(new Operator.Formal.RandomExpBool(kind))

  def allowOutOfRangeLiterals : this.type = {
    spinal.core.allowOutOfRangeLiterals.doIt(this)
    this
  }

  def mux[T <: Data](t: T, f: T): T = this ? t | f
  def mux[T <: SpinalEnum](t: SpinalEnumElement[T], f: SpinalEnumElement[T]): SpinalEnumCraft[T] = this ? t | f
}

/**
  * Bundle for the edge detection
  */
case class BoolEdges() extends Bundle{
  val rise, fall, toggle = Bool()

  @deprecated("Use toggle","???")
  def toogle = toggle
}
