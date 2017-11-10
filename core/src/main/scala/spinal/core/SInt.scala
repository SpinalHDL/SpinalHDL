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

/**
  * SInt factory used for instance by the IODirection to create a in/out SInt
  */
trait SIntFactory{
  /** Create a new SInt */
  def SInt() = new SInt()
  /** Create a new SInt of a given width */
  def SInt(width: BitCount): SInt = SInt.setWidth(width.value)
}


/**
  * The SInt type corresponds to a vector of bits that can be used for signed integer arithmetic.
  *
  * @example{{{
  *     val mySInt = SInt(8 bits)
  *     mySInt    := S(4, 8 bits) + S"0000_1111"
  *     mySInt    := S(4) - S"h1A"
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Int SInt Documentation]]
  */
class SInt extends BitVector with Num[SInt] with MinMaxProvider with DataPrimitives[SInt] with BitwiseOp[SInt] {
  override def getTypeObject = TypeSInt
  override private[core] def resizeFactory: Resize = new ResizeSInt
  override def opName: String = "SInt"


  override type T = SInt

  private[spinal] override  def _data: SInt = this

  /**
    * Concatenation between two SInt
    * @example{{{ val mySInt = sInt1 @@ sInt2 }}}
    * @param that an SInt to append
    * @return a new SInt of width (width(this) + width(right))
    */
  def @@(that: SInt): SInt = S(this ## that)
  /** Concatenation between a SInt and UInt */
  def @@(that: UInt): SInt = S(this ## that)
  /** Concatenation between a SInt and a Bool */
  def @@(that: Bool): SInt = S(this ## that)

  override def +(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Add)
  override def -(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Sub)
  override def *(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Mul)
  override def /(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Div)
  override def %(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Mod)

  override def |(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Or)
  override def &(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.And)
  override def ^(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Xor)
  override def unary_~(): SInt = wrapUnaryOperator(new Operator.SInt.Not)

  override def < (right: SInt): Bool = wrapLogicalOperator(right, new Operator.SInt.Smaller)
  override def > (right: SInt): Bool = right < this
  override def <=(right: SInt): Bool = wrapLogicalOperator(right, new Operator.SInt.SmallerOrEqual)
  override def >=(right: SInt): Bool = right <= this

  /**
    * Negative number
    * @example{{{ val result = -mySInt }}}
    * @return return a negative number
    */
  def unary_-(): SInt = wrapUnaryOperator(new Operator.SInt.Minus)

  /**
    * Logical shift Right (output width == input width)
    * @example{{{ val result = mySInt >> myUIntShift }}}
    * @param that the number of right shift
    * @return a Bits of width : w(this)
    */
  def >>(that: UInt): SInt         = wrapBinaryOperator(that, new Operator.SInt.ShiftRightByUInt)
  /** Logical shift Left (output width will increase of : w(this) + max(that) bits  */
  def <<(that: UInt): SInt         = wrapBinaryOperator(that, new Operator.SInt.ShiftLeftByUInt)
  override def >>(that: Int): SInt = wrapConstantOperator(new Operator.SInt.ShiftRightByInt(that))
  override def <<(that: Int): SInt = wrapConstantOperator(new Operator.SInt.ShiftLeftByInt(that))

  /**
    * Logical shift right (output width == input width)
    * @example{{{ val result = myUInt |>> 4 }}}
    * @param that the number of right shift
    * @return a Bits of width : w(this)
    */
  def |>>(that: Int): SInt  = wrapConstantOperator(new Operator.SInt.ShiftRightByIntFixedWidth(that))
  /** Logical shift left (output width == input width) */
  def |<<(that: Int): SInt  = wrapConstantOperator(new Operator.SInt.ShiftLeftByIntFixedWidth(that))
  /** Logical shift Right (output width == input width) */
  def |>>(that: UInt): SInt = this >> that
  /** Logical shift left (output width == input width) */
  def |<<(that: UInt): SInt = wrapBinaryOperator(that, new Operator.SInt.ShiftLeftByUIntFixedWidth)

  override def rotateLeft(that: Int): SInt = {
    val width = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) @@ this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): SInt = {
    val width = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) @@ this(this.high downto thatMod)
  }

  /**
    * Absolute value of a SInt
    * @example {{{ myUInt := mySInt.abs }}}
    * @return a UInt assign with the absolute value of the SInt
    */
  def abs: UInt = Mux(this.msb, ~this, this).asUInt + this.msb.asUInt
  /** Return the absolute value of the SInt when enable is True */
  def abs(enable: Bool): UInt = Mux(this.msb && enable, ~this, this).asUInt + (this.msb && enable).asUInt

  /**
    * Assign a range value to a SInt
    * @example{{{ core.io.interrupt = (0 -> uartCtrl.io.interrupt, 1 -> timerCtrl.io.interrupt, default -> false)}}}
    * @param rangesValue The first range value
    * @param _rangesValues Others range values
    */
  def :=(rangesValue : Tuple2[Any, Any],_rangesValues: Tuple2[Any, Any]*) : Unit = {
    val rangesValues = rangesValue +: _rangesValues
    S.applyTuples(this, rangesValues)
  }

  override def assignFromBits(bits: Bits): Unit = this := bits.asSInt
  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = this(hi, lo).assignFromBits(bits)

  /**
    * Cast a SInt into an UInt
    * @example {{{ myUInt := mySInt.asUInt }}}
    * @return a UInt data
    */
  def asUInt: UInt = wrapCast(UInt(), new CastSIntToUInt)

  override def asBits: Bits = wrapCast(Bits(), new CastSIntToBits)

  private[core] override def isEquals(that: Any): Bool = that match {
      case that: SInt           => wrapLogicalOperator(that,new Operator.SInt.Equal)
      case that: MaskedLiteral  => that === this
      case _                    => SpinalError(s"Don't know how compare $this with $that"); null
    }


  private[core] override def isNotEquals(that: Any): Bool = that match {
    case that: SInt          => wrapLogicalOperator(that,new Operator.SInt.NotEqual)
    case that: MaskedLiteral => that =/= this
    case _                   => SpinalError(s"Don't know how compare $this with $that"); null
  }


  private[core] override def newMultiplexer(sel: Bool, whenTrue: Expression, whenFalse: Expression): Multiplexer = newMultiplexer(sel, whenTrue, whenFalse, new MultiplexerSInt)

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node = new ResizeSInt
    node.input = this
    node.size = width
    node
  })

  override def minValue: BigInt = -(BigInt(1) << (getWidth - 1))
  override def maxValue: BigInt =  (BigInt(1) << (getWidth - 1)) - 1
  override def setAll(): Unit = this := (if(getWidth != 0) -1 else 0)

  override def apply(bitId: Int): Bool = newExtract(bitId, new SIntBitAccessFixed)
  override def apply(bitId: UInt): Bool = newExtract(bitId, new SIntBitAccessFloating)
  override def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset + bitCount.value - 1, offset, new SIntRangedAccessFixed).setWidth(bitCount.value)
  override def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset, bitCount.value, new SIntRangedAccessFloating).setWidth(bitCount.value)

  private[core] override def weakClone: this.type = new SInt().asInstanceOf[this.type]
  override def getZero: this.type = S(0, this.getWidth bits).asInstanceOf[this.type]
  override def getZeroUnconstrained: this.type = S(0).asInstanceOf[this.type]
  override def getAllTrue: this.type = S(if(getWidth != 0) -1 else 0, this.getWidth bits).asInstanceOf[this.type]

  override def assignDontCare(): this.type = {
    this.assignFrom(SIntLiteral(BigInt(0), (BigInt(1) << this.getWidth)-1, widthOf(this)))
    this
  }
}
