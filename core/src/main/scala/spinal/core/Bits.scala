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

/**
  * Bits factory used for instance by the IODirection to create a in/out Bits
  */
trait BitsFactory {
  /** Create a new Bits */
  def Bits(u: Unit = ()): Bits = new Bits()
  /** Create a new Bits of a given width */
  def Bits(width: BitCount): Bits = Bits().setWidth(width.value)
}


/**
  * The Bits type corresponds to a vector of bits that does not convey any arithmetic meaning.
  *
  * @example {{{
  *     val myBits1 = Bits(32 bits)
  *     val myBits2 = B(25, 8 bits)
  *     val myBits3 = B"8'xFF"
  *     val myBits4 = B"1001_0011
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Bits Bits Documentation]]
  */
class Bits extends BitVector with DataPrimitives[Bits] with BaseTypePrimitives[Bits] with BitwiseOp[Bits]{

  override def getTypeObject  = TypeBits

  override def opName: String = "Bits"

  override type T = Bits

  private[spinal] override def _data: Bits = this

  /**
    * Concatenation between two Bits
    * @example{{{ val myBits2 = bits1 ## bits2 }}}
    * @param right a Bits to append
    * @return a new Bits of width (w(this) + w(right))
    */
  def ##(right: Bits): Bits = wrapBinaryOperator(right, new Operator.Bits.Cat)

  /* Operator defined in BitwiseOp */
  override def |(right: Bits): Bits = wrapBinaryOperator(right, new Operator.Bits.Or)
  override def &(right: Bits): Bits = wrapBinaryOperator(right, new Operator.Bits.And)
  override def ^(right: Bits): Bits = wrapBinaryOperator(right, new Operator.Bits.Xor)
  override def unary_~ : Bits = wrapUnaryOperator(new Operator.Bits.Not)

  /**
    * Logical shift right (output width will decrease)
    * @example{{{ val result = myBits >> 4 }}}
    * @param that the number of shift
    * @return a Bits of width : w(this) - that bits
    */
  def >>(that: Int): Bits = wrapConstantOperator(new Operator.Bits.ShiftRightByInt(that))

  /**
    * Logical shift left (output width will increase)
    * @example{{{ val result = myBits << 4 }}}
    * @param that the number of shift
    * @return a Bits of width : w(this) + that bits
    */
  def <<(that: Int): Bits = wrapConstantOperator(new Operator.Bits.ShiftLeftByInt(that))

  /** Logical shift right (output width == input width) */
  def >>(that: UInt): Bits = wrapBinaryOperator(that, new Operator.Bits.ShiftRightByUInt)
  /** Logical shift left (output width will increase of w(this) + max(that) bits */
  def <<(that: UInt): Bits = wrapBinaryOperator(that, new Operator.Bits.ShiftLeftByUInt)

  /**
    * Logical shift right (output width == input width)
    * @example{{{ val result = myBits |>> 4 }}}
    * @param that the number of shift
    * @return a Bits of width : w(this)
    */
  def |>>(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftRightByIntFixedWidth(that))
  /** Logical shift left (output width == input width) */
  def |<<(that: Int): Bits  = wrapConstantOperator(new Operator.Bits.ShiftLeftByIntFixedWidth(that))
  /** Logical shift Right (output width == input width) */
  def |>>(that: UInt): Bits = this >> that
  /** Logical shift left (output width == input width) */
  def |<<(that: UInt): Bits = wrapBinaryOperator(that, new Operator.Bits.ShiftLeftByUIntFixedWidth)

  override def rotateLeft(that: Int): Bits = {
    val width   = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) ## this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): Bits = {
    val width   = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) ## this(this.high downto thatMod)
  }

  /**
    * Assign a range value to a Bits
    * @example{{{ core.io.interrupt = (0 -> uartCtrl.io.interrupt, 1 -> timerCtrl.io.interrupt, default -> false)}}}
    * @param rangesValue The first range value
    * @param _rangesValues Others range values
    */
  def :=(rangesValue: (Any, Any), _rangesValues: (Any, Any)*): Unit = {
    val rangesValues = rangesValue +: _rangesValues
    B.applyTuples(this, rangesValues)
  }

  def :=(value : String) : Unit = this := B(value)

  override def assignFromBits(bits: Bits): Unit = this := bits
  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = this (hi downto lo).assignFromBits(bits)

  /**
    * Cast a Bits to a SInt
    * @example {{{ val mySInt = myBits.asSInt }}}
    * @return a SInt data
    */
  def asSInt: SInt = wrapCast(SInt(), new CastBitsToSInt)

  /**
    * Cast a Bits to an UInt
    * @example {{{ val myUInt = myBits.asUInt }}}
    * @return an UInt data
    */
  def asUInt: UInt = wrapCast(UInt(), new CastBitsToUInt)

  override def asBits: Bits = {
    val ret = new Bits()
    ret := this
    ret
  }

  /**
    * Cast a Bits to a given data type
    * @example{{{ val myUInt = myBits.toDataType(UInt) }}}
    * @param dataType the wanted data type
    * @return a new data type assign with the value of Bits
    */
  def toDataType[T <: Data](dataType: T): T = {
    val ret = cloneOf(dataType)
    ret.assignFromBits(this)
    ret
  }

  private[core] override def isEqualTo(that: Any): Bool = that match {
    case that: Bits          => wrapLogicalOperator(that, new Operator.Bits.Equal)
    case that: MaskedLiteral => that === this
    case _                   => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  private[core] override def isNotEqualTo(that: Any): Bool = that match {
    case that: Bits          => wrapLogicalOperator(that, new Operator.Bits.NotEqual)
    case that: MaskedLiteral => that =/= this
    case _                   => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerBits
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerBits

  def valueRange: Range = {
    assert(getWidth < 32)
    0 to (1 << getWidth)-1
  }

  override def resize(width: Int): Bits = wrapWithWeakClone({
    val node   = new ResizeBits
    node.input = this
    node.size  = width
    node
  })

  override def resize(width: BitCount) : Bits = resize(width.value)

  override def resizeFactory: Resize = new ResizeBits

  /**
    * Resize by keeping MSB at the same place
    * If the final size is bigger than the original size, the leftmost bits are filled with zeroes
    * if the final size is smaller, only width MSB are kept
    * @param width Final width
    * @return Resized bits vector
    */
  def resizeLeft(width: Int): Bits = {
    val lengthDifference = width - this.getWidth

    if (lengthDifference >= 0) {
      this ## B(0, lengthDifference bits)
    } else {
      this(this.getWidth - 1 downto math.abs(lengthDifference))
    }
  }

  override def apply(bitId: Int): Bool  = newExtract(bitId, new BitsBitAccessFixed)
  override def apply(bitId: UInt): Bool = newExtract(bitId, new BitsBitAccessFloating)
  override def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1,offset, new BitsRangedAccessFixed).setWidth(bitCount.value)
  override def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset,bitCount.value, new BitsRangedAccessFloating).setWidth(bitCount.value)

  private[core] override def weakClone: this.type = new Bits().asInstanceOf[this.type]
  override def getZero: this.type = B(0, this.getWidth bits).asInstanceOf[this.type]
  override def getZeroUnconstrained: this.type = B(0).asInstanceOf[this.type]
  override def getAllTrue: this.type = B((BigInt(1) << this.getWidth) - 1, this.getWidth bits).asInstanceOf[this.type]
  override def setAll(): this.type = {
    this := (BigInt(1) << this.getWidth) - 1
    this.asInstanceOf[this.type]
  }

  override def assignDontCare(): this.type = {
    this.assignFrom(BitsLiteral(BigInt(0), (BigInt(1) << this.getWidth) - 1, widthOf(this)))
    this
  }


  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastBits(delay))
  def reversed = B(asBools.reverse).asInstanceOf[this.type]

  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = this.assignFrom(new Operator.Formal.RandomExpBits(kind, widthOf(this)))

  /**
   * Return a instance of the paramter which alias this.Bits in both read and assignments accesses.
   * Usefull for union like data structures.
   * @param t The type in which the alias will be
   * @return The alias
   */
  def aliasAs[T <: Data](t : HardType[T]) : T = {
    val wrap = t()
    wrap.assignFromBits(this.asBits.resized)
    var offsetCounter = 0
    for (e <- wrap.flatten) {
      val eWidth = e.getBitsWidth

      e.compositeAssign = new Assignable {
        val offset = offsetCounter

        override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
          def getBits(w: Int) = that match {
            case that: BitVector if widthOf(that) != w => {
              val tmp = cloneOf(that).setWidth(w)
              tmp := that
              tmp.asBits
            }
            case that: Data => that.asBits
          }

          target match {
            case x: BaseType => Bits.this.compositAssignFrom(getBits(eWidth), RangedAssignmentFixed(Bits.this, offset + eWidth - 1, offset), kind)
            case x: BitAssignmentFixed => Bits.this(offset + x.bitId).compositAssignFrom(that, Bits.this, kind)
            case x: BitAssignmentFloating => Bits.this(offset + x.bitId.asInstanceOf[UInt]).compositAssignFrom(that, Bits.this, kind)
            case x: RangedAssignmentFixed => Bits.this(offset + x.hi downto offset + x.lo).compositAssignFrom(getBits(x.getWidth), Bits.this, kind)
            case x: RangedAssignmentFloating => Bits.this(offset + x.offset.asInstanceOf[UInt], x.bitCount bits).compositAssignFrom(getBits(x.getWidth), Bits.this, kind)
          }
        }

        override def getRealSourceNoRec: Any = Bits.this
      }

      offsetCounter += eWidth
    }
    wrap
  }
}
