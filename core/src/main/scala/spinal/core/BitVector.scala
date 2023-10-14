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
  * BitVector is a family of types for storing multiple bits of information in a single value.
  * This type has three subtypes that can be used to model different behaviours:
  *     - Bits
  *     - UInt (unsigned integer)
  *     - SInt (signed integer)
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/TypeIntroduction BitVector Documentation]]
  */
abstract class BitVector extends BaseType with Widthable {

  /** Width of the BitVector (-1 = undefined) */
  private[core] var fixedWidth = -1

  /** Used to know the data type of the children class of BitVector */
  type T <: BitVector

  /** Return the upper bound */
  def high: Int = getWidth - 1
  /** Return the most significant bit */
  def msb: Bool = this(high)
  /** Return the least significant bit */
  def lsb: Bool = this(0)
  /** Return the range */
  @deprecated("Use bitsRange instead")
  def range: Range = bitsRange

  def bitsRange: Range = 0 until getWidth

  def reversed : this.type

  /** Logical OR of all bits */
//  def orR: Bool = this.asBits =/= 0
  def orR: Bool = {
    if(GlobalData.get.config.mode == VHDL) {
      this.asBits =/= 0
    } else {
      wrapUnaryWithBool(new Operator.BitVector.orR)
    }
  }
  /** Logical AND of all bits */
//  def andR: Bool = this.asBits === ((BigInt(1) << getWidth) - 1)
  def andR: Bool = {
    if(GlobalData.get.config.mode == VHDL) {
      this.asBits === ((BigInt(1) << getWidth) - 1)
    } else {
      wrapUnaryWithBool(new Operator.BitVector.andR)
    }
  }
  /** Logical XOR of all bits */
//  def xorR: Bool = this.asBools.reduce(_ ^ _)
  def xorR: Bool = {
    if(GlobalData.get.config.mode == VHDL) {
      this.asBools.reduce(_ ^ _)
    } else {
      wrapUnaryWithBool(new Operator.BitVector.xorR)
    }
  }

  /**
    * Compare a BitVector with a MaskedLiteral (M"110--0")
    * @example {{{ val myBool = myBits === M"0-1" }}}
    * @param that the maskedLiteral
    * @return a Bool data containing the result of the comparison
    */
  def ===(that: MaskedLiteral): Bool = this.isEqualTo(that)
  /** BitVector is not equal to MaskedLiteral */
  def =/=(that: MaskedLiteral): Bool = this.isNotEqualTo(that)

  def andMask(that : Bool) : this.type = (that ? this otherwise this.getZero).asInstanceOf[this.type]
  def orMask(that : Bool) : this.type = (that ? cloneOf(this).setAll() otherwise this).asInstanceOf[this.type]
  def xorMask(that : Bool) : this.type = (that ? (~this.asInstanceOf[BitVector with BitwiseOp[BitVector]]) otherwise this).asInstanceOf[this.type]

  /** Left rotation of that Bits */
  def rotateLeft(that: UInt): T = {
    val thatWidth = widthOf(that)
    val thisWidth = widthOf(this)

    require(thatWidth <= log2Up(thisWidth), s"RotateLeft thatWidth($thatWidth) <= log2Up(thisWidth) (${log2Up(thisWidth)})")

    var result = cloneOf(this).asInstanceOf[T]
    result := this.asInstanceOf[T]
    for(i <- that.range){
      result \= (that(i) ? result.rotateLeft(1 << i).asInstanceOf[T] | result)
    }
    result
  }

  /** Right rotation of that Bits */
  def rotateRight(that: UInt): T = {
    val thatWidth = widthOf(that)
    val thisWidth = widthOf(this)

    require(thatWidth <= log2Up(thisWidth), s"RotateRight thatWidth($thatWidth) <= log2Up(thisWidth) (${log2Up(thisWidth)})")

    var result = cloneOf(this).asInstanceOf[T]
    result := this.asInstanceOf[T]
    for(i <- that.range){
      result \= (that(i) ? result.rotateRight(1 << i).asInstanceOf[T] | result)
    }
    result
  }

  /** Left rotation of that bits */
  def rotateLeft(that: Int): T
  /** Right rotation of that bits */
  def rotateRight(that: Int): T

  private[core] def resizeFactory : Resize

  /** Return true if the BitVector has a fixed width */
  private[core] def isFixedWidth = fixedWidth != -1

  /** Unfix the width of the BitVector */
  private[core] def unfixWidth() = {
    fixedWidth           = -1
    widthWhenNotInferred = -1
    inferredWidth        = -1
  }

  private[core] def fixWidth() = {
    setWidth(getWidth)
  }

  /**
    * Set the width of the BitVector
    * @param width the width of the data
    * @return the BitVector of a given size
    */
  def setWidth(width: Int): this.type = {
    if(width < 0){
      LocatedPendingError(s"Width of $this is set by a negative number")
    }
    fixedWidth = width
    if (globalData.nodeAreInferringWidth) inferredWidth = fixedWidth
    this
  }

  override def getBitsWidth: Int = getWidth

  override def clone: this.type = {
    val res = super.clone
    if(this.fixedWidth == -1){
      res.fixedWidth = this.getWidth
    } else {
      res.fixedWidth = this.fixedWidth
    }
    res.asInstanceOf[this.type]
  }

  /**
    * Resize the bitVector to width
    * @example{{{ val res = myBits.resize(10) }}}
    * @return a resized bitVector
    */
  def resize(width: Int): BitVector
  def resize(width: BitCount): BitVector

  private[core] override def calcWidth: Int = {
    if (isFixedWidth) return fixedWidth
    var w = -1
    foreachStatements {
      s: AssignmentStatement =>
        s.target match {
          case target: BitVector => s.source match {
            case e: WidthProvider => w = Math.max(w, e.getWidth)
          }
          case target: BitVectorAssignmentExpression => w = Math.max(w, target.minimalTargetWidth)
        }
    }
    w
  }

  /**
    * Cast the BitVector into a Vector of Bool
    * @return a vector of Bool
    */
  def asBools: Vec[Bool] = signalCache(this, "asBools") {
    val vec = ArrayBuffer[Bool]()
    val bitCount = getWidth
    if (bitCount == -1) SpinalError("Can't convert to bools a Bit that has unspecified width value")
    for (i <- 0 until bitCount) vec += this (i)
    Vec(vec)
  }

  def asBool : Bool = {
    val ret = Bool()
    ret := lsb
    ret
  }

  private def toBitsInstance(x: BitVector): Bits ={
    x match{
      case x: Bits => x
      case x: SInt => x.asBits
      case x: UInt => x.asBits
    }
  }

  /**
    * Take lowerst n bits
    * @example {{{ val res = data10bits.take(4) }}}
    * @return data10bits(3 downto 0)
    */
  def take(n: Int): Bits = {
    val x = this(n - 1 downto 0)
    toBitsInstance(x)
  }

  def takeLow(n: Int): Bits = take(n)

  /**
    * Drop lowerst n bits
    * @example {{{ val res = data10bits.drop(4) }}}
    * @return data10bits(9 downto 4)
    */
  def drop(n: Int): Bits = {
    val ret = this(this.high downto n)
    toBitsInstance(ret)
  }

  def dropLow(n: Int): Bits = drop(n)
  /**
    * Take highest n bits
    * @example {{{ val res = data10bits.takeHigh(4) }}}
    * @return data10bits(9 downto 6)
    */
  def takeHigh(n: Int): Bits = drop(widthOf(this) - n)

  /**
    * Drop highest n bits
    * @example {{{ val res = data10bits.dropHigh(4) }}}
    * @return data10bits(5 downto 0)
    */
  def dropHigh(n: Int): Bits = take(widthOf(this) - n)

  /**
    * Split at n st bits
    * @example {{{ val res = data10bits.splitAt(4) }}}
    * @return (data10bits(8 downto 4), data10bits(3 downto 0))
    */
  def splitAt(n: Int): (Bits,Bits) = {
    require(n>=0)
    if(n==0){
      (toBitsInstance(this), Bits(0 bits))
    } else {
      (toBitsInstance(this(this.high downto n)), toBitsInstance(this(n - 1 downto 0)))
    }
  }

  /**
    * apart by a list of width
    * @example {{{
    *         val res = A.sliceBy(2, 3, 5)
    *         val res = A.sliceBy(List(2, 3, 5)) }}}
    * @return (List(A(1 downto 0), A(2 downto 4), A(9 downto 3))
    */
  def sliceBy(divisor: Int*): List[Bits] = sliceBy(divisor.toList)

  def sliceBy(divisor: List[Int]): List[Bits] = {
    val width  = widthOf(this)
    require(divisor.sum == width, s"the sum of ${divisor} =! ${this} width, cant parted")

    import scala.collection.mutable.ListBuffer

    val pool = ListBuffer.fill(divisor.size + 1)(0)
    (1 to divisor.size).foreach{ i =>
      pool(i) = pool(i - 1) + divisor(i - 1)
    }
    pool.take(divisor.size).zip(pool.tail).map{ case(pos, nxtpos) =>
      toBitsInstance(this(nxtpos - 1 downto pos))
    }.toList
  }

  /**
   * Split the BitVector into x slice
   *
   * @example {{{ val res = myBits.subdivideIn(3 slices) }}}
   * @param sliceCount the width of the slice
   * @param strict     allow `subdivideIn` to generate vectors with varying size
   * @return a Vector of slices
   */
  def subdivideIn(sliceCount: SlicesCount, strict: Boolean): Vec[T] = {
    val width = getWidth
    val dividesEvenly = width % sliceCount.value == 0
    require(!strict || dividesEvenly,
      s"subdivideIn can't evenly divide $width bit into ${sliceCount.value} slices as required by strict=true")
    val sliceWidth = width / sliceCount.value + (if (!dividesEvenly) 1 else 0)
    Vec(
      (0 until sliceCount.value)
        .map(i => this (i * sliceWidth, (width - i * sliceWidth) min sliceWidth bits).asInstanceOf[T])
    )
  }

  /**
   * Split the BitVector into slice of x bits
   *
   * @example {{{ val res = myBits.subdivideIn(3 bits) }}}
   * @param sliceWidth the width of the slice
   * @param strict     allow `subdivideIn` to generate vectors with varying size
   * @return a Vector of slices
   */
  def subdivideIn(sliceWidth: BitCount, strict: Boolean): Vec[T] = {
    val width = widthOf(this)
    require(!strict || width % sliceWidth.value == 0,
      s"subdivideIn can't evenly divide $width bit into ${sliceWidth.value} bit slices, as required by strict=true")
    Vec(
      (0 until width by sliceWidth.value)
        .map(i => this.apply(i until ((i + sliceWidth.value) min width)).asInstanceOf[T])
    )
  }


  def subdivideIn(sliceCount: SlicesCount): Vec[T] = subdivideIn(sliceCount, true)
  def subdivideIn(sliceWidth: BitCount): Vec[T] = subdivideIn(sliceWidth, true)


  private def copyAnalogTagTo[T <: Data](that : T) : T = {
    if(this.isAnalog) that.setAsAnalog()
    that
  }
  /** Extract a bit of the BitVector */
  def newExtract(bitId: Int, extract: BitVectorBitAccessFixed): Bool = {
    extract.source = this
    extract.bitId  = bitId

    val bool = wrapWithBool(extract)

    bool.compositeAssign = new Assignable {
      override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind : AnyRef)(implicit loc: Location): Unit = that match {
        case that: Bool         => BitVector.this.compositAssignFrom(that,BitAssignmentFixed(BitVector.this, bitId), kind)
        //        case that: DontCareNode => BitVector.this.assignFrom(that, BitAssignmentFixed(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), conservative = true)
      }
      override def getRealSourceNoRec: BaseType = BitVector.this
    }
    copyAnalogTagTo(bool)
  }

  /** Extract a bit of the BitVector */
  def newExtract(bitId: UInt, extract: BitVectorBitAccessFloating): Bool = {
    extract.source = this
    extract.bitId = bitId
    val bool =  wrapWithBool(extract)
    bool.compositeAssign = new Assignable  {
      override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind : AnyRef)(implicit loc: Location): Unit = that match {
        case x: Bool         => BitVector.this.compositAssignFrom(that, BitAssignmentFloating(BitVector.this, bitId), kind)
//        case x: DontCareNode => BitVector.this.assignFrom(that,BitAssignmentFloating(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
      }
      override def getRealSourceNoRec: BaseType = BitVector.this
    }
    copyAnalogTagTo(bool)
  }

  /** Extract a range of bits of the BitVector */
  def newExtract(hi: Int, lo: Int, accessFactory: => BitVectorRangedAccessFixed): this.type = {
    copyAnalogTagTo(if (hi - lo + 1 != 0) {
      val access = accessFactory
      access.source = this
      access.hi     = hi
      access.lo     = lo
      access.checkHiLo
      val ret = wrapWithWeakClone(access)
      ret.compositeAssign = new Assignable {
        override def assignFromImpl(that: AnyRef, target : AnyRef, kind : AnyRef)(implicit loc: Location): Unit = target match {
          case x: BitVector                => BitVector.this.compositAssignFrom(that, RangedAssignmentFixed(BitVector.this, hi, lo), kind)
//          case x: DontCareNode             => BitVector.this.assignFrom(that,new RangedAssignmentFixed(BitVector.this, new DontCareNodeFixed(BitVector.this, hi - lo + 1), hi, lo), true)
          case x: BitAssignmentFixed       => BitVector.this.apply(lo + x.bitId).compositAssignFrom(that, BitVector.this, kind)
          case x: BitAssignmentFloating    => BitVector.this.apply(lo + x.bitId.asInstanceOf[UInt]).compositAssignFrom(that, BitVector.this, kind)
          case x: RangedAssignmentFixed    => BitVector.this.apply(lo + x.hi downto lo + x.lo).compositAssignFrom(that,BitVector.this, kind)
          case x: RangedAssignmentFloating => BitVector.this.apply(lo + x.offset.asInstanceOf[UInt], x.bitCount bits).compositAssignFrom(that, BitVector.this, kind)
        }
        override def getRealSourceNoRec: BaseType = BitVector.this
      }
      ret.asInstanceOf[this.type]
    }
    else
      getZeroUnconstrained
    )
  }

  /** Extract a range of bits of the BitVector */
  def newExtract(offset: UInt, size: Int, extract : BitVectorRangedAccessFloating)(implicit loc: Location): this.type = {
    copyAnalogTagTo(if (size != 0) {
      extract.source = this
      extract.size   = size
      extract.offset = offset
      val ret = wrapWithWeakClone(extract)
      ret.compositeAssign = new Assignable {
        override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind : AnyRef)(implicit loc: Location): Unit = target match {
          case x: BitVector                => BitVector.this.compositAssignFrom(that,RangedAssignmentFloating(BitVector.this, offset, size), kind)
//          case x: DontCareNode             => BitVector.this.assignFrom(that,new RangedAssignmentFloating(BitVector.this, new DontCareNodeFixed(BitVector.this, size), offset, size bit), true)
          case x: BitAssignmentFixed       => BitVector.this.apply(offset + x.bitId).compositAssignFrom(that, BitVector.this, kind)
          case x: BitAssignmentFloating    => BitVector.this.apply(offset + x.bitId.asInstanceOf[UInt]).compositAssignFrom(that, BitVector.this, kind)
          case x: RangedAssignmentFixed    => BitVector.this.apply(offset + x.lo, x.hi - x.lo + 1 bits).compositAssignFrom(that, BitVector.this, kind)
          case x: RangedAssignmentFloating => BitVector.this.apply(offset + x.offset.asInstanceOf[UInt], x.bitCount bits).compositAssignFrom(that, BitVector.this, kind)
        }
        override def getRealSourceNoRec: BaseType = BitVector.this
      }
      ret.asInstanceOf[this.type]
    }
    else
      getZeroUnconstrained
    )
  }

  def getZeroUnconstrained: this.type
  def getAllTrue: this.type
  /**
    * Return the bit at index bitId
    * @example{{{ val myBool = myBits(3) }}}
    */
  def apply(bitId: Int): Bool

  /**
    * Return the bit at index bitId
    * @example{{{ val myBool = myBits(myUInt) }}}
    */
  def apply(bitId: UInt): Bool

  /**
    * Return a range of bits at offset and of width bitCount
    * @example{{{ val myBool = myBits(3, 2 bits) }}}
    */
  def apply(offset: Int, bitCount: BitCount): this.type

  /**
    * Return a range of bits at offset and of width bitCount
    * @example{{{ val myBool = myBits(myUInt, 2 bits) }}}
    */
  def apply(offset: UInt, bitCount: BitCount): this.type

  /**
    * Return a range of bits
    * @example{{{ val myBool = myBits(3 downto 1) }}}
    */
  def apply(range: Range): this.type = this.apply(range.low, range.high - range.low + 1 bits)


  override def isRegOnAssign : Boolean = {
    if(isReg) return true
    if(dlcHasOnlyOne){
      dlcHead.source match {
        case e : SubAccess => return e.getBitVector.asInstanceOf[BitVector].isRegOnAssign
        case _ => return false
      }
    }
    return false
  }

  /** Set all bits to value */
  def setAllTo(value: Boolean): this.type = {
    if(value) setAll() else clearAll()
    this
  }

  /** Set all bits to value */
  def setAllTo(value: Bool): this.type = {
    this := Mux(value, getAllTrue, getZero)
    this
  }

  /** Set all bits */
  override def setAll(): this.type
  /** Clear all bits */
  override def clearAll(): this.type = {
    this := this.getZeroUnconstrained
    this
  }

  /** Return the width */
  def getWidthNoInferation: Int = if (inferredWidth != -1 ) inferredWidth else fixedWidth
  def getWidthStringNoInferation: String = if (getWidthNoInferation == -1 ) "?" else getWidthNoInferation.toString

  override private[core] def canSymplifyIt = (inferredWidth != -1 || !isFixedWidth) && super.canSymplifyIt

  override def toString(): String = {
    if(component == null)
      s"${getDisplayName()} : ${dirString()} $getClassIdentifier[$getWidthStringNoInferation bits])"
    else if((isNamed || !hasOnlyOneStatement || !head.source.isInstanceOf[Literal]))
      s"(${component.getPath() + "/" + this.getDisplayName()} : ${dirString()} $getClassIdentifier[$getWidthStringNoInferation bits])"
    else
      head.source.toString
  }

  override def getMuxType[T <: Data](list: TraversableOnce[T]) = {
    val w = list.filter(!_.hasTag(tagAutoResize)).map(e => widthOf(e)).max
    cloneOf(this).setWidth(w).asInstanceOf[T]
  }
}
