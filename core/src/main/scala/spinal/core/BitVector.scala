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
abstract class BitVector extends BaseType with Widthable /*with CheckWidth*/ {
//
//  /** Prefix that define the corresponding class (Currently not used) */
//  private[core] def prefix: String
//
  /** Width of the BitVector (-1 = undefined) */
  private[core] var fixedWidth = -1
//
//  /** Used to know the data type of the children class of BitVector */
//  type T <: BitVector
//
//  /** Return the upper bound */
//  def high: Int = getWidth - 1
//  /** Return the most significant bit */
//  def msb: Bool = this(high)
//  /** Return the least significant bit */
//  def lsb: Bool = this(0)
//  /** Return the range */
//  def range: Range = 0 until getWidth
//
//  /** Logical OR of all bits */
//  def orR: Bool = this.asBits =/= 0
//  /** Logical AND of all bits */
//  def andR: Bool = this.asBits === ((BigInt(1) << getWidth) - 1)
//  /** Logical XOR of all bits */
//  def xorR: Bool = this.asBools.reduce(_ ^ _)
//
//  /**
//    * Compare a BitVector with a MaskedLiteral (M"110--0")
//    * @example {{{ val myBool = myBits === M"0-1" }}}
//    * @param that the maskedLiteral
//    * @return a Bool data containing the result of the comparison
//    */
//  def ===(that: MaskedLiteral): Bool = this.isEquals(that)
//  /** BitVector is not equal to MaskedLiteral */
//  def =/=(that: MaskedLiteral): Bool = this.isNotEquals(that)
//
//
//  /** Left rotation of that Bits */
//  def rotateLeft(that: UInt): T = {
//    val thatWidth = widthOf(that)
//    val thisWidth = widthOf(this)
//    require(thatWidth <= log2Up(thisWidth))
//    var result = cloneOf(this).asInstanceOf[T]
//    result := this.asInstanceOf[T]
//    for(i <- that.range){
//      result \= (that(i) ? result.rotateLeft(1 << i).asInstanceOf[T] | result)
//    }
//    result
//  }
//
//  /** Right rotation of that Bits */
//  def rotateRight(that: UInt): T = {
//    val thatWidth = widthOf(that)
//    val thisWidth = widthOf(this)
//    require(thatWidth <= log2Up(thisWidth))
//    var result = cloneOf(this).asInstanceOf[T]
//    result := this.asInstanceOf[T]
//    for(i <- that.range){
//      result \= (that(i) ? result.rotateRight(1<<i).asInstanceOf[T] | result)
//    }
//    result
//  }
//
//  /** Left rotation of that bits */
//  def rotateLeft(that: Int): T
//  /** Right rotation of that bits */
//  def rotateRight(that: Int): T
//
//
//  /** Return true if the BitVector has a fixed width */
  private[core] def isFixedWidth = fixedWidth != -1

  /** Unfix the width of the BitVector */
  private[core] def unfixWidth() = {
    fixedWidth = -1
    widthWhenNotInferred = -1
    inferredWidth = -1
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

//  override def getBitsWidth: Int = getWidth

  override def clone: this.type = {
    val res = super.clone
    res.fixedWidth = this.fixedWidth
    res
  }
//
//  private[core] override def normalizeInputs: Unit = InputNormalize.resizedOrUnfixedLit(this, 0, this.getWidth)
//
//  /**
//    * Resize the bitVector to width
//    * @example{{{ val res = myBits.resize(10) }}}
//    * @return a resized bitVector
//    */
//  def resize(width: Int): this.type

  private[core] override def calcWidth: Int = {
    if (isFixedWidth) return fixedWidth
    var w = -1
    foreachStatements(_ match{
      case s : AssignementStatement =>{
        s.source match {
          case e : WidthProvider => w = Math.max(w, e.getWidth)
        }
      }
    })
    w
  }



//  override private[core] def assignFromImpl(that: AnyRef): Unit = {
//    that match {
//      case that : BaseType =>
//        component.addStatement(new AssignementStatement(target = RefExpression(this), source = new RefExpression(that), AssignementKind.DATA))
//      case that : Expression =>
//        component.addStatement(new AssignementStatement(target = RefExpression(this), source = that, AssignementKind.DATA))
//      case _ =>
//        throw new Exception(s"Undefined assignment $this := $that")
//    }
//  }
//
//
//  override private[core] def initFromImpl(that: AnyRef): Unit = {
//    if(!isReg)
//      LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
//    else that match {
//      case that : BaseType =>
//        component.addStatement(new AssignementStatement(target = RefExpression(this), source = new RefExpression(that), AssignementKind.INIT))
//      case that : Expression =>
//        component.addStatement(new AssignementStatement(target = RefExpression(this), source = that, AssignementKind.INIT))
//      case _ =>
//        throw new Exception("Undefined assignment")
//    }
//  }

//
//  /**
//    * Cast the BitVector into a Vector of Bool
//    * @return a vector of Bool
//    */
//  def asBools: Vec[Bool] = {
//    val vec = ArrayBuffer[Bool]()
//    val bitCount = getWidth
//    if (bitCount == -1) SpinalError("Can't convert to bools a Bit that has unspecified width value")
//    for (i <- 0 until bitCount) vec += this (i)
//    Vec(vec)
//  }
//
//  /**
//    * Split the BitVector into x slice
//    * @example {{{ val res = myBits.subdiviedIn(3 slices) }}}
//    * @param sliceCount the width of the slice
//    * @return a Vector of slices
//    */
//  def subdivideIn(sliceCount: SlicesCount): Vec[T] = {
//    require(this.getWidth % sliceCount.value == 0)
//    val sliceWidth = widthOf(this) / sliceCount.value
//    Vec((0 until sliceCount.value).map(i => this(i * sliceWidth, sliceWidth bits).asInstanceOf[T]))
//  }
//
//  /**
//    * Split the BitVector into slice of x bits
//    * * @example {{{ val res = myBits.subdiviedIn(3 bits) }}}
//    * @param sliceWidth the width of the slice
//    * @return a Vector of slices
//    */
//  def subdivideIn(sliceWidth: BitCount): Vec[T] = {
//    require(this.getWidth % sliceWidth.value == 0)
//    subdivideIn(this.getWidth / sliceWidth.value slices)
//  }
//
//  /** Extract a bit of the BitVector */
//  def newExtract(bitId: Int, extract: ExtractBoolFixed): Bool = {
//    extract.input = this
//    extract.bitId = bitId
//    val bool = new Bool
//    bool.input = extract
//
//    bool.compositeAssign = new Assignable {
//      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
//        case that: Bool         => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, that, bitId), conservative = true)
//        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFixed(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), conservative = true)
//      }
//    }
//    bool
//  }
//
//  /** Extract a bit of the BitVector */
//  def newExtract(bitId: UInt, extract: ExtractBoolFloating): Bool = {
//    extract.input = this
//    extract.bitId = bitId
//    val bool = new Bool
//    bool.input = extract
//
//    bool.compositeAssign = new Assignable {
//      override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
//        case that: Bool         => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, that, bitId), true)
//        case that: DontCareNode => BitVector.this.assignFrom(new BitAssignmentFloating(BitVector.this, new DontCareNodeFixed(Bool(), 1), bitId), true)
//      }
//    }
//    bool
//  }
//
//  /** Extract a range of bits of the BitVector */
//  def newExtract(hi: Int, lo: Int, extract: ExtractBitsVectorFixed): this.type = {
//    if (hi - lo + 1 != 0) {
//      extract.input = this
//      extract.hi = hi
//      extract.lo = lo
//      extract.checkHiLo
//      val ret = wrapWithWeakClone(extract)
//      ret.compositeAssign = new Assignable {
//        override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
//          case that: BitVector                => BitVector.this.assignFrom(new RangedAssignmentFixed(BitVector.this, that, hi, lo), true)
//          case that: DontCareNode             => BitVector.this.assignFrom(new RangedAssignmentFixed(BitVector.this, new DontCareNodeFixed(BitVector.this, hi - lo + 1), hi, lo), true)
//          case that: BitAssignmentFixed       => BitVector.this.apply(lo + that.getBitId).assignFrom(that.getInput, true)
//          case that: BitAssignmentFloating    => BitVector.this.apply(lo + that.getBitId.asInstanceOf[UInt]).assignFrom(that.getInput, true)
//          case that: RangedAssignmentFixed    => BitVector.this.apply(lo + that.getHi, lo + that.getLo).assignFrom(that.getInput, true)
//          case that: RangedAssignmentFloating => BitVector.this.apply(lo + that.getOffset.asInstanceOf[UInt], that.getBitCount).assignFrom(that.getInput, true)
//        }
//      }
//      ret
//    }
//    else
//      getZeroUnconstrained()
//  }
//
//  /** Extract a range of bits of the BitVector */
//  def newExtract(offset: UInt, size: Int, extract : ExtractBitsVectorFloating): this.type = {
//    if (size != 0) {
//      extract.input = this
//      extract.size = size
//      extract.offset = offset
//      offset.dontSimplifyIt()
//      val ret = wrapWithWeakClone(extract)
//      ret.compositeAssign = new Assignable {
//        override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = that match {
//          case that: BitVector                => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, that, offset, size bit), true)
//          case that: DontCareNode             => BitVector.this.assignFrom(new RangedAssignmentFloating(BitVector.this, new DontCareNodeFixed(BitVector.this, size), offset, size bit), true)
//          case that: BitAssignmentFixed       => BitVector.this.apply(offset + that.getBitId).assignFrom(that.getInput, true)
//          case that: BitAssignmentFloating    => BitVector.this.apply(offset + that.getBitId.asInstanceOf[UInt]).assignFrom(that.getInput, true)
//          case that: RangedAssignmentFixed    => BitVector.this.apply(offset + that.getLo, that.getHi - that.getLo + 1 bit).assignFrom(that.getInput, true)
//          case that: RangedAssignmentFloating => BitVector.this.apply(offset + that.getOffset.asInstanceOf[UInt], that.getBitCount).assignFrom(that.getInput, true)
//        }
//      }
//      ret
//    }
//    else
//      getZeroUnconstrained()
//  }
//
//  def getZeroUnconstrained() : this.type
//
//  /**
//    * Return the bit at index bitId
//    * @example{{{ val myBool = myBits(3) }}}
//    */
//  def apply(bitId: Int) : Bool
//
//  /**
//    * Return the bit at index bitId
//    * @example{{{ val myBool = myBits(myUInt) }}}
//    */
//  def apply(bitId: UInt): Bool
//
//  /**
//    * Return a range of bits at offset and of width bitCount
//    * @example{{{ val myBool = myBits(3, 2 bits) }}}
//    */
//  def apply(offset: Int, bitCount: BitCount): this.type
//
//  /**
//    * Return a range of bits at offset and of width bitCount
//    * @example{{{ val myBool = myBits(myUInt, 2 bits) }}}
//    */
//  def apply(offset: UInt, bitCount: BitCount): this.type
//
//  /**
//    * Return a range of bits form hi index to lo index
//    * @example{{{ val myBool = myBits(3, 1) }}}
//    */
//  def apply(hi: Int, lo: Int): this.type = this.apply(lo, hi - lo + 1 bit)
//
//  /**
//    * Return a range of bits
//    * @example{{{ val myBool = myBits(3 downto 1) }}}
//    */
//  def apply(range: Range): this.type = this.apply(range.high, range.low)
//
//  /** Set all bits to value */
//  def setAllTo(value: Boolean): Unit = {
//    val litBt = weakClone
//    litBt.input = new BitsAllToLiteral(this, value)
//    this := litBt
//  }
//
//  /** Set all bits to value */
//  def setAllTo(value: Bool): Unit = {
//    val litBt = weakClone
//    val node = getAllToBoolNode()
//    node.input = value.asInstanceOf[node.T]
//    litBt.input = node
//    this := litBt
//  }
//
//  /** Set all bits */
//  def setAll() = setAllTo(true)
//  /** Clear all bits */
//  def clearAll() = setAllTo(false)
//
//  protected def getAllToBoolNode(): Operator.BitVector.AllByBool
//
//  private[core] override def wrapWithWeakClone(node: Node): this.type = {
//    val typeNode = super.wrapWithWeakClone(node)
//    typeNode
//  }
//
//  /** Return the width */
//  def getWidthNoInferation: Int = if (inferredWidth != -1 ) inferredWidth else fixedWidth
//
//  def getWidthStringNoInferation: String = if (getWidthNoInferation == -1 ) "?" else getWidthNoInferation.toString
//
//  private[core] override def checkInferedWidth: Unit = {
//    val input = this.input
//    if (input != null && input.component != null && this.getWidth != input.asInstanceOf[WidthProvider].getWidth) {
//      PendingError(s"Assignment bit count mismatch. ${this} := $input at \n${ScalaLocated.long(getAssignementContext(0))}")
//    }
//  }
//
//  override def assignDontCare(): this.type = {
//    this.assignFrom(new DontCareNodeInfered(this), false)
//    this
//  }
//
//  override def toString(): String = s"(${component.getPath() + "/" + this.getDisplayName()} : $getClassIdentifier[$getWidthStringNoInferation bits])"
}