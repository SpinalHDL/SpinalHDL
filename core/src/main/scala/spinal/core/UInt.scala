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
  * UInt factory used for instance by the IODirection to create a in/out UInt
  */
trait UIntFactory{
  /** Create a new UInt */
  def UInt() = new UInt()
  /** Create a new UInt of a given width */
  def UInt(width: BitCount): UInt = UInt().setWidth(width.value)
}


/**
  * The UInt type corresponds to a vector of bits that can be used for unsigned integer arithmetic.
  *
  * @example {{{
  *    val myUInt = UInt(8 bits)
  *     myUInt := U(2,8 bits)
  *     myUInt := U(2)
  *     myUInt := U"0000_0101"
  *     myUInt := U"h1A"
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Int UInt Documentation]]
  */
class UInt extends BitVector with Num[UInt] with MinMaxProvider with DataPrimitives[UInt] with BitwiseOp[UInt]{

  override def getTypeObject = TypeUInt

  override type T = UInt

  override def _data: UInt = this

  /**
    * Concatenation between two UInt
    * @example{{{ val myUInt = uInt1 @@ uInt2 }}}
    * @param that an UInt to append
    * @return a new UInt of width (w(this) + w(right))
    */
  def @@(that: UInt): UInt = U(this ## that)
  /** Concatenation between a UInt and a Bool */
  def @@(that: Bool): UInt = U(this ## that)

  override private[core] def resizeFactory: Resize = new ResizeUInt

  override def opName: String = "UInt"

  /* Implement Num operators */
  override def + (right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Add)
  override def - (right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Sub)
  override def * (right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Mul)
  override def / (right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Div)
  override def % (right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Mod)
  override def < (right: UInt): Bool = wrapLogicalOperator(right, new Operator.UInt.Smaller)
  override def > (right: UInt): Bool = right < this
  override def <=(right: UInt): Bool = wrapLogicalOperator(right, new Operator.UInt.SmallerOrEqual)
  override def >=(right: UInt): Bool = right <= this
  override def >>(that: Int): UInt   = wrapConstantOperator(new Operator.UInt.ShiftRightByInt(that))
  override def <<(that: Int): UInt   = wrapConstantOperator(new Operator.UInt.ShiftLeftByInt(that))
  override def +^(right: UInt): UInt = this.expand + right
  override def -^(right: UInt): UInt = this.expand - right
  override def +|(right: UInt): UInt = (this +^ right).sat(1)
  override def -|(right: UInt): UInt = (this -^ right).sat(1)

  /* Implement BitwiseOp operators */
  override def |(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Or)
  override def &(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.And)
  override def ^(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Xor)
  override def unary_~ : UInt      = wrapUnaryOperator(new Operator.UInt.Not)

  /* Implement fixPoint operators */
  /**Saturation highest m bits*/
  override def sat(m: Int): UInt = {
    require(getWidth > m, s"Saturation bit width $m must be less than data bit width $getWidth")
    m match {
      case 0 => this
      case x if x > 0  => {
        val ret = UInt(getWidth-m bit)
        when(this(getWidth-1 downto getWidth-m).asBits.orR){
          ret.setAll()
        }.otherwise{
          ret := this(getWidth-m-1 downto 0)
        }
        ret
      }
      case _ => (B(0, -m bits) ## this).asUInt
    }
  }

  override def floor(n: Int): UInt = this >> n
  /**
    * UInt round
    * @example{{{ val result = round(n}}}
    * @return a Bits of width : w(this)-n+1 (expand 1bit)
    */
  override def round(n: Int): UInt = {
    require(getWidth > n, s"Round bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this
      case x if x > 0 => {
        val ret = UInt(getWidth-n+1 bits)
        val postive0p5: UInt = (Bits(getWidth-n+1 bits).clearAll ## Bits(1 bits).setAll).asUInt //0.5
        ret := (this(getWidth-1 downto n-1) +^ postive0p5).floor(1) //(x + 0.5).floor
        ret
      }
      case _ => this << -n
    }
  }

  override def ceil(n: Int): UInt = {
    require(getWidth > n, s"Round bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this
      case x if x > 0 => {
        val ret = UInt(getWidth-n+1 bits)
        when(this(n-1 downto 0).asBits.orR){
          ret := this(getWidth-1 downto n) +^ 1
        }.otherwise {
          ret := this (getWidth-1 downto n).resized
        }
        ret
      }
      case _ => this << -n
    }
  }

  /**roundNoExpand return w(this)-n bits Width UInt with safe saturation*/
  def roundNoExpand(n: Int): UInt =  if(n>0) round(n).sat(1) else round(n)
  def ceilNoExpand(n: Int): UInt = if(n>0) ceil(n).sat(1) else ceil(n)
  def trim(m: Int): UInt = this(getWidth-m-1 downto 0)

  /**fixpoint Api*/
  //TODO: add default fix mode in spinal global config
  def fixTo(section: Range.Inclusive): UInt = {
    this.fixWithRound(section)
  }
//  private val _w: Int = this.getWidth
//  private val _wl: Int = _w - 1
  def fixWithFloor(section: Range.Inclusive): UInt = {
    require(math.abs(section.step) == 1, "section step must 1/-1 !")
    val _w: Int = this.getWidth
    val _wl: Int = _w - 1
    (section.min, section.max, section.size) match {
      case (0,  _,   `_w`) => this
      case (x, `_wl`, _  ) => this.floor(x)
      case (0,  y,    _  ) => this.sat(this.getWidth -1 - y)
      case (x,  y,    _  ) => this.floor(x).sat(this.getWidth -1 - y)
    }
  }

  def fixWithRound(section: Range.Inclusive): UInt = {
    require(math.abs(section.step) == 1, "section step must 1/-1 !")
    val _w: Int = this.getWidth
    val _wl: Int = _w - 1
    (section.min, section.max, section.size) match {
      case (0,  _,   `_w`) => this
      case (x, `_wl`, _  ) => if(x >0) this.round(x).sat(1)
                              else this.round(x)
      case (0,  y,    _  ) => this.sat(this.getWidth -1 - y)
      case (x,  y,    _  ) => if(x >0) this.round(x).sat(this.getWidth - 1 - y + 1)
                              else     this.round(x).sat(this.getWidth - 1 - y)
    }
  }
  /**
    * Logical shift Right (output width = input width)
    * @example{{{ val result = myUInt >> myUIntShift }}}
    * @param that the number of shift
    * @return a Bits of width : w(this)
    */
  def >>(that: UInt): UInt = wrapBinaryOperator(that, new Operator.UInt.ShiftRightByUInt)
  /** Logical shift Left (output width will increase of w(this) + max(that) bits */
  def <<(that: UInt): UInt = wrapBinaryOperator(that, new Operator.UInt.ShiftLeftByUInt)

  /**
    * Logical shift right (output width = input width)
    * @example{{{ val result = myUInt |>> 4 }}}
    * @param that the number of shift
    * @return a Bits of width : w(this)
    */
  def |>>(that: Int): UInt  = wrapConstantOperator(new Operator.UInt.ShiftRightByIntFixedWidth(that))
  /** Logical shift left (output width == input width) */
  def |<<(that: Int): UInt  = wrapConstantOperator(new Operator.UInt.ShiftLeftByIntFixedWidth(that))
  /** Logical shift Right (output width == input width) */
  def |>>(that: UInt): UInt = this >> that
  /** Logical shift left (output width == input width) */
  def |<<(that: UInt): UInt = wrapBinaryOperator(that, new Operator.UInt.ShiftLeftByUIntFixedWidth)

  override def rotateLeft(that: Int): UInt = {
    val width   = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) @@ this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): UInt = {
    val width   = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) @@ this(this.high downto thatMod)
  }

  /**
    * 2'Complement
    * @param enable enable the 2'complement
    * @return Return the 2'Complement of the number
    */
  def twoComplement(enable: Bool): SInt = ((False ## Mux(enable, ~this, this)).asUInt + enable.asUInt).asSInt

  /**
    * Assign a range value to an UInt
    * @example{{{ core.io.interrupt = (0 -> uartCtrl.io.interrupt, 1 -> timerCtrl.io.interrupt, default -> false)}}}
    * @param rangesValue The first range value
    * @param _rangesValues Others range values
    */
  def :=(rangesValue: (Any, Any), _rangesValues: (Any, Any)*): Unit = {
    val rangesValues = rangesValue +: _rangesValues
    U.applyTuples(this, rangesValues)
  }

  override def assignFromBits(bits: Bits): Unit = this := bits.asUInt
  override def assignFromBits(bits: Bits, hi : Int, lo : Int): Unit = this(hi downto lo).assignFromBits(bits)

  /**
    * Cast an UInt to a SInt
    * @example {{{ mySInt := myUInt.asSInt }}}
    * @return a SInt data
    */
  def asSInt: SInt = wrapCast(SInt(), new CastUIntToSInt)
  /*UInt toSInt add 1 bit 0 at hsb for sign bit*/
  def toSInt: SInt = this.expand.asSInt
  def expand: UInt = (False ## this.asBits).asUInt

  override def asBits: Bits = wrapCast(Bits(), new CastUIntToBits)

  private[core] override def isEquals(that: Any): Bool = that match {
    case that: UInt           => wrapLogicalOperator(that,new Operator.UInt.Equal)
    case that: MaskedLiteral  => that === this
    case that: Int            => this === that
    case that: BigInt         => this === that
    case _                    => SpinalError(s"Don't know how compare $this with $that"); null
  }

  private[core] override def isNotEquals(that: Any): Bool = that match {
    case that: UInt           => wrapLogicalOperator(that,new Operator.UInt.NotEqual)
    case that: MaskedLiteral  => that === this
    case _                    => SpinalError(s"Don't know how compare $this with $that"); null
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerUInt
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerUInt

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node   = new ResizeUInt
    node.input = this
    node.size  = width
    node
  })

  override def resize(width: BitCount) = resize(width.value)

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1

  /**
    * Assign a mask to the output signal
    * @example {{{ output4 assignMask M"1111 }}}
    * @param maskedLiteral masked litteral value
    */
  def assignMask(maskedLiteral: MaskedLiteral): Unit = {
    assert(maskedLiteral.width == this.getWidth)
    val (literal, careAbout) = (maskedLiteral.value, maskedLiteral.careAbout)
    var offset = 0
    var value = careAbout.testBit(0)
    while(offset != maskedLiteral.width){
      var bitCount = 0
      while(offset + bitCount != maskedLiteral.width && careAbout.testBit(offset + bitCount) == value){
        bitCount += 1
      }
      if(value){
        this(offset, bitCount bit) := U((literal >> offset) & ((BigInt(1) << bitCount) - 1))
      }else{
        this(offset, bitCount bit).assignDontCare()
      }
      value = !value
      offset += bitCount
    }
  }

  override def apply(bitId: Int): Bool  = newExtract(bitId, new UIntBitAccessFixed)
  override def apply(bitId: UInt): Bool = newExtract(bitId, new UIntBitAccessFloating)
  override def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset+bitCount.value-1, offset, new UIntRangedAccessFixed).setWidth(bitCount.value)
  override def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset, bitCount.value, new UIntRangedAccessFloating).setWidth(bitCount.value)

  private[core] override def weakClone: this.type = new UInt().asInstanceOf[this.type]

  override def getZero: this.type = U(0, this.getWidth bits).asInstanceOf[this.type]

  override def getZeroUnconstrained: this.type = U(0).asInstanceOf[this.type]
  override def getAllTrue: this.type = U(maxValue, this.getWidth bits).asInstanceOf[this.type]
  override def setAll(): this.type = {
    this := maxValue
    this
  }

  override def assignDontCare(): this.type = {
    this.assignFrom(UIntLiteral(BigInt(0), (BigInt(1) << this.getWidth) - 1, widthOf(this)))
    this
  }

  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastUInt(delay))
}


/**
  * Define an UInt 2D point
  * @example{{{ val positionOnScreen = Reg(UInt2D(log2Up(p.screenResX) bits, log2Up(p.screenResY) bits)) }}}
  * @param xBitCount width of the x point
  * @param yBitCount width of the y point
  */
case class UInt2D(xBitCount: BitCount, yBitCount: BitCount) extends Bundle {
  val x = UInt(xBitCount)
  val y = UInt(yBitCount)
}


object UInt2D{

  /**
    * Construct a UInt2D with x and y of the same width
    * @param commonBitCount the width of the x and y
    * @return an UInt2 with x and y of the same width
    */
  def apply(commonBitCount: BitCount) : UInt2D = UInt2D(commonBitCount, commonBitCount)
}

