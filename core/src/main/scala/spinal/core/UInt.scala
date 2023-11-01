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
  * UInt factory used for instance by the IODirection to create a in/out UInt
  */
trait UIntFactory{
  /** Create a new UInt */
  def UInt(u: Unit = ()): UInt = new UInt()
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
class UInt extends BitVector with Num[UInt] with MinMaxProvider with DataPrimitives[UInt] with BaseTypePrimitives[UInt] with BitwiseOp[UInt]{
  override def tag(q: QFormat): UInt = {
    require(!q.signed, "assign SQ to UInt")
    require(q.width == this.getWidth, s"${q} width mismatch!")
    Qtag = q
    this
  }

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
  override def +^(right: UInt): UInt = this.expand + right.expand
  override def -^(right: UInt): UInt = this.expand - right.expand
  override def +|(right: UInt): UInt = (this +^ right).sat(1)
  override def -|(right: UInt): UInt = (this -^ right)._satAsSInt2UInt()

  /* Implement BitwiseOp operators */
  override def |(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Or)
  override def &(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.And)
  override def ^(right: UInt): UInt = wrapBinaryOperator(right, new Operator.UInt.Xor)
  override def unary_~ : UInt      = wrapUnaryOperator(new Operator.UInt.Not)

  def valueRange: Range = {
    assert(getWidth < 32)
    0 to (1 << getWidth)-1
  }

  /* Implement fixPoint operators */
  /**highest m bits Saturation */
  override def sat(m: Int): UInt = {
    require(getWidth > m, s"Saturation bit width $m must be less than data bit width $getWidth")
    m match {
      case 0           => this << 0
      case x if x > 0  => this._sat(m)
      case _           => (Bits(-m bits).clearAll ## this).asUInt
    }
  }
  private def _sat(m: Int): UInt = {
    val ret = UInt(getWidth-m bit)
    when(this(getWidth-1 downto getWidth-m).asBits.orR){
      ret.setAll()
    }.otherwise{
      ret := this(getWidth-m-1 downto 0)
    }
    ret
  }
  private def _satAsSInt2UInt(): UInt = {
    val ret = UInt(getWidth-1 bit)
    when(this.msb){
      ret.clearAll()
    }.otherwise{
      ret := this(getWidth-2 downto 0)
    }
    ret
  }
  /**highest m bits Discard */
  def trim(m: Int): UInt = this(getWidth-m-1 downto 0)

  /**Round Api*/

  /** UInt ceil
    * floor(x)
    * return w(this)-n bits
    * */
  override def floor(n: Int): UInt = {
    require(getWidth > n, s"floor bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => this._floor(n)
      case _          => this << -n
    }
  }
  private def _floor(n: Int): UInt = this >> n

  /** UInt ceil
    * ceil(x)
    * return if(align) w(this)-n bits else w(this)-n+1 bits
    * */
  override def ceil(n: Int, align: Boolean = true): UInt = {
    require(getWidth > n, s"ceil bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _ceil(n).sat(1) else _ceil(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n + 1 bit*/
  private[core] def _ceil(n: Int): UInt = {
    val ret = UInt(getWidth-n+1 bits)
    when(this(n-1 downto 0).asBits.orR){
      ret := this(getWidth-1 downto n) +^ 1
    }.otherwise {
      ret := this (getWidth-1 downto n).resized
    }
    ret
  }

  override def floorToZero(n: Int): UInt = floor(n)
  override def ceilToInf(n: Int, align: Boolean = true): UInt   = ceil(n, align)
  override def roundToZero(n: Int, align: Boolean = true): UInt = roundDown(n, align)
  override def roundToInf(n: Int, align: Boolean = true): UInt  = roundUp(n, align)
  /**
    * UInt roundUp
    * floor(x + 0.5)
    * return if(align) w(this)-n bits else w(this)-n+1 bits
    */
  override def roundUp(n: Int, align: Boolean = true): UInt = {
    require(getWidth > n, s"RoundUp bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundUp(n).sat(1) else _roundUp(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n + 1 bit*/
  private def _roundUp(n: Int): UInt = {
    val ret = UInt(getWidth-n+1 bits)
    val positive0p5: UInt = (Bits(getWidth-n bits).clearAll ## True).asUInt //0.5
    ret := (this(getWidth-1 downto n-1) +^ positive0p5).floor(1) //(x + 0.5).floor
    ret
  }

  /** UInt roundDown
    * ceil(x - 0.5)
    * return w(this)-n bits
    * */
  override def roundDown(n: Int, align: Boolean): UInt = {
    require(getWidth > n, s"RoundDown bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this << 0
      case x if x > 0 => if(align) _roundDown(n).sat(1) else _roundDown(n)
      case _ => this << -n
    }
  }
  /**return w(this)-n bit*/
  private def _roundDown(n: Int): UInt = {
    require( n > 0, s"RoundDown bit width $n must be less than data bit width $getWidth")
    val ret = UInt(getWidth-n+1 bits)
    when(this(n-1) && this(n-2 downto 0).asBits.orR){
      ret := this(getWidth-1 downto n) +^ 1
    }.otherwise {
      ret := this(getWidth-1 downto n).expand
    }
    ret
  }

  override def roundToEven(n: Int, align: Boolean): UInt = {
    require(getWidth > n, s"RoundToEven bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToEven(n).sat(1) else _roundToEven(n)
      case _          => this << -n
    }
  }

  private def _roundToEven(n: Int): UInt = {
    val ret = UInt(getWidth-n+1 bits)
    when (!this(n)) {
      ret := _roundDown(n)
    } otherwise {
      ret := _roundUp(n)
    }
    ret
  }

  override def roundToOdd(n: Int, align: Boolean): UInt = {
    require(getWidth > n, s"RoundToOdd bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToOdd(n).sat(1) else _roundToOdd(n)
      case _          => this << -n
    }
  }

  private def _roundToOdd(n: Int): UInt = {
    val ret = UInt(getWidth-n+1 bits)
    when (!this(n)) {
      ret := _roundUp(n)
    } otherwise {
      ret := _roundDown(n)
    }
    ret
  }

  //SpinalHDL chose roundToInf as default round
  override def round(n: Int, align: Boolean = true): UInt = roundToInf(n, align)

  protected def _fixEntry(roundN: Int, roundType: RoundType, satN: Int): UInt ={
    roundType match{
      case RoundType.CEIL          => this.ceil(roundN, false).sat(satN + 1)
      case RoundType.FLOOR         => this.floor(roundN).sat(satN)
      case RoundType.FLOORTOZERO   => this.floorToZero(roundN).sat(satN)
      case RoundType.CEILTOINF     => this.ceilToInf(roundN, false).sat(satN + 1)
      case RoundType.ROUNDUP       => this.roundUp(roundN, false).sat(satN + 1)
      case RoundType.ROUNDDOWN     => this.roundDown(roundN,false).sat(satN + 1)
      case RoundType.ROUNDTOZERO   => this.roundToZero(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOINF    => this.roundToInf(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOEVEN   => this.roundToEven(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOODD    => this.roundToOdd(roundN, false).sat(satN + 1)
    }
  }

  /**Factory fixTo Function*/
  private def fixToWrap(section: Range.Inclusive, roundType: RoundType): UInt = {
    val w: Int = this.getWidth
    val wl: Int = w - 1
    (section.min, section.max, section.size) match {
      case (0,  _,   `w`) => this << 0
      case (x, `wl`,  _ ) => _fixEntry(x, roundType, satN = 0)
      case (0,  y,    _ ) => this.sat(this.getWidth -1 - y)
      case (x,  y,    _ ) => _fixEntry(x, roundType, satN = this.getWidth -1 - y)
    }
  }

  def fixTo(section: Range.Inclusive, roundType: RoundType): UInt = {

    class fixTo(width: Int, section: Range.Inclusive,
                roundType: RoundType) extends Component{
      val wrapName = s"UInt${width}fixTo${section.max}_${section.min}_${roundType}"

      val din = in UInt(width bits)
      val dout = out UInt(section.size bits)
      dout := din.fixToWrap(section, roundType)
    }

    if(GlobalData.get.config.fixToWithWrap) {
      val dut = new fixTo(this.getWidth, section, roundType)
      dut.din := this
      dut.dout
    } else {
      fixToWrap(section, roundType)
    }
  }

  def fixTo(section: Range.Inclusive): UInt = fixTo(section, getFixRound())

  def fixTo(q: QFormat, roundType: RoundType): UInt = {
    val section = getfixSection(q)
    fixTo(section, roundType)
  }

  def fixTo(q: QFormat): UInt = fixTo(q, getFixRound())

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
  def twoComplement(enable: Bool, plusOneEnable : Bool = null): SInt = ((enable ## Mux(enable, ~this, this)).asUInt + U(if(plusOneEnable == null) enable else enable && plusOneEnable)).asSInt
//  def twoComplementUInt(enable: Bool): UInt = ((Mux(enable, ~this, this)) + enable.asUInt)

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

  def :=(value : String) : Unit = this := U(value)

  override def assignFromBits(bits: Bits): Unit = this := bits.asUInt
  override def assignFromBits(bits: Bits, hi : Int, lo : Int): Unit = this(hi downto lo).assignFromBits(bits)

  /**
    * Cast an UInt to a SInt
    * @example {{{ mySInt := myUInt.asSInt }}}
    * @return a SInt data
    */
  def asSInt: SInt = wrapCast(SInt(), new CastUIntToSInt)
  /*UInt toSInt add 1 bit 0 at hsb for sign bit*/
  def intoSInt: SInt = this.expand.asSInt
  def expand: UInt = (False ## this.asBits).asUInt

  override def asBits: Bits = wrapCast(Bits(), new CastUIntToBits)

  private[core] override def isEqualTo(that: Any): Bool = that match {
    case that: UInt           => wrapLogicalOperator(that,new Operator.UInt.Equal)
    case that: MaskedLiteral  => that === this
    case that: Int            => this === that
    case that: BigInt         => this === that
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  private[core] override def isNotEqualTo(that: Any): Bool = that match {
    case that: UInt           => wrapLogicalOperator(that,new Operator.UInt.NotEqual)
    case that: MaskedLiteral  => that === this
    case _                    => SpinalError(s"Don't know how to compare $this with $that"); null
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerUInt
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerUInt

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node   = new ResizeUInt
    node.input = this
    node.size  = width
    node
  })

  override def resize(width: BitCount) : this.type = resize(width.value)

  override def minValue: BigInt = BigInt(0)
  override def maxValue: BigInt = (BigInt(1) << getWidth) - 1

  /**
    * Assign a mask to the output signal
    * @example {{{ output4 assignMask M"1111 }}}
    * @param maskedLiteral masked literal value
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

  def reversed = U(B(this.asBools.reverse)).asInstanceOf[this.type]

  def wrap = new {
    private def checkBits(that: UInt) = {
      assert(that.getBitsWidth == _data.getBitsWidth, "wrap only works on UInt with same width.")
    }
    def <(that: UInt): Bool = { checkBits(that); (_data - that).msb }
    def >=(that: UInt): Bool = { checkBits(that); !(<(that)) }
    def <=(that: UInt): Bool = { checkBits(that); val result = _data - that; result === 0 || result.msb }
    def >(that: UInt): Bool = { checkBits(that); !(<=(that)) }
  }


  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = this.assignFrom(new Operator.Formal.RandomExpUInt(kind, widthOf(this)))
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

