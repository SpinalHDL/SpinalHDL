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
  * SInt factory used for instance by the IODirection to create a in/out SInt
  */
trait SIntFactory{
  /** Create a new SInt */
  def SInt(u: Unit = ()) = new SInt()
  /** Create a new SInt of a given width */
  def SInt(width: BitCount): SInt = SInt().setWidth(width.value)
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
class SInt extends BitVector with Num[SInt] with MinMaxProvider with DataPrimitives[SInt] with BaseTypePrimitives[SInt]  with BitwiseOp[SInt] {
  override def tag(q: QFormat): SInt = {
    require(q.signed, "assign UQ to SInt")
    require(q.width == this.getWidth, s"${q} width mismatch!")
    Qtag = q
    this
  }

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

  /* Implement Num operators */
  override def + (right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Add)
  override def - (right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Sub)
  override def * (right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Mul)
  override def / (right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Div)
  override def % (right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Mod)
  override def < (right: SInt): Bool = wrapLogicalOperator(right, new Operator.SInt.Smaller)
  override def > (right: SInt): Bool = right < this
  override def <=(right: SInt): Bool = wrapLogicalOperator(right, new Operator.SInt.SmallerOrEqual)
  override def >=(right: SInt): Bool = right <= this
  override def >>(that: Int): SInt   = wrapConstantOperator(new Operator.SInt.ShiftRightByInt(that))
  override def <<(that: Int): SInt   = wrapConstantOperator(new Operator.SInt.ShiftLeftByInt(that))
  override def +^(right: SInt): SInt = this.expand + right.expand
  override def -^(right: SInt): SInt = this.expand - right.expand
  override def +|(right: SInt): SInt = (this +^ right).sat(1)
  override def -|(right: SInt): SInt = (this -^ right).sat(1)

  /* Implement BitwiseOp operators */
  override def |(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Or)
  override def &(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.And)
  override def ^(right: SInt): SInt = wrapBinaryOperator(right, new Operator.SInt.Xor)
  override def unary_~ : SInt      = wrapUnaryOperator(new Operator.SInt.Not)

  def valueRange: Range = {
    assert(getWidth < 33)
    (-(1l << getWidth-1) toInt) to (1 << getWidth-1)-1
  }

  /* Implement fixPoint operators */
  def sign: Bool = this.msb
  /**
    * SInt symmetric
    * @example{{{ val symmetrySInt = mySInt.symmetry }}}
    * @return return a SInt which minValue equal -maxValue
    */
  def symmetry: SInt = {
    val ret = cloneOf(this)
    ret := Mux(this === minValue, S(-maxValue), this)
    ret
  }

  /**Saturation highest m bits*/
  override def sat(m: Int): SInt = {
    require(getWidth > m, s"Saturation bit width $m must be less than data bit width $getWidth")
    m match {
      case 0          => this << 0
      case x if x > 0 => this._sat(m)
      case _          => (Vec(this.sign,-m).asBits ## this).asSInt //sign bit expand
    }
  }

  private def _sat(m: Int): SInt ={
    val ret = SInt(getWidth-m bit)
    when(this.sign){//negative process
      when(!this(getWidth-1 downto getWidth-m-1).asBits.andR){
        ret := ret.minValue
      }.otherwise{
        ret := this(getWidth-m-1 downto 0)
      }
    }.otherwise{//positive process
      when(this(getWidth-2 downto getWidth-m-1).asBits.orR){
        ret := ret.maxValue
      }.otherwise {
        ret := this(getWidth-m- 1 downto 0)
      }
    }
    ret
  }

  def satWithSym(m: Int): SInt = sat(m).symmetry

  /**highest m bits Discard */
  override def trim(m: Int): SInt = this(getWidth-m-1 downto 0)

  /**Round Api*/

  /**return w(this)-n bits*/
  override def floor(n: Int): SInt = {
    require(getWidth > n, s"floor bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => this._floor(n)
      case _          => this << -n
    }
  }
  private def _floor(n: Int): SInt = this >> n

  /**
    * SInt ceil
    * @example{{{ val mySInt = SInt(w bits).ceil }}}
    * @param  n : ceil lowerest n bit
    * @return a new SInt of width (w - n + 1)
    */
  override def ceil(n: Int, align: Boolean = true): SInt = {
    require(getWidth > n, s"ceil bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _ceil(n).sat(1) else _ceil(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n+1 bits*/
  private def _ceil(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    when(sign){
      ret := this._negativeCeil(n).expand
    }.otherwise{
      ret := this._positiveCeil(n)
    }
    ret
  }
  /**return w(this)-n   bits*/
  private def _negativeCeil(n: Int): SInt ={
    val ret = SInt(getWidth-n bits)
    when(this(n-1 downto 0).orR){
      ret := this(getWidth - 1 downto n) + 1
    }.otherwise{
      ret := this(getWidth - 1 downto n)
    }
    ret
  }
  /**return w(this)-n+1 bits*/
  private def _positiveCeil(n: Int): SInt ={
    val ret = SInt(getWidth-n+1 bits)
    ret := this(getWidth-2 downto 0).asUInt._ceil(n).intoSInt
    ret
  }

  /** SInt roundUp lowest m bits, friendly for hardware timing and area
    * sign * floor(abs(x))
    * */
  override def floorToZero(n: Int): SInt = {
    require(getWidth > n, s"floorToZero bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => _floorToZero(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n bits*/
  private def _floorToZero(n: Int): SInt = {
    val ret = SInt(getWidth-n bits)
    when(sign){
      ret := this._negativeCeil(n)
    }.otherwise{
      ret := this._floor(n)
    }
    ret
  }

  /** SInt roundUp lowest m bits, friendly for hardware timing and area
    * sign * ceil(abs(x))
    * */
  override def ceilToInf(n: Int, align: Boolean = true): SInt = {
    require(getWidth > n, s"ceilToInf bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _ceilToInf(n).sat(1) else _ceilToInf(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n+1 bits*/
  private def _ceilToInf(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    when(sign){
      ret := this._floor(n).expand
    }.otherwise{
      ret := this._positiveCeil(n)
    }
    ret
  }

  /** SInt roundUp lowest m bits, friendly for hardware timing and area
    * floor(x + 0.5)
    * */
  override def roundUp(n: Int, align: Boolean = true): SInt = {
    require(getWidth > n, s"RoundUp bit width $n must be less than data bit width $getWidth")
    n match {
      case 0         => this << 0
      case x if x >0 => if(align) _roundUp(n).sat(1) else _roundUp(n)
      case _         => this << -n
    }
  }
  /**return w(this)-n bits*/
  private def _roundUp(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    val positive0p5: SInt = (Bits(getWidth-n bits).clearAll ## True).asSInt
    ret := (this(getWidth-1 downto n-1) +^ positive0p5)._floor(1) //(x + 0.5).floor
    ret
  }

  /** SInt roundDown lowest m bits, complex for hardware , not recommended
    * The algorithm represented by python code :
    * ceil(x - 0.5)
    * */
  override def roundDown(n: Int, align: Boolean): SInt = {
    require(getWidth > n, s"RoundDown bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundDown(n).sat(1) else _roundDown(n)
      case _          => this << -n
    }
  }
  private def _roundDown(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    val negative0p5: SInt = (Bits(getWidth-n+1 bits).setAll ## Bits(n-1 bits).clearAll).asSInt
    val sub0p5: SInt = this(getWidth-1 downto 0) +^ negative0p5 //need carry
    when(sub0p5.sign){
      ret := sub0p5._negativeCeil(n)
    }.otherwise{
      ret := sub0p5(getWidth-1 downto 0)._positiveCeil(n)
    }
    ret
  }

  /** SInt roundToZero
    * The algorithm represented by python code :
    * sign * ceil(abs(x) - 0.5)
    * */
  override def roundToZero(n: Int, align: Boolean): SInt = {
    require(getWidth > n, s"RoundToZero bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToZero(n).sat(1) else _roundToZero(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n bits*/
  private def _roundToZero(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    val positive0p5: SInt = (Bits(getWidth-n bits).clearAll ## True ## Bits(n-1 bits).clearAll()).asSInt
    val negative0p5: SInt = (Bits(getWidth-n+1 bits).setAll ## Bits(n-1 bits).clearAll).asSInt
    val sub0p5ForPos: SInt = this(getWidth-1 downto 0) +^ negative0p5
    val add0p5ForNeg: SInt = this(getWidth-1 downto 0) +  positive0p5  //no carry needed
    when(sub0p5ForPos.sign){
      ret := add0p5ForNeg._floor(n).expand
    }.otherwise{
      ret := sub0p5ForPos(getWidth-1 downto 0)._positiveCeil(n)
    }
    ret
  }

  /** SInt roundToInf
    * sign * floor(abs(x) + 0.5)
    * */
  override def roundToInf(n: Int, align: Boolean = true): SInt = {
    require(getWidth > n, s"RoundToInf bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToInf(n).sat(1) else _roundToInf(n)
      case _          => this << -n
    }
  }
  /**return w(this)-n+1 bits*/
  private def _roundToInf(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    val positive0p5: SInt = (Bits(getWidth-n bits).clearAll ## True ## Bits(n-1 bits).clearAll()).asSInt
    val negative0p5: SInt = (Bits(getWidth-n+1 bits).setAll ## Bits(n-1 bits).clearAll).asSInt
    val sub0p5ForNeg: SInt = this(getWidth-1 downto 0) +^ negative0p5  //need carry
    val add0p5ForPos: SInt = this(getWidth-1 downto 0) +^ positive0p5  //need carry
    when(sub0p5ForNeg.sign){
      ret := sub0p5ForNeg._negativeCeil(n)
    }.otherwise{
      ret := add0p5ForPos._floor(n)
    }
    ret
  }

  override def roundToEven(n: Int, align: Boolean): SInt = {
    require(getWidth > n, s"RoundToEven bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToEven(n).sat(1) else _roundToEven(n)
      case _          => this << -n
    }
  }

  private def _roundToEven(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    when (!this(n)) {
      ret := _roundDown(n)
    } otherwise {
      ret := _roundUp(n)
    }
    ret
  }

  override def roundToOdd(n: Int, align: Boolean): SInt = {
    require(getWidth > n, s"RoundToOdd bit width $n must be less than data bit width $getWidth")
    n match {
      case 0          => this << 0
      case x if x > 0 => if(align) _roundToOdd(n).sat(1) else _roundToOdd(n)
      case _          => this << -n
    }
  }

  private def _roundToOdd(n: Int): SInt = {
    val ret = SInt(getWidth-n+1 bits)
    when (!this(n)) {
      ret := _roundUp(n)
    } otherwise {
      ret := _roundDown(n)
    }
    ret
  }

  //SpinalHDL chose roundToInf as default round
  override def round(n: Int, align: Boolean = true): SInt = roundToInf(n, align)

  def expand: SInt = (this.sign ## this.asBits).asSInt

  protected def _fixEntry(roundN: Int, roundType: RoundType, satN: Int): SInt = {
    roundType match{
      case RoundType.CEIL          => this.ceil(roundN, false).sat(satN + 1)
      case RoundType.FLOOR         => this.floor(roundN).sat(satN)
      case RoundType.FLOORTOZERO   => this.floorToZero(roundN).sat(satN)
      case RoundType.CEILTOINF     => this.ceilToInf(roundN, false).sat(satN + 1)
      case RoundType.ROUNDUP       => this.roundUp(roundN, false).sat(satN + 1)
      case RoundType.ROUNDDOWN     => this.roundDown(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOZERO   => this.roundToZero(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOINF    => this.roundToInf(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOEVEN   => this.roundToEven(roundN, false).sat(satN + 1)
      case RoundType.ROUNDTOODD    => this.roundToOdd(roundN, false).sat(satN + 1)
    }
  }

  /**Factory fixTo Function*/
  private def fixToWrap(section: Range.Inclusive, roundType: RoundType, sym: Boolean): SInt = {
    val w: Int = this.getWidth
    val wl: Int = w - 1
    val ret = (section.min, section.max, section.size) match {
      case (0,  _,   `w`) => this << 0
      case (x, `wl`,  _ ) => _fixEntry(x, roundType, satN = 0)
      case (0,  y,    _ ) => this.sat(this.getWidth -1 - y)
      case (x,  y,    _ ) => _fixEntry(x, roundType, satN = this.getWidth -1 - y)
    }
    if(sym) ret.symmetry else ret
  }

  def fixTo(section: Range.Inclusive, roundType: RoundType, sym: Boolean): SInt = {

    class fixTo(width: Int, section: Range.Inclusive,
                roundType: RoundType, sym: Boolean) extends Component{

      val symTag = if(sym) "_sym" else ""
      definitionName = s"SInt${width}fixTo${section.max}_${section.min}_${roundType}${symTag}"

      val din = in SInt(width bits)
      val dout = out SInt(section.size bits)
      dout := din.fixToWrap(section, roundType, sym)
    }

    if(GlobalData.get.config.fixToWithWrap) {
      val dut = new fixTo(this.getWidth, section, roundType, sym)
      dut.din := this
      dut.dout
    } else {
      fixToWrap(section, roundType, sym)
    }
  }

  def fixTo(section: Range.Inclusive, roundType: RoundType): SInt = fixTo(section, roundType, getFixSym())
  def fixTo(section: Range.Inclusive): SInt = fixTo(section, getFixRound(), getFixSym())

  def fixTo(q: QFormat, roundType: RoundType, sym: Boolean ): SInt = {
    val section = getfixSection(q)
    fixTo(section, roundType, sym)
  }

  def fixTo(q: QFormat, roundType: RoundType): SInt = fixTo(q, roundType, getFixSym())
  def fixTo(q: QFormat): SInt = fixTo(q, getFixRound(), getFixSym())

  /**
    * Negative number
    * @example{{{ val result = -mySInt }}}
    * @return return a negative number
    */
  def unary_- : SInt = wrapUnaryOperator(new Operator.SInt.Minus)

  /**
    * Logical shift Right (output width == input width)
    * @example{{{ val result = mySInt >> myUIntShift }}}
    * @param that the number of right shift
    * @return a Bits of width : w(this)
    */
  def >>(that: UInt): SInt = wrapBinaryOperator(that, new Operator.SInt.ShiftRightByUInt)
  /** Logical shift Left (output width will increase of : w(this) + max(that) bits  */
  def <<(that: UInt): SInt = wrapBinaryOperator(that, new Operator.SInt.ShiftLeftByUInt)

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
    val width   = widthOf(this)
    val thatMod = that % width
    this(this.high - thatMod downto 0) @@ this(this.high downto this.high - thatMod + 1)
  }

  override def rotateRight(that: Int): SInt = {
    val width   = widthOf(this)
    val thatMod = that % width
    this(thatMod - 1 downto 0) @@ this(this.high downto thatMod)
  }

  /**
    * Assign a range value to a SInt
    * @example{{{ core.io.interrupt = (0 -> uartCtrl.io.interrupt, 1 -> timerCtrl.io.interrupt, default -> false)}}}
    * @param rangesValue The first range value
    * @param _rangesValues Others range values
    */
  def :=(rangesValue : (Any, Any), _rangesValues: (Any, Any)*) : Unit = {
    val rangesValues = rangesValue +: _rangesValues
    S.applyTuples(this, rangesValues)
  }

  def :=(value : String) : Unit = this := S(value)

  override def assignFromBits(bits: Bits): Unit = this := bits.asSInt
  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = this(hi downto lo).assignFromBits(bits)

  /**
    * Cast a SInt into an UInt
    * @example {{{ myUInt := mySInt.asUInt }}}
    * @return a UInt data
    */
  def asUInt: UInt = wrapCast(UInt(), new CastSIntToUInt)

  override def asBits: Bits = wrapCast(Bits(), new CastSIntToBits)

  private[core] override def isEqualTo(that: Any): Bool = that match {
    case that: SInt           => wrapLogicalOperator(that, new Operator.SInt.Equal)
    case that: MaskedLiteral  => that === this
    case _                    => SpinalError(s"Don't know how compare $this with $that"); null
  }

  private[core] override def isNotEqualTo(that: Any): Bool = that match {
    case that: SInt          => wrapLogicalOperator(that, new Operator.SInt.NotEqual)
    case that: MaskedLiteral => that =/= this
    case _                   => SpinalError(s"Don't know how compare $this with $that"); null
  }

  private[core] override def newMultiplexerExpression() = new MultiplexerSInt
  private[core] override def newBinaryMultiplexerExpression() = new BinaryMultiplexerSInt

  override def resize(width: Int): this.type = wrapWithWeakClone({
    val node   = new ResizeSInt
    node.input = this
    node.size  = width
    node
  })

  override def resize(width: BitCount) : SInt = resize(width.value)

  override def minValue: BigInt = -(BigInt(1) << (getWidth - 1))
  override def maxValue: BigInt =  (BigInt(1) << (getWidth - 1)) - 1

  override def apply(bitId: Int): Bool = newExtract(bitId, new SIntBitAccessFixed)
  override def apply(bitId: UInt): Bool = newExtract(bitId, new SIntBitAccessFloating)
  override def apply(offset: Int, bitCount: BitCount): this.type  = newExtract(offset + bitCount.value - 1, offset, new SIntRangedAccessFixed).setWidth(bitCount.value)
  override def apply(offset: UInt, bitCount: BitCount): this.type = newExtract(offset, bitCount.value, new SIntRangedAccessFloating).setWidth(bitCount.value)

  private[core] override def weakClone: this.type = new SInt().asInstanceOf[this.type]
  override def getZero: this.type = S(0, this.getWidth bits).asInstanceOf[this.type]

  override def getZeroUnconstrained: this.type = S(0).asInstanceOf[this.type]
  override def getAllTrue: this.type = S(if(getWidth != 0) -1 else 0, this.getWidth bits).asInstanceOf[this.type]
  override def setAll(): this.type = {
    this := (if(getWidth != 0) -1 else 0)
    this
  }

  override def assignDontCare(): this.type = {
    this.assignFrom(SIntLiteral(BigInt(0), (BigInt(1) << this.getWidth) - 1, widthOf(this)))
    this
  }

  override private[core] def formalPast(delay: Int) = this.wrapUnaryOperator(new Operator.Formal.PastSInt(delay))

  def reversed = S(B(this.asBools.reverse)).asInstanceOf[this.type]

  override def assignFormalRandom(kind: Operator.Formal.RandomExpKind) = this.assignFrom(new Operator.Formal.RandomExpSInt(kind, widthOf(this)))
}
