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
      case 0 => this
      case x if x > 0 => {
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
      case _ => (Bits(-m bits).setAll() ## this).asSInt //sign bit expand
    }

  }

  def satWithSym(m: Int): SInt = sat(m).symmetry
  override def floor(n: Int): SInt = this >> n

  /** SInt Round lowest m bits
    * The algorithm represented by python code :
    * def sround(x) = math.ceil(x-0.5) if x<0 else math.floor(x+0.5)
    * */
  override def round(n: Int): SInt = {
    require(getWidth > n, s"Round bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this
      case x if x >0 => {
        val ret = SInt(getWidth-n+1 bits)
        when(sign){
          val negtive0p5: SInt = (Bits(getWidth-n+1 bits).setAll ## Bits(n-1 bits).clearAll).asSInt
          ret := (this +^ negtive0p5).negtiveCeil(n) // (x - 0.5).ceil
        }.otherwise{
          ret := this(getWidth-2 downto 0).asUInt.round(n).toSInt //expand 1 bit
        }
        ret
      }
      case _ => this << -n
    }
  }

  /** SInt Special Round lowest m bits
    * The algorithm represented by python code :
    * def sround(x) = math.floor(x+0.5)"
    * special Round compare to Round save 1 mux and 1 reduce-OR only difference on -0.5 process
    * sround(-1.5) = -1; sround(1.5) = 2; sround(-1.50001) = -2
    *  round(-1.5) = -2;  round(1.5) = 2;  round(-1.50001) = -2
    * sround is better for timing almost no loss of precision
    * */
  def sround(n: Int): SInt = {
    require(getWidth > n, s"Round bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this
      case x if x > 0 => {
        val ret = SInt(getWidth-n+1 bits)
        val postive0p5: SInt = (Bits(getWidth-n bits).clearAll ## Bits(1 bits).setAll).asSInt //0.5
        ret :=  (this(getWidth-1 downto n-1) +^ postive0p5).floor(1) //(x + 0.5).floor
        ret
      }
      case _ => this << -n
    }
  }

  /* private
   * negtiveCeil is safe without expand
   * SInt(w bits).negtiveCeil(n)
   * return new SInt of width (w - n)
   * */
  private def negtiveCeil(n: Int): SInt ={
    val ret = SInt(getWidth-n bits)
    when(this(n-1 downto 0).orR){
      ret := this(getWidth - 1 downto n) + 1
    }.otherwise{
      ret := this(getWidth - 1 downto n)
    }
    ret
  }

  /**
    * SInt ceil
    * @example{{{ val mySInt = SInt(w bits).ceil }}}
    * @param  n : ceil lowerest n bit
    * @return a new SInt of width (w - n + 1)
    */
  def ceil(n: Int): SInt = {
    require(getWidth > n, s"ceil bit width $n must be less than data bit width $getWidth")
    n match {
      case 0 => this
      case x if x > 0 => {
        val ret = SInt(getWidth-n+1 bits)
        when(sign){
          ret := this.negtiveCeil(n).expand
        }.otherwise{
          ret := this(getWidth-2 downto 0).asUInt.ceil(n).toSInt
        }
        ret
      }
      case _ => this << -n
    }
  }

  def roundNoExpand(n: Int): SInt = if(n>0) round(n).sat(1) else round(n)
  def sroundNoExpand(n: Int): SInt = if(n>0) sround(n).sat(1) else sround(n)
  def ceilNoExpand(n: Int): SInt = if(n>0) ceil(n).sat(1) else ceil(n)
  def trim(m: Int): SInt = this(getWidth-m-1 downto 0)
  def expand: SInt = (this.sign ## this.asBits).asSInt

  /**fixpoint Api*/
  //TODO: add default fix mode in spinal global config
  def fixTo(section: Range.Inclusive, roundType: RoundType =  RoundHalfUp, sym: Boolean = false): SInt = {
    val ret = roundType match {
      case RoundUpp            => this.fixWithCeil(section)
      case RoundDown           => this.fixWithFloor(section)
      case RoundHalfUp         => this.fixWithRound(section)
      case RoundHalfUpSpecial  => this.fixWithSRound(section)
      case _                   => this.fixWithRound(section)
    }
    if(sym) ret.symmetry else ret
  }

  def fixWithFloor(section: Range.Inclusive): SInt = {
    val _w  = this.getWidth
    val _wl = _w - 1
    (section.min, section.max, section.size) match {
      case (0,  _,   `_w`) => this
      case (x, `_wl`, _  ) => this.floor(x)
      case (0,  y,    _  ) => this.sat(this.getWidth - 1 - y)
      case (x,  y,    _  ) => this.floor(x).sat(this.getWidth -1 - y)
    }
  }

  def fixWithRound(section: Range.Inclusive): SInt = {
    val _w  = this.getWidth
    val _wl = _w - 1
    (section.min, section.max, section.size) match {
      case (0, _,    `_w`) => this
      case (x, `_wl`, _  ) => if(x >0) this.round(x).sat(1)
                              else     this.round(x)
      case (0, y,     _  ) => this.sat(this.getWidth - 1 - y)
      case (x, y,     _  ) => if(x >0) this.round(x).sat(this.getWidth -1 - y + 1)
                              else     this.round(x).sat(this.getWidth -1 - y)
    }
  }

  def fixWithSRound(section: Range.Inclusive): SInt = {
    val _w  = this.getWidth
    val _wl = _w - 1
    (section.min, section.max, section.size) match {
      case (0, _,    `_w`) => this
      case (x, `_wl`, _  ) => if(x >0) this.sround(x).sat(1)
                              else     this.sround(x)
      case (0, y,     _  ) => this.sat(this.getWidth -1 - y)
      case (x, y,     _  ) => if(x >0) this.sround(x).sat(this.getWidth - 1 - y + 1)
                              else     this.sround(x).sat(this.getWidth - 1 - y)
    }
  }

  def fixWithCeil(section: Range.Inclusive): SInt = {
    val _w  = this.getWidth
    val _wl = _w - 1
    (section.min, section.max, section.size) match {
      case (0, _,    `_w`) => this
      case (x, `_wl`, _  ) => if(x >0) this.ceil(x).sat(1)
                              else     this.ceil(x)
      case (0, y,     _  ) => this.sat(this.getWidth -1 - y)
      case (x, y,     _  ) => if(x >0) this.ceil(x).sat(this.getWidth - 1 - y + 1)
                              else     this.ceil(x).sat(this.getWidth - 1 - y)
    }
  }
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
    * Absolute value of a SInt
    * @example {{{ myUInt := mySInt.abs }}}
    * @return a UInt assign with the absolute value of the SInt
    */
  def abs: UInt = Mux(this.msb, ~this, this).asUInt + this.msb.asUInt
  /** Return the absolute value of the SInt when enable is True */
  def abs(enable: Bool): UInt = Mux(this.msb && enable, ~this, this).asUInt + (this.msb && enable).asUInt
  /** symmetric abs
    * @example {{{ S(-128,8 bits).absWithSym got U(127,7 bits) }}}
    * @return a UInt assign with the absolute value save 1 bit
    */
  def absWithSym: UInt = this.symmetry.abs.resize(getWidth-1)

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

  override def assignFromBits(bits: Bits): Unit = this := bits.asSInt
  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = this(hi downto lo).assignFromBits(bits)

  /**
    * Cast a SInt into an UInt
    * @example {{{ myUInt := mySInt.asUInt }}}
    * @return a UInt data
    */
  def asUInt: UInt = wrapCast(UInt(), new CastSIntToUInt)

  override def asBits: Bits = wrapCast(Bits(), new CastSIntToBits)

  private[core] override def isEquals(that: Any): Bool = that match {
    case that: SInt           => wrapLogicalOperator(that, new Operator.SInt.Equal)
    case that: MaskedLiteral  => that === this
    case _                    => SpinalError(s"Don't know how compare $this with $that"); null
  }

  private[core] override def isNotEquals(that: Any): Bool = that match {
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

  override def resize(width: BitCount) = resize(width.value)

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
}
