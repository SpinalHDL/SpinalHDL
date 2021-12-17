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

sealed trait RoundType

object RoundType{
  case object CEIL           extends RoundType ;// Wikipedia name: RoundUp
  case object FLOOR          extends RoundType ;// Wikipedia name: RoundDown
  case object FLOORTOZERO    extends RoundType ;// Wikipedia name: RoundToZero
  case object CEILTOINF      extends RoundType ;// Wikipedia name: RoundToInf
  case object ROUNDUP        extends RoundType ;// Wikipedia name: RoundHalfUp
  case object ROUNDDOWN      extends RoundType ;// Wikipedia name: RoundHalfDown
  case object ROUNDTOZERO    extends RoundType ;// Wikipedia name: RoundHalfToZero
  case object ROUNDTOINF     extends RoundType ;// Wikipedia name: RoundHalfToInf
  case object ROUNDTOEVEN    extends RoundType ;// Wikipedia name: RoundHalfToEven; Have not been implemented yet
  case object ROUNDTOODD     extends RoundType ;// Wikipedia name: RoundHalfToOdd ; Have not been implemented yet
}

case class FixPointConfig(roundType: RoundType,
                          symmetric: Boolean){
  def on[B](body : => B) = {
    FixPointProperty(this) on {
      body
    }
  }
  def apply[B](body : => B): B = on(body)
  def setAsDefault() = FixPointProperty.set(this)
}



object FixPointProperty extends ScopeProperty[FixPointConfig]{
  override def default = DefaultFixPointConfig
}

object getFixRound{
  def apply(): RoundType = FixPointProperty.get.roundType
}

object getFixSym{
  def apply(): Boolean = FixPointProperty.get.symmetric
}


//case class FixPointConfig(roundType: RoundType,
//                          symmetric: Boolean) {
//
//  def flush(infos: String = s"${this} enabled!"): FixPointConfig = {
//    setFixRound(roundType)
//    setFixSym(symmetric)
//    SpinalInfo(infos)
//    this
//  }
//
//  def apply[T](block: => T): T = {
//    val outer = FixPointConfig(getFixRound(), getFixSym())
//    this.flush()
//    val ret: T = block
//    outer.flush(s"$outer recovered")
//    ret
//  }
//}
//

//
///*singleton Object*/
//private object GlobalRoundType{
//  private var roundType: RoundType = RoundType.ROUNDTOINF
//
//  def apply(): RoundType = roundType
//
//  def set(round: RoundType) = {
//    roundType = round
//    roundType
//  }
//}
//
//private object GlobalSymmetricType{
//  private var symmetric: Boolean = false
//
//  def apply(): Boolean = symmetric
//
//  def set(sym: Boolean) = {
//    symmetric = sym
//    symmetric
//  }
//}
//
//object getFixRound{
//  def apply(): RoundType = GlobalRoundType()
//}
//
//object setFixRound{
//  def apply(round: RoundType): RoundType = GlobalRoundType.set(round)
//}
//
//object getFixSym{
//  def apply(): Boolean = GlobalSymmetricType()
//}
//
//object setFixSym{
//  def apply(sym: Boolean): Boolean = GlobalSymmetricType.set(sym)
//}
//
//object ResetFixConfig{
//  def apply() = {
//    DefaultFixPointConfig().flush()
//  }
//}
//
//object ShowFixConfig{
//  def apply(pretag: String = "") = {
//    SpinalInfo(pretag + " " + FixPointConfig(getFixRound(),getFixSym()).toString())
//  }
//}

object Fix {

  def apply(maxExp: Int, bitCount: Int, signed: Boolean = false): Fix = new Fix(QFormat(bitCount, bitCount-maxExp, signed = signed))
  def apply(i: Int): Fix = Fix(BigInt(i))
  def apply(i: BigInt): Fix = {
    val fixed = new Fix(QFormat(i.bitCount+1, 0, signed = true))
    fixed.raw := i.intoSInt.asBits
    fixed
  }
  def apply(f: Float): Fix = Fix(BigDecimal(f))
  def apply(d: Double): Fix = Fix(BigDecimal(d))
  def apply(d: BigDecimal): Fix = {
    val i = (d * BigDecimal(10).pow(d.precision)).toBigInt()
    val fracBitCount = log2Up(d.precision)
    val fixed = Fix(QFormat(i.bitCount+1, fracBitCount, signed = true))
    fixed.raw := i.intoSInt.asBits
    fixed
  }

  def SFix(maxExp: Int, bitCount: Int) = new Fix(QFormat(bitCount, bitCount-maxExp, signed = true))
  def UFix(maxExp: Int, bitCount: Int) = new Fix(QFormat(bitCount, bitCount-maxExp, signed = false))

  implicit class SIntToFix(num: SInt) {
    def asFix(): Fix = {
      val res = new Fix(QFormat(num.getWidth, 0, signed = true))
      res := num
      res
    }
  }

  implicit class UIntToFix(num: SInt) {
    def asFix(): Fix = {
      val res = new Fix(QFormat(num.getWidth, 0, signed = false))
      res := num
      res
    }
  }
}


case class Fix(q: QFormat) extends MultiData with Num[Fix] {

  require(q.amplify >= 0)
  require(q.fraction >= 0)

  val raw = Bits(q.width bit)

  def alignLsb(left: Fix, right: Fix): (Fix, Fix) = {
    val difLsb = left.q.fraction - right.q.fraction
    if (difLsb == 0)
      (left, right)
    else if (difLsb > 0)
      (left, right << difLsb)
    else
      (left << -difLsb, right)
  }

  override def tag(q: QFormat): Fix = ???

  override def +(right: Fix): Fix = {
    val (_left, _right): (Fix, Fix) = alignLsb(this, right)
    val res = new Fix(_left.q + _right.q)
    res.raw := ((_left.q.signed, _right.q.signed) match {
      case (false, false) => _left.raw.asUInt + _right.raw.asUInt
      case (true, false) => _left.raw.asSInt + _right.raw.asUInt.intoSInt
      case (false, true) => _left.raw.asUInt.intoSInt + _right.raw.asSInt
      case (true, true) => _left.raw.asSInt + _right.raw.asSInt
    }).asBits
    res
  }

  override def +^(right: Fix): Fix = extend() + right.extend()

  override def +|(right: Fix): Fix = (this +^ right).sat(this.q.width)

  override def -(right: Fix): Fix = this + (-right)

  override def -^(right: Fix): Fix = extend() - right.extend()

  override def -|(right: Fix): Fix = (this -^ right).sat(this.q.width)

  override def *(right: Fix): Fix = {
    val (_left, _right) = alignLsb(this, right)
    val res = new Fix(_left.q * _right.q)
    res.raw := ((_left.q.signed, _right.q.signed) match {
      case (false, false) => _left.raw.asUInt * _right.raw.asUInt
      case (true, false) => _left.raw.asSInt * _right.raw.asUInt.intoSInt
      case (false, true) => _left.raw.asUInt.intoSInt * _right.raw.asSInt
      case (true, true) => _left.raw.asSInt * _right.raw.asSInt
    }).asBits
    res
  }

  override def /(right: Fix): Fix = {
    val (_left, _right) = alignLsb(this, right)
    val res = new Fix(_left.q * _right.q)
    res.raw := ((_left.q.signed, _right.q.signed) match {
      case (false, false) => _left.raw.asUInt / _right.raw.asUInt
      case (true, false) => _left.raw.asSInt / _right.raw.asUInt.intoSInt
      case (false, true) => _left.raw.asUInt.intoSInt / _right.raw.asSInt
      case (true, true) => _left.raw.asSInt / _right.raw.asSInt
    }).asBits
    res
  }

  override def %(right: Fix): Fix = {
    val (_left, _right) = alignLsb(this, right)
    val res = new Fix(QFormat(math.min(this.q.numeric, _right.q.numeric), math.max(this.q.fraction, _right.q.fraction), this.q.signed | _right.q.signed))
    res.raw := ((_left.q.signed, _right.q.signed) match {
      case (false, false) => _left.raw.asUInt % _right.raw.asUInt
      case (true, false) => _left.raw.asSInt % _right.raw.asUInt.intoSInt
      case (false, true) => _left.raw.asUInt.intoSInt % _right.raw.asSInt
      case (true, true) => _left.raw.asSInt % _right.raw.asSInt
    }).asBits
    res
  }

  override def <(right: Fix): Bool = ???

  override def <=(right: Fix): Bool = ???

  override def >(right: Fix): Bool = ???

  override def >=(right: Fix): Bool = ???

  override def <<(shift: Int): Fix = {
    assert(this.q.amplify >= shift)
    val res = new Fix(QFormat(this.q.width, this.q.fraction+shift, this.q.signed))
    res.raw := this.raw
    res
  }

  def <<|(shift: Int): Fix = {
    val res = new Fix(QFormat(this.q.width+shift, this.q.fraction, this.q.signed))
    if (this.q.signed)
      res.raw := (this.raw.asSInt << shift).asBits
    else
      res.raw := (this.raw.asUInt << shift).asBits
    res
  }

  override def >>(shift: Int): Fix = {
    assert(this.q.fraction >= shift)
    val res = new Fix(QFormat(this.q.width, this.q.fraction-shift, this.q.signed))
    res.raw := this.raw
    res
  }

  def >>|(shift: Int): Fix = {
    assert(this.q.numeric >= shift)
    val res = new Fix(QFormat(this.q.width-shift, math.min(this.q.fraction-shift, 0), this.q.signed))
    if (this.q.signed)
      res.raw := (this.raw.asSInt >> shift).asBits
    else
      res.raw := (this.raw.asUInt >> shift).asBits
    res
  }

  /**
   * Aligned LSB saturation.
   * Ex:
   *    SQ(16, 5).sat(8)  -> SQ( 8, 5)
   *    UQ( 8, 4).sat(7)  -> UQ( 7, 4)
   *    SQ(12, 6).sat(10) -> SQ(10, 6)
   *    SQ(10,10).sat(8)  -> SQ( 8, 8)
   * @param m Bit width from LSB
   * @return
   */
  override def sat(m: Int): Fix = {
    if (this.q.width < m) {
      val signBit = if (this.q.signed) 1 else 0
      if (this.q.fraction+signBit <= m) {
        // Truncate the fractional part
        val res = new Fix(QFormat(signBit, this.q.fraction-m-signBit, this.q.signed))
        val bitTrimmed = this.raw(this.q.fraction downto this.q.fraction-m+signBit)
        if (this.q.signed)
          res := bitTrimmed.asSInt.sat(m)
        else
          res := bitTrimmed.asUInt.sat(m)
        res
      } else {
        val res = new Fix(QFormat(m-this.q.fraction-signBit, this.q.fraction, this.q.signed))
        if (this.q.signed)
          res := this.raw.asSInt.sat(m)
        else
          res := this.raw.asUInt.sat(m)
        res
      }
    } else {
      // Trivial operations
      if (this.q.width == m) {
        val res = new Fix(this.q)
        res := this
        res
      } else {
        this.extend(m-this.q.width)
      }
    }
  }

  def sat(whole: Int, frac: Int): Fix = {
    ((this << frac).truncate() >> frac).sat(whole+frac)
  }

  /**
   * Drop the highest m bits
   * Ex:
   *    SQ(16,5).sat(8)  -> SQ( 8,5)
   *    SQ( 5,5).sat(8)  -> SQ( 8,5)
   * @param m Bit width from LSB
   * @return
   */
  override def trim(m: Int): Fix = {
    assert(this.q.width >= m, s"Cannot trim Fix() to ${m} bits!")
    if (m < this.q.width) {
      val res = new Fix(QFormat(m, 0, this.q.signed))
      if (this.q.signed)
        res.raw := (this.raw << this.q.fraction).asSInt.trim(m).asBits
      else
        res.raw := (this.raw << this.q.fraction).asUInt.trim(m).asBits
      res >> this.q.fraction
    } else {
      this.extend(m-this.q.width)
    }
  }

  def truncate(): Fix = (this >> this.q.fraction).trim(this.q.width-this.q.fraction)

  override def floor(n: Int): Fix = ???

  override def ceil(n: Int, align: Boolean): Fix = ???

  override def floorToZero(n: Int): Fix = ???

  override def ceilToInf(n: Int, align: Boolean): Fix = ???

  override def roundUp(n: Int, align: Boolean): Fix = ???

  override def roundDown(n: Int, align: Boolean): Fix = ???

  override def roundToZero(n: Int, align: Boolean): Fix = ???

  override def roundToInf(n: Int, align: Boolean): Fix = ???

  override def round(n: Int, align: Boolean): Fix = ???

  def unary_-(): Fix = {
    val res = new Fix(-this.q)
    if (this.q.signed)
      res.raw := (-raw.asSInt).asBits
    else
      res.raw := raw.asUInt.twoComplement(True).asBits
    res
  }

  /**
   * Sign extends the whole portion and can add bits to the fractional portion.
   * @param numeric Number of numeric bits to add
   * @param fractional Number of fractional bits to add
   * @return The new sign extended (if signed) fixed-point
   */
  def extend(numeric: Int = 1, fractional: Int = 0): Fix = {
    val res = new Fix(QFormat(this.q.width+numeric+fractional, this.q.fraction+fractional, signed = this.q.signed))
    if (this.q.signed)
      res.raw := (Bits(numeric bit).setAllTo(raw.msb) ## raw) << fractional
    else
      res.raw := (Bits(numeric bit).clearAll() ## raw) << fractional
    res
  }

  def :=(that: Fix): Unit = {
    val shifted = (that >> (this.q.fraction-that.q.fraction))
    if (shifted.q.signed) {
      if (this.q.signed) {
        this.raw := shifted.raw
      } else {
        when (shifted.raw.msb) {
          this.raw.clearAll()
        } otherwise {
          this.raw := shifted.raw
        }
      }
    } else {
      if (this.q.signed) {
        this.raw := that.extend(this.q.nonFraction, this.q.fraction).raw
      } else {
        this.raw := that.raw
      }
    }
  }

  def :=(that: UInt): Unit = {
    if (this.q.signed)
      if (that.getWidth > this.q.numeric)
        this.raw := that.intoSInt.sat(this.q.numeric).asBits
      else
        this.raw := that.intoSInt.expand(this.q.numeric).asBits
    else
      if (that.getWidth > this.q.numeric)
        this.raw := that.sat(this.q.numeric).asBits
      else
        this.raw := that.expand(this.q.numeric).asBits
  }

  def :=(that: SInt): Unit = {
    if (this.q.signed) {
      if (that.getWidth > this.q.numeric)
        this.raw := that.sat(this.q.numeric).asBits
      else
        this.raw := that.expand(this.q.numeric).asBits
    } else {
      when (that.sign) {
        this.raw.clearAll()
      } otherwise {
        if (that.getWidth > this.q.numeric)
          this.raw := that.sat(this.q.numeric).asBits
        else
          this.raw := that.expand(this.q.numeric).asBits
      }
    }
  }

  def :=(that: BigInt): Unit = if (this.q.signed) { this := BigIntToSInt(that) } else { this := BigIntToUInt(that) }
  def :=(that: BigDecimal): Unit = {
    val scaledUp: BigInt = (that * BigDecimal(2).pow(this.q.fraction)).toBigInt()
    val scaledFix = new Fix(QFormat(scaledUp.bitCount, 0, signed=this.q.signed))
    scaledFix := scaledUp
    this := (scaledFix >> this.q.fraction)
  }
  def :=(that: Int): Unit = this := BigInt(that)
  def :=(that: Long): Unit = this := BigInt(that)
  def :=(that: Float): Unit = this := BigDecimal(that)
  def :=(that: Double): Unit = this := BigDecimal(that)

  def :=(that: Bits): Unit = this.raw := that

  def toSInt: SInt = if (this.q.signed) this.raw.asSInt else this.raw.asUInt.asSInt
  def toUInt: UInt = if (this.q.signed) this.raw.asSInt.asUInt else this.raw.asUInt

  override def clone: this.type = new Fix(this.q).asInstanceOf[this.type]

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  override private[spinal] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) => this := that.asInstanceOf[Fix]
      case _ => SpinalError("Undefined assignment")
    }
  }

  override def toString : String = s"(${this.name} : Fix[${this.q.toString}])"
}