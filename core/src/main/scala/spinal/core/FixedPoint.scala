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

import spinal.core.internals.ScopeStatement

import scala.collection.mutable
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
  /** Direct conversions */
  def apply(that: UInt): Fix = {
    val fix = new Fix(that.getWidth, that.getWidth, signed=false)
    fix.raw := that.asBits
    fix
  }

  def apply(that: SInt): Fix = {
    val fix = new Fix(that.getWidth+1, that.getWidth+1, signed=true)
    fix.raw := that.asBits
    fix
  }

  def apply(that: UFix): Fix = {
    val fix = new Fix(that.maxExp, that.bitCount, signed=false)
    fix.raw := that.asBits
    fix
  }

  def apply(that: SFix): Fix = {
    val fix = new Fix(that.maxExp+1, that.bitCount+1, signed=true)
    fix.raw := that.asBits
    fix
  }
}

/**
 * Fixed-point number representation.
 *
 * Underlying bits are:
 * Fix = [S|U]N.(M-N)
 * Where:
 * N = intWidth - signed
 * M = bitWidth
 *
 * Ex:
 * U16.8 = unsigned 16 bits + 8 frac bits
 * S8.8 = sign + 7 bits + 8 frac bits
 *
 * @param intWidth Integer part bit width
 * @param bitWidth Total number bit width
 * @param signed   Interpret number as signed if true
 */
class Fix(val intWidth: Int, val bitWidth: Int, val signed: Boolean) extends MultiData with Num[Fix] {

  assert(bitWidth >= intWidth, s"${this.name}: BitWidth (${bitWidth}) must be at least as large as IntWidth (${intWidth})!")

  def signWidth = if (signed) 1 else 0
  /** Total binary width without sign */
  def numericWidth = bitWidth - signWidth
  /** Whole binary width without sign bit */
  def wholeWidth = intWidth - signWidth
  /** Fractional binary width */
  def fracWidth = bitWidth - intWidth

  val raw: Bits = Bits(bitWidth bit)

  val maxValue: BigDecimal = (BigDecimal(2).pow(numericWidth) - BigDecimal(1)) * BigDecimal(2).pow(-fracWidth)
  val minValue: BigDecimal = if (signed) BigDecimal(-2).pow(numericWidth) * BigDecimal(2).pow(-fracWidth) else BigDecimal(0)
  val lsbValue: BigDecimal = BigDecimal(2).pow(-fracWidth)
  val msbValue: BigDecimal = BigDecimal(2).pow(numericWidth-1)

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  def sign: Bool = if (signed) raw.msb else False
  def int: Bits = raw.takeHigh(intWidth).dropHigh(signWidth)
  def frac: Bits = raw.takeLow(fracWidth)

  override def tag(q: QFormat): Fix = ???

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  /** Aligns the decimal point of two fixed-point numbers */
  protected def align(left: Fix, right: Fix): (Bits, Bits) = {
    val shift = left.fracWidth - right.fracWidth
    if (shift > 0)
      (left.raw, right.raw << shift)
    else if (shift < 0)
      (left.raw << -shift, right.raw)
    else
      (left.raw, right.raw)
  }

  /** Addition */
  override def +(right: Fix): Fix = {
    val (_left, _right) = align(this, right)
    val newIntWidth = math.max(this.wholeWidth, right.wholeWidth) + (this.signWidth ^ right.signWidth)
    val newFracWidth = math.max(this.fracWidth, right.fracWidth)
    val newBitWidth = newIntWidth + newFracWidth
    val res = new Fix(newIntWidth, newBitWidth, this.signed | right.signed)
    res.setName(s"${this.getName()}_${right.getName()}_add", true)
    res.raw := ((this.signed, right.signed) match {
      case (false, false) => (_left.asUInt + _right.asUInt).resize(res.bitWidth)
      case (false, true) => (_left.asUInt.asSInt + _right.asSInt).resize(res.bitWidth)
      case (true, false) => (_left.asSInt + _right.asUInt.asSInt).resize(res.bitWidth)
      case (true, true) => (_left.asSInt + _right.asSInt).resize(res.bitWidth)
    }).asBits
    res
  }

  /** Safe Addition with 1 bit expand */
  override def +^(right: Fix): Fix = {
    if (this.bitWidth == right.bitWidth) {
      this.expand(1) + right
    } else {
      this + right
    }
  }

  /** Safe Addition with saturation */
  override def +|(right: Fix): Fix = {
    val expandedAdd = this +^ right
    val realigned = expandedAdd >> (expandedAdd.fracWidth - this.fracWidth)
    realigned.sat(this.bitWidth)
  }

  /** Subtraction */
  override def -(right: Fix): Fix = {
    val (_left, _right) = align(this, right)
    val newIntWidth = math.max(this.wholeWidth, right.wholeWidth) + (this.signWidth ^ right.signWidth)
    val newFracWidth = math.max(this.fracWidth, right.fracWidth)
    val newBitWidth = newIntWidth + newFracWidth
    val res = new Fix(newIntWidth, newBitWidth, this.signed | right.signed)
    res.setName(s"${this.getName()}_${right.getName()}_sub", true)
    res.raw := ((this.signed, right.signed) match {
      case (false, false) => (_left.asUInt - _right.asUInt).resize(res.bitWidth)
      case (false, true) => (_left.asUInt.asSInt - _right.asSInt).resize(res.bitWidth)
      case (true, false) => (_left.asSInt - _right.asUInt.asSInt).resize(res.bitWidth)
      case (true, true) => (_left.asSInt - _right.asSInt).resize(res.bitWidth)
    }).asBits
    res
  }

  /** Safe Subtraction with 1 bit expand */
  override def -^(right: Fix): Fix = {
    if (this.bitWidth == right.bitWidth) {
      this.expand(1) - right
    } else {
      this - right
    }
  }

  /** Safe Subtraction with saturation */
  override def -|(right: Fix): Fix = {
    val expandedAdd = this -^ right
    val realigned = expandedAdd >> (expandedAdd.fracWidth - this.fracWidth)
    realigned.sat(this.bitWidth)
  }

  /** Multiplication */
  override def *(right: Fix): Fix = {
    val (_left, _right) = align(this, right)
    // A + B + 1 if result is signed
    val res = new Fix(this.intWidth + right.intWidth + (this.signWidth | right.signWidth),
                      _left.getWidth + _right.getWidth + (this.signWidth | right.signWidth),
                      this.signed | right.signed)
    res.setName(s"${this.getName()}_${right.getName()}_mul", true)
    res.raw := ((this.signed, right.signed) match {
      case (false, false) => _left.asUInt * _right.asUInt
      case (false, true) => _left.asUInt.intoSInt * _right.asSInt
      case (true, false) => _left.asSInt * _right.asUInt.intoSInt
      case (true, true) => _left.asSInt * _right.asSInt
    }).resize(res.bitWidth).asBits
    res
  }

  /** Division */
  override def /(right: Fix): Fix = {
    SpinalWarning("Fixed-point division is not finalized and not recommended for use! " + ScalaLocated.long)
    val (leftAligned, rightAligned) = align(this, right) // These will need additional shifting
    val newIntWidth = this.intWidth + right.fracWidth
    val newFracWidth = right.wholeWidth + this.fracWidth
    val res = new Fix(newIntWidth,
                      newIntWidth + newFracWidth,
                      this.signed | right.signed)
    val _left = leftAligned << res.fracWidth
    val _right = rightAligned
    res.setName(s"${this.getName()}_${right.getName()}_div", true)
    res.raw := ((this.signed, right.signed) match {
      case (false, false) => _left.asUInt / _right.asUInt
      case (false, true) => _left.asUInt.intoSInt / _right.asSInt
      case (true, false) => _left.asSInt / _right.asUInt.intoSInt
      case (true, true) => _left.asSInt / _right.asSInt
    }).resize(res.bitWidth).asBits
    res
  }

  /** Modulo */
  override def %(right: Fix): Fix = {
    SpinalWarning("Fixed-point modulo is not finalized and not recommended for use! " + ScalaLocated.long)
    val (leftAligned, rightAligned) = align(this, right) // These will need additional shifting
    val res = new Fix(right.intWidth + (this.signWidth | right.signWidth),
                      right.bitWidth + (this.signWidth | right.signWidth), this.signed | right.signed)
    val _left = leftAligned
    val _right = rightAligned // << right.fracWidth
    res.setName(s"${this.getName()}_${right.getName()}_mod", true)
    res.raw := ((this.signed, right.signed) match {
      case (false, false) => _left.asUInt % _right.asUInt
      case (false, true) => _left.asUInt.intoSInt % _right.asSInt
      case (true, false) => _left.asSInt % _right.asUInt.intoSInt
      case (true, true) => _left.asSInt % _right.asSInt
    }).resize(res.bitWidth).asBits
    res
  }

  /** Is less than right */
  override def <(right: Fix): Bool = {
    (this.signed, right.signed) match {
      case (false, false) => this.raw.asUInt < right.raw.asUInt
      case (false, true) => this.raw.asUInt.intoSInt < right.raw.asSInt
      case (true, false) => this.raw.asSInt < right.raw.asUInt.intoSInt
      case (true, true) => this.raw.asSInt < right.raw.asSInt
    }
  }

  /** Is equal or less than right */
  override def <=(right: Fix): Bool = {
    (this.signed, right.signed) match {
      case (false, false) => this.raw.asUInt <= right.raw.asUInt
      case (false, true) => this.raw.asUInt.intoSInt <= right.raw.asSInt
      case (true, false) => this.raw.asSInt <= right.raw.asUInt.intoSInt
      case (true, true) => this.raw.asSInt <= right.raw.asSInt
    }
  }

  /** Is greater than right */
  override def >(right: Fix): Bool = {
    (this.signed, right.signed) match {
      case (false, false) => this.raw.asUInt > right.raw.asUInt
      case (false, true) => this.raw.asUInt.intoSInt > right.raw.asSInt
      case (true, false) => this.raw.asSInt > right.raw.asUInt.intoSInt
      case (true, true) => this.raw.asSInt > right.raw.asSInt
    }
  }

  /** Is equal or greater than right */
  override def >=(right: Fix): Bool = {
    (this.signed, right.signed) match {
      case (false, false) => this.raw.asUInt >= right.raw.asUInt
      case (false, true) => this.raw.asUInt.intoSInt >= right.raw.asSInt
      case (true, false) => this.raw.asSInt >= right.raw.asUInt.intoSInt
      case (true, true) => this.raw.asSInt >= right.raw.asSInt
    }
  }

  /** Logical left shift (w(T) = w(this) + shift) */
  override def <<(shift: Int): Fix = {
    val res = new Fix(this.intWidth+shift, this.bitWidth+shift, this.signed)
    res.raw := this.raw << shift
    res
  }

  /** Logical right shift (w(T) = w(this) - shift) */
  override def >>(shift: Int): Fix = {
    assert(this.bitWidth >= shift, s"Cannot right shift ${this} by ${shift} bits!")
    val res = new Fix(math.max(this.intWidth-shift, 0), this.bitWidth-shift, this.signed)
    res.raw := this.raw >> shift
    res
  }

  /**
   * Saturate dropping the highest m bits. Equivalent to .dropHigh(m)
   * S2.5 sat(2) => S0.5
   * U8.8 sat(4) => U4.8
   * S24.8 sat(1) => S23.8
   *
   * @param m Highest m bits to drop
   * @return Saturated result
   */
  override def sat(m: Int): Fix = {
    val res = new Fix(math.max(signWidth, intWidth - m), bitWidth - m, signed = this.signed)
    res.setPartialName(s"${this.getName()}_sat", true)
    if (signed) {
      val needSatPos = this.raw.takeHigh(m).orR
      val needSatNeg = this.raw.takeHigh(m).andR
      val needSat = (needSatPos && ~this.sign) || (needSatNeg && this.sign)
      when (needSat) {
        res.raw.dropHigh(1).setAllTo(~this.sign)
      } otherwise {
        res.raw.dropHigh(1) := this.raw.dropHigh(m+1)
      }
      res.raw.msb := this.sign
    } else {
      val needSat = this.raw.dropLow(m).orR
      when (needSat) {
        res.raw.setAll()
      } otherwise {
        res.raw := this.raw.takeLow(m)
      }
    }
    res
  }

  /** Drop m lowest bits */
  override def trim(m: Int): Fix = {
    val res = new Fix(intWidth - math.max(0, m-fracWidth), bitWidth - m, signed = this.signed)
    res.setPartialName(s"${this.getName()}_trim", true)
    res.raw := this.raw.dropLow(m)
    res
  }

  def abs(): Fix = {
    val res = this.clone
    if (this.signed) {
      when (this.raw.msb) {
        res.raw := ((~this.raw).asUInt+1).asBits
      } otherwise {
        res.raw := this.raw
      }
    } else {
      res.raw := this.raw
    }
    res
  }

  def floor(): Fix = floor(this.fracWidth)
  /** lowest n bits Round Operation */
  override def floor(n: Int): Fix = {
    assert(n < this.numericWidth, f"Cannot floor(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    res.setPartialName(s"${this.getName()}_floor", true)
    res.raw := this.raw.dropLow(n)
    res
  }

  override def ceil(n: Int, align: Boolean): Fix = ???

  def ceil(): Fix = ceil(this.fracWidth)
  def ceil(n: Int): Fix = {
    assert(n < this.numericWidth, f"Cannot ceil(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    res.setPartialName(s"${this.getName()}_ceil", true)
    val fracOr = this.raw.takeLow(n).orR
    if (this.signed) {
      res.raw := (this.raw.dropLow(n).asSInt + (False ## fracOr).asSInt).asBits
    } else {
      res.raw := (this.raw.dropLow(n).asUInt + fracOr.asUInt).asBits
    }
    res
  }

  def floorToZero(): Fix = floorToZero(this.fracWidth)
  override def floorToZero(n: Int): Fix = {
    assert(n < this.numericWidth, f"Cannot floorToZero(${n}) because numeric width is only ${this.numericWidth}")
    if (this.signed) {
      val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
      res.setPartialName(s"${this.getName()}_floorZero", true)
      val fracOr = this.raw.takeLow(n).orR
      val addValue = SInt(2 bit)
      when(this.raw.msb && fracOr) {
        addValue := 1
      } otherwise {
        addValue := 0
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      floor(n).setPartialName(s"${this.getName()}_floorZero", true)
    }
  }

  override def ceilToInf(n: Int, align: Boolean): Fix = ceilToInf(n)

  def ceilToInf(): Fix = ceilToInf(this.fracWidth)
  def ceilToInf(n: Int): Fix = {
    assert(n < this.numericWidth, f"Cannot ceilToInf(${n}) because numeric width is only ${this.numericWidth}")
    if (this.signed) {
      val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
      res.setPartialName(s"${this.getName()}_ceilInf", true)
      val fracOr = this.raw.takeLow(n).orR
      val addValue = SInt(2 bit)
      when(fracOr) {
        when(!this.raw.msb) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        addValue := 0
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      ceil(n).setPartialName(s"${this.getName()}_ceilToInf", true)
    }
  }

  def roundHalfUp(): Fix = roundHalfUp(this.fracWidth)
  def roundHalfUp(n: Int): Fix = {
    assert(n < this.numericWidth-1, f"Cannot roundHalfUp(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    res.setPartialName(s"${this.getName()}_halfUp", true)
//    val fracOr = this.raw.takeLow(n-1).orR
    val fracMSB = this.raw(n-1)
    val addValue = SInt(2 bit)
    when(fracMSB) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(n).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundHalfDown(): Fix = roundHalfDown(this.fracWidth)
  def roundHalfDown(n: Int): Fix = {
    assert(n < this.numericWidth-1, f"Cannot roundHalfDown(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    res.setPartialName(s"${this.getName()}_halfDown", true)
    val fracOr = this.raw.takeLow(n-1).orR
    val fracMSB = this.raw(n-1)
    val addValue = SInt(2 bit)
    when(fracMSB && fracOr) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(n).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundHalfToZero(): Fix = roundHalfToZero(this.fracWidth)
  def roundHalfToZero(n: Int): Fix = {
    assert(n < this.numericWidth-1, f"Cannot roundHalfToZero(${n}) because numeric width is only ${this.numericWidth}")
    if (this.signed) {
      val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
      res.setPartialName(s"${this.getName()}_halfZero", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        when (fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      roundHalfDown(n).setPartialName(s"${this.getName()}_halfZero", true)
    }
  }

  def roundHalfToInf(): Fix = roundHalfToInf(this.fracWidth)
  def roundHalfToInf(n: Int): Fix = {
    assert(n < this.numericWidth-1, f"Cannot roundHalfToInf(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    if (signed) {
      res.setPartialName(s"${this.getName()}_halfInf", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      roundHalfUp(n).setPartialName(s"${this.getName()}_halfInf", true)
    }
  }

  def roundHalfToEven(): Fix = roundHalfToEven(this.fracWidth)
  def roundHalfToEven(n: Int): Fix = {
    assert(n < this.numericWidth-2, f"Cannot roundHalfToEven(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    if (signed) {
      res.setPartialName(s"${this.getName()}_halfEven", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val intLSB = this.raw(n)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        // positive
        when(!intLSB) {
          // even
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // odd
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      } otherwise {
        // negative
        when(!intLSB) {
          // even
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // odd
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      res.setPartialName(s"${this.getName()}_halfEven", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val intLSB = this.raw(n)
      val addValue = UInt(1 bit)
      when(!intLSB) {
        // even
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        // odd
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(n).asUInt + addValue).asBits
      res
    }
  }

  def roundHalfToOdd(): Fix = roundHalfToOdd(this.fracWidth)
  def roundHalfToOdd(n: Int): Fix = {
    assert(n < this.numericWidth-2, f"Cannot roundHalfToOdd(${n}) because numeric width is only ${this.numericWidth}")
    val res = new Fix(intWidth - Math.max(0, n-this.fracWidth), bitWidth - n, this.signed)
    if (signed) {
      res.setPartialName(s"${this.getName()}_halfOdd", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val intLSB = this.raw(n)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        // positive
        when(intLSB) {
          // odd
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // even
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      } otherwise {
        // negative
        when(intLSB) {
          // odd
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // even
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      }
      res.raw := (this.raw.dropLow(n).asSInt + addValue).asBits
      res
    } else {
      res.setPartialName(s"${this.getName()}_halfOdd", true)
      val fracOr = this.raw.takeLow(n-1).orR
      val fracMSB = this.raw(n-1)
      val intLSB = this.raw(n)
      val addValue = UInt(1 bit)
      when(intLSB) {
        // odd
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        // even
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(n).asUInt + addValue).asBits
      res
    }
  }

  override def roundUp(n: Int, align: Boolean): Fix = ???

  override def roundDown(n: Int, align: Boolean): Fix = ???

  override def roundToZero(n: Int, align: Boolean): Fix = ???

  override def roundToInf(n: Int, align: Boolean): Fix = ???

  override def round(n: Int, align: Boolean): Fix = ???

  /** Expands the fixed-point number by N whole bits or F fractional bits */
  def expand(n: Int, f: Int = 0): Fix = {
    assert(n >= 0, "Integer expansion amount must not be negative!")
    assert(f >= 0, "Fractional expansion amount must not be negative!")
    val res = new Fix(this.intWidth + n, this.bitWidth + n + f, this.signed)
    res.setPartialName(s"${this.getName()}_expand", true)
    if (this.signed) {
      res.raw := (this.raw.asSInt.resize(this.bitWidth+n).asBits) << f
    } else {
      res.raw := (this.raw.asUInt.resize(this.bitWidth+n).asBits) << f
    }
    res
  }

  def truncated: this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTags(this.getTags())
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }

  def :=(that: Bits): Unit = {
    this.raw := that
  }

  def :=(that: SInt): Unit = {
    if (signed) {
      this.raw := that.asBits
    } else {
      when(that.msb) {
        this.raw.clearAll()
      } otherwise {
        this.raw := that.asBits
      }
    }
  }

  def :=(that: UInt): Unit = {
    if (signed) {
      if (that.getWidth > this.numericWidth) {
        SpinalError(s"$this can't be assigned by $that because the width of the UInt is larger than the numeric width of the Fixed-point.")
      }
      this.raw.takeLow(that.getWidth) := that.asBits
      this.raw.dropLow(that.getWidth).clearAll()
    } else {
      this.raw := that.asBits
    }
  }

  def :=(that: SFix): Unit = this := Fix(that)
  def :=(that: UFix): Unit = this := Fix(that)

  override def clone: this.type = new Fix(intWidth, bitWidth, signed).asInstanceOf[this.type]

  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case f: Fix =>
        if (f.fracWidth > this.fracWidth && !f.hasTag(tagTruncated)) {
          val trace = ScalaLocated.long
          globalData.pendingErrors += (() => s"$this can't be assigned by $f because the value will need to be truncated.\n$trace")
        }
        if (f.wholeWidth > this.wholeWidth && !(f.hasTag(tagSaturate) || f.hasTag(tagAutoResize))) {
          val trace = ScalaLocated.long
          globalData.pendingErrors += (() => s"$this whole value width is less than $f and $f is not tagged for auto-saturation or resize\n$trace")
        }
        // Remove tags
        val t = f.clone.removeTags(f.getTags())
        t.raw := f.raw
        // Align the integer portion LSBs
        var aligned: Fix = f
        if (f.fracWidth > this.fracWidth) {
          // TODO: Add rounding mode tag?
          aligned = f.trim(f.fracWidth-this.fracWidth)
        } else if (f.fracWidth < this.fracWidth) {
          aligned = f.expand(0, this.fracWidth-f.fracWidth)
        }
        // Resize the integer portion
        var resized: Fix = aligned
        if (f.wholeWidth > this.wholeWidth) {
          if (f.hasTag(tagSaturate)) {
            resized = aligned.sat(f.wholeWidth-this.wholeWidth)
          } else {
            resized = this.clone
            resized.setName(s"${this.getName()}_resized")
            resized.raw := aligned.raw.takeHigh(this.signWidth) ## aligned.raw.takeLow(this.numericWidth)
          }
        } else if (f.wholeWidth < this.wholeWidth) {
          resized = aligned.expand(this.wholeWidth-f.wholeWidth)
        }
        this.raw := resized.raw
      case b: Bits => this := b
      case s: SInt => this := s
      case u: UInt => this := u
      case _ => SpinalError("Undefined assignment")
    }
  }

  override def toString: String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[signed=${if (this.signed) "1" else "0"},whole=${this.wholeWidth},frac=${this.fracWidth}]"
}

object AFix {

  def apply(f: Fix): AFix = {
    val maxValue = BigInt(2).pow(f.bitWidth-1)-1
    val minvalue = -BigInt(2).pow(f.bitWidth-1)
    val ret = new AFix(maxValue, minvalue, -f.fracWidth exp)
    ret.raw := f.raw
    ret
  }

  def apply(u: UInt): AFix = AFix(u, 0 exp)
  def apply(u: UInt, exp: ExpNumber): AFix = {
    val maxValue = BigInt(2).pow(u.getWidth)-1
    val ret = new AFix(maxValue, 0, exp)
    ret.raw := u.asBits
    ret
  }

  def apply(s: SInt): AFix = AFix(s, 0 exp)
  def apply(s: SInt, exp: ExpNumber): AFix = {
    val maxValue = BigInt(2).pow(s.getWidth-1)-1
    val minValue = -BigInt(2).pow(s.getWidth-1)
    val ret = new AFix(maxValue, minValue, exp)
    ret.raw := s.asBits
    ret
  }

  def apply(uf: UFix): AFix = {
    val maxValue = BigInt(2).pow(uf.bitCount)-1
    val ret = new AFix(maxValue, 0, -uf.minExp exp)
    ret.raw := uf.raw.asBits
    ret
  }

  def apply(sf: SFix): AFix = {
    val maxValue = BigInt(2).pow(sf.bitCount-1)-1
    val minValue = -BigInt(2).pow(sf.bitCount-1)
    val ret = new AFix(maxValue, minValue, -sf.minExp exp)
    ret.raw := sf.raw.asBits
    ret
  }

  def apply(num: BigInt, exp: ExpNumber): AFix = {
    val ret = new AFix(num, num, exp)
    if (num >= 0)
      ret.raw := BigIntToUInt(num).asBits
    else
      ret.raw := BigIntToSInt(num).asBits
    ret
  }

  def apply(wholeBits: BitCount, fracBits: BitCount, signed: Boolean): AFix = AFix(wholeBits.value, fracBits.value, signed)
  def apply(wholeBits: Int, fracBits: Int, signed: Boolean): AFix = {
    val signedBit = if (signed) 1 else 0
    val maxValue = BigInt(2).pow(wholeBits+fracBits-signedBit)-1
    val minValue = if (signed) -BigInt(2).pow(wholeBits+fracBits-signedBit) else BigInt(0)
    new AFix(maxValue, minValue, -fracBits exp)
  }

}

class AFix(val maxValue: BigInt, val minValue: BigInt, val exp: ExpNumber) extends MultiData {
//  assert((maxValue*BigDecimal(2).pow(-exp.value)).isWhole,
//    s"maxValue ${maxValue} is not representable with exponent 2^${exp.value}")
//  assert((minValue*BigDecimal(2).pow(-exp.value)).isWhole,
//    s"maxValue ${minValue} is not representable with exponent 2^${exp.value}")

  // TODO?: Support dropping sign bit iff max and min have the same sign?
  private val needsSign = (maxValue < 0) ^ (minValue < 0)

  val signed = (maxValue < 0) || (minValue < 0)
  val signWidth = if (signed) 1 else 0

  private val maxShifted = maxValue.abs - (if (maxValue < 0) signWidth else 0)
  private val maxBits = maxShifted.bitLength
  private val minShifted = minValue.abs - (if (minValue < 0) signWidth else 0)
  private val minBits = minShifted.bitLength

  val bitWidth = Math.max(maxBits, minBits) + signWidth
  val fracWidth = Math.min(exp.value, 0)
  val wholeWidth = bitWidth - fracWidth - signWidth
  val intWidth = bitWidth - fracWidth

  val raw: Bits = Bits(bitWidth bit)

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  // These are used for reference tracking
  private var parents: Seq[AFix] = Nil
  private val children: mutable.Set[AFix] = mutable.Set()

  private def dependsOn(parents: AFix*): Unit = {
    this.parents = parents
    for (p <- parents) {
      p.children += this
    }
  }

  private def alignRanges(l: AFix, r: AFix): (BigInt, BigInt, BigInt, BigInt) = {
    val expDiff = l.exp.value - r.exp.value
    if (expDiff >= 0) {
      (l.maxValue*BigInt(2).pow(expDiff), l.minValue*BigInt(2).pow(expDiff), r.maxValue, r.minValue)
    } else {
      (l.maxValue, l.minValue, r.maxValue*BigInt(2).pow(-expDiff), r.minValue*BigInt(2).pow(-expDiff))
    }
  }

  private def alignLR(l: AFix, r: AFix): (Bits, Bits) = {
    val expDiff = l.exp.value - r.exp.value
    if (expDiff >= 0) {
      (l.raw << expDiff, r.raw)
    } else {
      (l.raw, r.raw << -expDiff)
    }
  }

  private def trim(b: Bits, w: Int): Bits = {
    if (b.getWidth > w) {
      b.takeLow(w)
    } else {
      b
    }
  }

  def +(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax+rMax, lMin+rMin, Math.min(this.exp.value, right.exp.value) exp)
    ret dependsOn (this, right)

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) + _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt             + _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) + _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) + _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  def -(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax-rMin, lMin-rMax, Math.min(this.exp.value, right.exp.value) exp)
    ret dependsOn (this, right)

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) - _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt - _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) - _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) - _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  def *(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = (this.maxValue, this.minValue, right.maxValue, right.minValue)
    val possibleLimits = List(lMax*rMax, lMax*rMin, lMin*rMax, lMin*rMin)
    val ret = new AFix(possibleLimits.max, possibleLimits.min, this.exp.value + right.exp.value exp)
    ret dependsOn (this, right)

    val _l = this.raw
    val _r = right.raw
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) * _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt             * _r.asSInt.resize(ret.bitWidth)).resize(ret.bitWidth)
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) * _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) * _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  def /(right: AFix): AFix = {
    SpinalWarning("Fixed-point division is not finalized and not recommended for use!\n" + ScalaLocated.long)
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp.value + right.exp.value exp)
    ret dependsOn (this, right)

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt          / _r.asUInt).resize(ret.bitWidth)
      case (false,  true) => (_l.asUInt.intoSInt / _r.asSInt).resize(ret.bitWidth)
      case ( true, false) => (_l.asSInt          / _r.asUInt.intoSInt).resize(ret.bitWidth)
      case ( true,  true) => (_l.asSInt          / _r.asSInt).resize(ret.bitWidth)
    }).asBits, ret.bitWidth)

    ret
  }

  def %(right: AFix): AFix = {
    SpinalWarning("Fixed-point modulo is not finalized and not recommended for use!\n" + ScalaLocated.long)
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp.value + right.exp.value exp)
    ret dependsOn (this, right)

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt          % _r.asUInt).resize(ret.bitWidth)
      case (false,  true) => (_l.asUInt.intoSInt % _r.asSInt).resize(ret.bitWidth)
      case ( true, false) => (_l.asSInt          % _r.asUInt.intoSInt).resize(ret.bitWidth)
      case ( true,  true) => (_l.asSInt          % _r.asSInt).resize(ret.bitWidth)
    }).asBits, ret.bitWidth)

    ret
  }

  def ==(right: AFix): Bool = this === right
  def ===(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMin > rMax || lMax < rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        === _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized === _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        === _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        === _r.asSInt.resized)
      }
    }
  }

  def !=(right: AFix): Bool = this =/= right
  def =/=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMin > rMax || lMax < rMin) {
      True
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        =/= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized =/= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        =/= _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        =/= _r.asSInt.resized)
      }
    }
  }

  def <(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMax < rMin) {
      True
    } else if (lMin >= rMax) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        < _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized < _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        < _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        < _r.asSInt.resized)
      }
    }
  }

  def <=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMax <= rMin) {
      True
    } else if (lMin > rMax) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        <= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized <= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        <= _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        <= _r.asSInt.resized)
      }
    }
  }

  def >(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMin > rMax) {
      True
    } else if (lMax <= rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        > _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized > _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        > _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        > _r.asSInt.resized)
      }
    }
  }

  def >=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    this dependsOn right

    if (lMin >= rMax) {
      True
    } else if (lMax < rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized        >= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.asSInt.resized >= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized        >= _r.asUInt.asSInt.resized)
        case ( true,  true) => (_l.asSInt.resized        >= _r.asSInt.resized)
      }
    }
  }

  def <<(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue * shiftBig, this.minValue * shiftBig, (this.exp.value + shift) exp)
    ret dependsOn this

    ret.raw := this.raw << shift

    ret
  }

  def >>(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue / shiftBig, this.minValue / shiftBig, (this.exp.value - shift) exp)
    ret dependsOn this

    ret.raw := this.raw >> shift

    ret
  }

  def unary_-(): AFix = negate()

  def negate(): AFix = {
    val ret = new AFix(-this.maxValue, -this.minValue, this.exp)
    ret dependsOn this

    if (this.signed) {
      when (!this.raw.msb) {
        ret.raw := ((~this.raw).asUInt+1).resize(ret.bitWidth).asBits
      } otherwise {
        ret.raw := (~(this.raw.asUInt-1)).resize(ret.bitWidth).asBits
      }
    } else {
      ret.raw := ((~this.raw).asUInt+1).resize(ret.bitWidth).asBits
    }

    ret
  }

  def sat(satMax: BigInt, satMin: BigInt): AFix = {
    val ret = new AFix(satMax, satMin, exp)
    when (this > AFix(satMax, exp)) {
      if (this.signed)
        ret.raw := BigIntToSInt(satMax).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(satMax).resize(ret.bitWidth).asBits
    } elsewhen (this < AFix(satMin, exp)) {
      if (this.signed)
        ret.raw := BigIntToSInt(satMin).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(satMin).resize(ret.bitWidth).asBits
    } otherwise {
      if (this.signed)
        ret.raw := this.raw.asSInt.resize(ret.bitWidth).asBits
      else
        ret.raw := this.raw.asUInt.resize(ret.bitWidth).asBits
    }
    ret
  }

  def floor(): AFix = {
    assert(this.exp.value < 0, f"Cannot floor() because number does not have enough fractional bits, needs at least -1 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    res.raw := this.raw.dropLow(-this.exp.value)
    res
  }

  def ceil(): AFix = {
    assert(this.exp.value < 0, f"Cannot ceil() because number does not have enough fractional bits, needs at least -1 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    val fracOr = this.raw.takeLow(-this.exp.value).orR
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + (False ## fracOr).asSInt).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + fracOr.asUInt).asBits
    }
    res
  }

  def floorToZero(): AFix = {
    assert(this.exp.value < 0, f"Cannot floorToZero() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
      res dependsOn this
      val fracOr = this.raw.takeLow(-this.exp.value).orR
      val addValue = SInt(2 bit)
      when(this.raw.msb && fracOr) {
        addValue := 1
      } otherwise {
        addValue := 0
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
      res
    } else {
      floor()
    }
  }

  def ceilToInf(): AFix = {
    assert(this.exp.value < 0, f"Cannot ceilToInf() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
      res dependsOn this
      val fracOr = this.raw.takeLow(-this.exp.value).orR
      val addValue = SInt(2 bit)
      when(fracOr) {
        when(!this.raw.msb) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        addValue := 0
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
      res
    } else {
      ceil()
    }
  }

  def roundHalfUp(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfUp() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    val fracMSB = this.raw(-this.exp.value-1)
    val addValue = SInt(2 bit)
    when(fracMSB) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundHalfDown(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfDown() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    val fracOr = this.raw.takeLow(-this.exp.value-1).orR
    val fracMSB = this.raw(-this.exp.value-1)
    val addValue = SInt(2 bit)
    when(fracMSB && fracOr) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundHalfToZero(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToZero() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
      res dependsOn this
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        when (fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
      res
    } else {
      roundHalfDown()
    }
  }

  def roundHalfToInf(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToInf() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
      res dependsOn this
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        when (fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
      res
    } else {
      roundHalfDown()
    }
  }

  def roundHalfToEven(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToEven() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    if (this.signed) {
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val intLSB = this.raw(-this.exp.value)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        // positive
        when(!intLSB) {
          // even
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // odd
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      } otherwise {
        // negative
        when(!intLSB) {
          // even
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // odd
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
    } else {
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val intLSB = this.raw(-this.exp.value)
      val addValue = UInt(1 bit)
      when(!intLSB) {
        // even
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        // odd
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + addValue).asBits
    }
    res
  }

  def roundHalfToOdd(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToOdd() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)
    res dependsOn this
    if (this.signed) {
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val intLSB = this.raw(-this.exp.value)
      val addValue = SInt(2 bit)
      when(!this.raw.msb) {
        // positive
        when(intLSB) {
          // odd
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // even
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      } otherwise {
        // negative
        when(intLSB) {
          // odd
          when(fracMSB && fracOr) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        } otherwise {
          // even
          when(fracMSB) {
            addValue := 1
          } otherwise {
            addValue := 0
          }
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + addValue).asBits
    } else {
      val fracOr = this.raw.takeLow(-this.exp.value-1).orR
      val fracMSB = this.raw(-this.exp.value-1)
      val intLSB = this.raw(-this.exp.value)
      val addValue = UInt(1 bit)
      when(intLSB) {
        // odd
        when(fracMSB && fracOr) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      } otherwise {
        // even
        when(fracMSB) {
          addValue := 1
        } otherwise {
          addValue := 0
        }
      }
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + addValue).asBits
    }
    res
  }

  override def toString: String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[max=${maxValue}, min=${minValue}, exp=${exp}, bits=${raw.getWidth}]"

  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case af: AFix =>
        var af_rounded: AFix = af
        if (af.exp.value < this.exp.value) {
          val exp_diff = af.exp.value - this.exp.value
          if (exp_diff > 1)
            af_rounded = (af << this.exp.value).roundHalfDown() >> this.exp.value
          else
            af_rounded = (af << this.exp.value).floor() >> this.exp.value
        } else if (af.exp.value > this.exp.value) {
          val exp_diff = af.exp.value - this.exp.value
          af_rounded = af << exp_diff
        }

        if (this.signed)
          this.raw := af_rounded.raw.asSInt.resize(this.bitWidth).asBits
        else
          this.raw := af_rounded.raw.asUInt.resize(this.bitWidth).asBits
      case f: Fix => this := AFix(f)
      case u: UInt => this := AFix(u)
      case s: SInt => this := AFix(s)
      case uf: UFix => this := AFix(uf)
      case sf: SFix => this := AFix(sf)
    }
  }

  def asUInt(): UInt = {
    if (this.signed) {
      val out = UInt(this.bitWidth bit)
      this.raw.asSInt.asUInt
      when(this.raw.msb) {
        out := U(0).resize(this.bitWidth)
      } otherwise {
        out := this.raw.asUInt
      }
      out
    } else {
      this.raw.asUInt
    }
  }

  def asSInt(): SInt = {
    if (this.signed) {
      this.raw.asSInt
    } else {
      this.raw.asUInt.intoSInt
    }
  }

  def asUFix(): UFix = this.asUInt().toUFix >> -this.exp.value
  def asSFix(): SFix = this.asSInt().toSFix >> -this.exp.value

  override def clone: this.type = new AFix(maxValue, minValue, exp).asInstanceOf[this.type]
}


trait SFixFactory extends TypeFactory{
  def SFix(peak: ExpNumber, width: BitCount): SFix = postTypeFactory(new SFix(peak.value, width.value))
  def SFix(peak: ExpNumber, resolution: ExpNumber): SFix = postTypeFactory(new SFix(peak.value, 1 + peak.value - resolution.value))
}


trait UFixFactory extends TypeFactory{
  def UFix(peak: ExpNumber, width: BitCount): UFix = postTypeFactory(new UFix(peak.value, width.value))
  def UFix(peak: ExpNumber, resolution: ExpNumber): UFix = postTypeFactory(new UFix(peak.value, peak.value - resolution.value))
}


trait SFixCast {
  @deprecated("Use xxx.toSFix instead", "???")
  def toSFix(sint: SInt) = sint.toSFix
}


trait UFixCast {
  @deprecated("Use xxx.toUFix instead", "???")
  def toUFix(uint: UInt): UFix = uint.toUFix
}


/**
  * Base class for SFix and UFix
  */
abstract class XFix[T <: XFix[T, R], R <: BitVector with Num[R]](val maxExp: Int, val bitCount: Int) extends MultiData {

  require(bitCount >= 0)

  val raw = rawFactory(maxExp, bitCount)

  def minExp: Int

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  def rawFactory(exp: Int, bitCount: Int): R
  def fixFactory(exp: Int, bitCount: Int): T

  def difLsb(that: T) = (this.maxExp - this.bitCount) - (that.maxExp - that.bitCount)

  def resolution : BigDecimal

  def alignLsb(that: T): (R, R) = {
    val lsbDif   = difLsb(that)
    val left: R  = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: R = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  def doAddSub(that: T, sub: Boolean): T = {
    val (rawLeft, rawRight) = alignLsb(that)
    val ret = fixFactory(Math.max(this.maxExp, that.maxExp), Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth))
    ret.raw := (if (sub) rawLeft - rawRight else rawLeft + rawRight)
    ret
  }

  def doSmaller(that: T): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

  def doSmallerEguals(that: T): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft <= rawRight
  }

  def doShiftLeft(that: Int): T = {
    val ret = fixFactory(maxExp + that, bitCount)
    ret.raw := this.raw
    ret
  }

  def doShiftRight(that: Int): T = {
    val ret = fixFactory(maxExp - that, bitCount)
    ret.raw := this.raw
    ret
  }

  def doShiftLeftBorned(that: Int): T = {
    val ret = fixFactory(maxExp + that, bitCount + that)
    ret.raw := this.raw << that
    ret
  }

  def doShiftRightBorned(that: Int): T = {
    val ret = fixFactory(maxExp - that, bitCount - that)
    ret.raw := this.raw >> that
    ret
  }

  override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  def truncated: this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }

  override private[spinal] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) =>
        val t = that.asInstanceOf[T]
        if(this.maxExp < t.maxExp || this.minExp > t.minExp){
          if(!t.hasTag(tagTruncated)){
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => s"$this can't be assigned by $t because of truncation. You can do x := y.truncated if that's fine.\n $trace")
          }
        }
        val difLsb = this.difLsb(t)
        if (difLsb > 0)
          this.raw compositAssignFrom ((t.raw >> difLsb).resized, this.raw, kind)
        else if (difLsb < 0)
          this.raw compositAssignFrom ( (t.raw << -difLsb).resized, this.raw, kind)
        else
          this.raw compositAssignFrom ( t.raw.resized, this.raw, kind)
      case _ => SpinalError("Undefined assignment")
    }
  }
}

//TODO Fix autoconnect
/**
  * Signed fix point
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Fix SFix Documentation]]
  */
class SFix(maxExp: Int, bitCount: Int) extends XFix[SFix, SInt](maxExp, bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): SInt = SInt(bitCount bit)

  override def fixFactory(maxExp: Int, bitCount: Int): SFix = SFix(maxExp exp, bitCount bit)

  override def minExp: Int = maxExp - bitCount + 1

  def +(that: SFix): SFix = doAddSub(that, sub = false)
  def -(that: SFix): SFix = doAddSub(that, sub = true)
  def *(that: SFix): SFix = {
    val ret = fixFactory(this.maxExp + that.maxExp + 1, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def << (that: Int): SFix  = doShiftLeft(that)
  def >> (that: Int): SFix  = doShiftRight(that)
  def <<|(that: Int): SFix  = doShiftLeftBorned(that)
  def >>|(that: Int): SFix  = doShiftRightBorned(that)
  def <  (that: SFix): Bool = doSmaller(that)
  def >  (that: SFix): Bool = that.doSmaller(this)
  def <= (that: SFix): Bool = doSmallerEguals(that)
  def >= (that: SFix): Bool = that.doSmallerEguals(this)

  def >= (that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def >(that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this > other
  }

  def <(that: BigDecimal): Bool = {
    if (that < minValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this < other
  }

  def <=(that: BigDecimal): Bool = {
    if (that < minValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this <= other
  }

  def :=(that: Int): Unit   = this := BigInt(that)
  def :=(that: Long): Unit  = this := BigInt(that)
  def :=(that: Float): Unit = this := BigDecimal(that.toDouble)

  def :=(that: BigDecimal): Unit = {
    assert(that <= this.maxValue, s"Literal $that is too big to be assigned in $this")
    assert(that >= this.minValue, s"Literal $that is too negative to be assigned in this $this")

    val shift = bitCount - maxExp - 1
    val value = if(shift >= 0)
      (that * BigDecimal(BigInt(1) << shift)).toBigInt
    else
      (that / BigDecimal(BigInt(1) << -shift)).toBigInt
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    assert(BigDecimal(that) <= this.maxValue, s"Literal $that is too big to be assigned in $this")
    assert(BigDecimal(that)  >= this.minValue, s"Literal $that is too negative to be assigned in this $this")

    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  // Rounded down
  def toSInt: SInt = {
    if (maxExp < 0)
      S(0)
    else {
      if (minExp == 0)
        raw
      else if (minExp > 0)
        raw << minExp
      else
        raw >> (-minExp)
    }
  }

  //TODO Should update the calculation with BigDecimal
  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - BigDecimal(1.0 / math.pow(2.0, bitCount - 1)))
  def minValue: BigDecimal = -Math.pow(2.0, maxExp)
  override def resolution: BigDecimal = Math.pow(2.0, maxExp-bitCount+1)

  override def clone: this.type = new SFix(maxExp, bitCount).asInstanceOf[this.type]

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[peak=2^$maxExp resolution=2^$minExp]"
}


//@valClone
class SFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {

  val x = SFix(maxExp exp, bitCount bit)
  val y = SFix(maxExp exp, bitCount bit)

  def truncated : this.type = {
    val copy = clone()
    copy.x := this.x
    copy.y := this.y
    copy.x.addTag(tagTruncated)
    copy.y.addTag(tagTruncated)
    copy
  }

  override def clone: this.type = new SFix2D(maxExp, bitCount).asInstanceOf[this.type]
}

/**
  * Unsigned fix point
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Fix UFix Documentation]]
  */
class UFix(maxExp: Int, bitCount: Int) extends XFix[UFix, UInt](maxExp, bitCount) {

  override def rawFactory(maxExp: Int, bitCount: Int): UInt = UInt(bitCount bit)
  override def fixFactory(maxExp: Int, bitCount: Int): UFix = UFix(maxExp exp, bitCount bit)
  override def minExp: Int = maxExp - bitCount

  def +(that: UFix): UFix = doAddSub(that, sub = false)
  def -(that: UFix): UFix = doAddSub(that, sub = true)
  def *(that: UFix): UFix = {
    val ret = fixFactory(this.maxExp + that.maxExp, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def << (that: Int): UFix  = doShiftLeft(that)
  def >> (that: Int): UFix  = doShiftRight(that)
  def <<|(that: Int): UFix  = doShiftLeftBorned(that)
  def >>|(that: Int): UFix  = doShiftRightBorned(that)
  def <  (that: UFix): Bool = doSmaller(that)
  def >  (that: UFix): Bool = that.doSmaller(this)
  def <= (that: UFix): Bool = doSmallerEguals(that)
  def >= (that: UFix): Bool = that.doSmallerEguals(this)

  def >=(that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def :=(that: Int): Unit   = this := BigInt(that)
  def :=(that: Long): Unit  = this := BigInt(that)
  def :=(that: Float): Unit = this := BigDecimal(that.toDouble)

  def :=(that: BigDecimal): Unit = {
    assert(that >= 0)
    assert(that <= this.maxValue, s"Literal $that is too big to be assigned in this $this")

    val shift = bitCount - maxExp
    val value = if(shift >= 0)
      (that * BigDecimal(BigInt(1) << shift)).toBigInt
    else
      (that / BigDecimal(BigInt(1) << -shift)).toBigInt
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    assert(that >= 0)
    assert(that < (BigInt(1) << maxExp), s"Literal $that is too big to be assigned in this $this")

    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  //Rounded down
  def toUInt: UInt = {
    if (maxExp < 0)
      U(0)
    else {
      if (minExp == 0)
        raw
      else if (minExp > 0)
        raw << minExp
      else
        raw >> (-minExp)
    }
  }

  def fractionalPart: UFix = {
    assert(this.bitCount - this.maxExp > 0)
    val ret = UFix(0 exp, -minExp bit)
    ret := this.resized
    ret
  }

  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - 1.0 / math.pow(2.0, bitCount))
  def minValue: BigDecimal = 0.0

  override def resolution: BigDecimal = Math.pow(2.0, maxExp - bitCount)

  override def clone: this.type = new UFix(maxExp, bitCount).asInstanceOf[this.type]
}


/**
  * Two-dimensional XFix
  */
class UFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = UFix(maxExp exp, bitCount bit)
  val y = UFix(maxExp exp, bitCount bit)

  def truncated : this.type = {
    val copy = clone()
    copy.x := this.x
    copy.y := this.y
    copy.x.addTag(tagTruncated)
    copy.y.addTag(tagTruncated)
    copy
  }

  override def clone: UFix2D.this.type = new UFix2D(maxExp, bitCount).asInstanceOf[this.type]
}


/**
  * Two-dimensional SFix
  */
object SFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): SFix2D = new SFix2D(maxExp.value, bitCount.value)

  def apply(copy: SFix): SFix2D = SFix2D(copy.maxExp exp, copy.bitCount bit)
}


/**
  * Two-dimensional UFix
  */
object UFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): UFix2D = new UFix2D(maxExp.value, bitCount.value)

  def apply(copy: UFix): UFix2D = UFix2D(copy.maxExp exp, copy.bitCount bit)
}


object SF{
  def apply(value: BigDecimal, peak: ExpNumber, width: BitCount): SFix = {
    val tmp = SFix(peak, width)
    tmp := value
    tmp
  }

  def apply(value: BigDecimal, peak: ExpNumber, resolution: ExpNumber): SFix = {
    val tmp = SFix(peak, resolution)
    tmp := value
    tmp
  }
}


object UF{
  def apply(value: BigDecimal, peak: ExpNumber, width: BitCount): UFix = {
    val tmp = UFix(peak, width)
    tmp := value
    tmp
  }

  def apply(value: BigDecimal, peak: ExpNumber, resolution: ExpNumber): UFix = {
    val tmp = UFix(peak, resolution)
    tmp := value
    tmp
  }
}
