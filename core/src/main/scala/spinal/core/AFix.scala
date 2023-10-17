package spinal.core

import spinal.idslplugin.Location

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

object AFix {

  def apply(maxRaw: BigInt, minRaw: BigInt, exp: ExpNumber) : AFix = new AFix(maxRaw = maxRaw, minRaw = minRaw, exp = exp.value)
  def apply(u: UInt): AFix = AFix(u, 0 exp)
  def apply(u: UInt, exp: ExpNumber): AFix = {
    val maxValue = BigInt(2).pow(u.getWidth)-1
    val ret = new AFix(maxValue, 0, exp.value)
    ret.raw := u.asBits
    ret
  }
  def apply(u: UInt, maxRaw: BigInt, exp : ExpNumber): AFix = {
    val ret = new AFix(maxRaw, 0, exp.value)
    ret.raw := u.asBits.resized
    ret
  }

  def apply(b: Bool): AFix = this(b, 0 exp)
  def apply(b: Bool, exp : ExpNumber): AFix = {
    val ret = new AFix(1, 0, exp.value)
    ret.raw(0) := b
    ret
  }

  def apply(s: SInt): AFix = AFix(s, 0 exp)
  def apply(s: SInt, exp: ExpNumber): AFix = {
    val maxValue = BigInt(2).pow(s.getWidth-1)-1
    val minValue = -BigInt(2).pow(s.getWidth-1)
    val ret = new AFix(maxValue, minValue, exp.value)
    ret.raw := s.asBits
    ret
  }

  def apply(uf: UFix): AFix = {
    val maxValue = BigInt(2).pow(uf.bitCount)-1
    val ret = new AFix(maxValue, 0, -uf.minExp)
    ret.raw := uf.raw.asBits
    ret
  }

  def apply(sf: SFix): AFix = {
    val maxRaw = BigInt(2).pow(sf.bitCount-1)-1
    val minRaw = -BigInt(2).pow(sf.bitCount-1)
    val ret = new AFix(maxRaw, minRaw, -sf.minExp)
    ret.raw := sf.raw.asBits
    ret
  }

  def apply(num: BigInt, exp: ExpNumber): AFix = {
    val ret = new AFix(num, num, exp.value)
    if (num >= 0)
      ret.raw := BigIntToUInt(num).asBits
    else
      ret.raw := BigIntToSInt(num).asBits
    ret
  }

  def apply(num: BigInt): AFix = apply(num, 0 exp)


  def apply(amplitude : ExpNumber, resolution : ExpNumber, signed : Boolean) : AFix = {
    val maxRaw = BigInt(2).pow(amplitude.value-resolution.value)-1
    val minRaw = if (signed) -BigInt(2).pow(amplitude.value-resolution.value) else BigInt(0)
    new AFix(maxRaw, minRaw, resolution.value)
  }

  def U(width: BitCount): AFix = AFix(width.value exp, 0 exp, signed = false)
  def UQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AFix(integerWidth.value exp, -fractionWidth.value exp, signed = false)
  def U(amplitude: ExpNumber, width: BitCount): AFix = AFix(amplitude, (amplitude.value - width.value) exp, false)
  def U(amplitude: ExpNumber, resolution: ExpNumber): AFix = AFix(amplitude, resolution, false)
//  def U(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits, -exp bit, false)
//  def U(maximum: BigInt, resolution: ExpNumber): AFix = {
//    assert(maximum >= 0, s"AFix.U maxRaw must be non-negative! (${maximum} is not >= 0)")
//    new AFix(maximum*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)-1), 0, resolution)
//  }
//  def U(maximum: BigInt, minimum: BigInt, resolution: ExpNumber): AFix = {
//    assert(maximum >= 0, s"AFix.U maxRaw must be non-negative! (${maximum} is not >= 0)")
//    assert(maximum >= 0, s"AFix.U minRaw must be non-negative! (${minimum} is not >= 0)")
//    new AFix(maximum*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)-1),
//      minimum*BigInt(2).pow(-resolution.value), resolution)
//  }

  def S(width: BitCount): AFix = AFix(width.value-1 exp, 0 exp, signed = true)
  def SQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AFix(integerWidth.value exp, -fractionWidth.value exp, signed = true)
  def S(amplitude: ExpNumber, width: BitCount): AFix = AFix(amplitude, (amplitude.value - width.value + 1) exp, true)
  def S(amplitude: ExpNumber, resolution: ExpNumber): AFix = AFix(amplitude, resolution, signed = true)
//  def S(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits+(1 bit), -exp bit, signed = true)
//  def S(maximum: BigInt, resolution: ExpNumber): AFix =
//    new AFix(maximum.max(0)*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)*maximum.signum-maximum.signum),
//      maximum.min(0)*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)*maximum.signum-maximum.signum), resolution)
//  def S(maxRaw: BigInt, minRaw: BigInt, exp: ExpNumber): AFix =
//    new AFix(maxRaw*BigInt(2).pow(-exp)+(BigInt(2).pow(-exp)*maxRaw.signum-maxRaw.signum),
//      minRaw*BigInt(2).pow(-exp), exp)

  def holding(values: TraversableOnce[AFix]) : AFix = {
    val param = holdingParams(values)
    new AFix(
      maxRaw = param._1,
      minRaw = param._2,
      exp      = param._3
    )
  }


  def holdingParams(values: TraversableOnce[AFix]) : (BigInt, BigInt, Int) = {
    val ex = values.map(_.exp).min
    (
      values.map(e => e.maxRaw << (e.exp - ex).max(0)).max,
      values.map(e => e.minRaw << (e.exp - ex).max(0)).min,
      ex
    )
  }
}

class AFix(val maxRaw: BigInt, val minRaw: BigInt, val exp: Int) extends MultiData with Num[AFix] with BitwiseOp[AFix] with MinMaxDecimalProvider {
  assert(maxRaw >= minRaw)

  val signed = (maxRaw < 0) || (minRaw < 0)
  val signWidth = if (signed) 1 else 0

  lazy val maxValue = BigDecimal(maxRaw) * BigDecimal(2).pow(exp)
  lazy val minValue = BigDecimal(minRaw) * BigDecimal(2).pow(exp)
  lazy val step = BigDecimal(2).pow(exp)

  private val maxShifted = maxRaw.abs - (if (maxRaw < 0) signWidth else 0)
  private val maxBits = maxShifted.bitLength
  private val minShifted = minRaw.abs - (if (minRaw < 0) signWidth else 0)
  private val minBits = minShifted.bitLength

  // Number of bits to represent the entire value
  val bitWidth = Math.max(maxBits, minBits) + signWidth
  // Number of bits to represent the fractional value
  val fracWidth = Math.max(-exp, 0)
  // Number of bits to represent the whole value, no sign
  val wholeWidth = bitWidth - fracWidth - signWidth
  // Number of bits to represent the whole ("integer") value, with sign
  val intWidth = bitWidth - fracWidth
  // Number of bits to represent the numeric value, no sign
  val numWidth = bitWidth - signWidth

  val raw: Bits = Bits(bitWidth bit)

  // Representable range, range which could be represented by the backing bit vector
  private val maxRepr = BigInt(2).pow(numWidth)
  private val minRepr = BigInt(2).pow(numWidth)+1

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def Q: QFormat = QFormat(bitWidth, fracWidth, signed)

  /** This function differs from traditional Num[T] by returning a new AFix */
  override def tag(q: QFormat): AFix = {
    require(q.width == this.bitWidth)
    val res = AFix(q.width-q.fraction exp, -q.fraction exp, q.signed)
    res.raw := this.raw
    res
  }

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  private def alignRanges(l: AFix, r: AFix): (BigInt, BigInt, BigInt, BigInt) = {
    val expDiff = l.exp - r.exp
    // Scale left or right ranges if there's a difference in precision
    if (expDiff > 0) {
      (l.maxRaw*BigInt(2).pow(expDiff),
       l.minRaw*BigInt(2).pow(expDiff),
       r.maxRaw, r.minRaw)
    } else if (expDiff < 0) {
      (l.maxRaw, l.minRaw,
       r.maxRaw*BigInt(2).pow(-expDiff),
       r.minRaw*BigInt(2).pow(-expDiff))
    } else {
      (l.maxRaw, l.minRaw, r.maxRaw, r.minRaw)
    }
  }

  /** Aligns representable ranges of two AFix numbers */
  private def alignRangesRepr(l: AFix, r: AFix): (BigInt, BigInt, BigInt, BigInt) = {
    val expDiff = l.exp - r.exp
    // Scale left or right ranges if there's a difference in precision
    if (expDiff > 0) {
      (l.maxRepr*BigInt(2).pow(expDiff),
        l.minRepr*BigInt(2).pow(expDiff),
        r.maxRepr, r.minRepr)
    } else if (expDiff < 0) {
      (l.maxRepr, l.minRaw,
        r.maxRepr*BigInt(2).pow(-expDiff),
        r.minRepr*BigInt(2).pow(-expDiff))
    } else {
      (l.maxRepr, l.minRepr, r.maxRepr, r.minRepr)
    }
  }

  private def alignLR(l: AFix, r: AFix): (Bits, Bits) = {
    val expDiff = l.exp - r.exp
    // Shift left or right range if there's a difference in precision
    if (expDiff > 0) {
      (l.raw << expDiff, r.raw)
    } else if (expDiff < 0) {
      (l.raw, r.raw << -expDiff)
    } else {
      (l.raw, r.raw)
    }
  }

  private def roundedBounds(exp: Int, roundType: RoundType): (BigInt, BigInt) = {
    val drop = exp-this.exp
    (_roundFixedBigInt(maxRaw, drop, roundType), _roundFixedBigInt(minRaw, drop, roundType))
  }


  private def _roundFixedBigInt(i: BigInt, exp: Int, roundType: RoundType): BigInt = {
    import RoundType._

    val scaled = i / BigInt(2).pow(exp)
    val even = if (exp >= 0) !i.abs.testBit(exp) else false
    val positive = i.signum >= 0

    val halfBit = if (exp > 0) i.abs.testBit(exp-1) else false // 0.5
    val halfDropped = if (exp > 1) ((i.abs & BigInt(2).pow(exp-1)-1) != 0) else false // Bits smaller than 0.5
    val droppedBits = halfBit || halfDropped // 0.5 + smaller

    roundType match {
      case CEIL =>
        if (positive) {
          if (droppedBits) {
            scaled + 1
          } else {
            scaled
          }
        } else {
          scaled
        }
      case FLOOR =>
        if (positive) {
          scaled
        } else {
          if (droppedBits) {
            scaled - 1
          } else {
            scaled
          }
        }
      case FLOORTOZERO =>
        if (positive) {
          _roundFixedBigInt(i, exp, FLOOR)
        } else {
          _roundFixedBigInt(i, exp, CEIL)
        }
      case CEILTOINF =>
        if (positive) {
          _roundFixedBigInt(i ,exp, CEIL)
        } else {
          _roundFixedBigInt(i, exp, FLOOR)
        }
      case ROUNDUP =>
        if (positive) {
          if (halfBit) {
            _roundFixedBigInt(i, exp, CEIL)
          } else {
            _roundFixedBigInt(i, exp, FLOOR)
          }
        } else {
          if (halfBit ^ halfDropped) {
            _roundFixedBigInt(i, exp, CEIL)
          } else {
            _roundFixedBigInt(i, exp, FLOOR)
          }
        }
      case ROUNDDOWN =>
        if (positive) {
          if (halfBit ^ halfDropped) {
            _roundFixedBigInt(i, exp, FLOOR)
          } else {
            _roundFixedBigInt(i, exp, CEIL)
          }
        } else {
          if (halfBit) {
            _roundFixedBigInt(i, exp, FLOOR)
          } else {
            _roundFixedBigInt(i, exp, CEIL)
          }
        }
      case ROUNDTOZERO =>
        if (positive) {
          _roundFixedBigInt(i, exp, ROUNDDOWN)
        } else {
          _roundFixedBigInt(i, exp, ROUNDUP)
        }
      case ROUNDTOINF =>
        if (positive) {
          _roundFixedBigInt(i, exp, ROUNDUP)
        } else {
          _roundFixedBigInt(i, exp, ROUNDDOWN)
        }
      case ROUNDTOEVEN =>
        if (positive) {
          if (even) {
            _roundFixedBigInt(i, exp, ROUNDDOWN)
          } else {
            _roundFixedBigInt(i, exp, ROUNDUP)
          }
        } else {
          if (even) {
            _roundFixedBigInt(i, exp, ROUNDUP)
          } else {
            _roundFixedBigInt(i, exp, ROUNDDOWN)
          }
        }
      case ROUNDTOODD =>
        if (positive) {
          if (even) {
            _roundFixedBigInt(i, exp, ROUNDUP)
          } else {
            _roundFixedBigInt(i, exp, ROUNDDOWN)
          }
        } else {
          if (even) {
            _roundFixedBigInt(i, exp, ROUNDDOWN)
          } else {
            _roundFixedBigInt(i, exp, ROUNDUP)
          }
        }
    }
  }

  private def trim(b: Bits, w: Int): Bits = {
    if (b.getWidth > w) {
      b.takeLow(w)
    } else {
      b
    }
  }

  /**
   * Adds `this` to the right hand side AFix value expanding ranges as necessary
   * @param right Value to add to `this`
   * @return Sum
   */
  def +(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax+rMax, lMin+rMin, Math.min(this.exp, right.exp))

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) + _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt             + _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) + _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) + _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  def +^(right: AFix): AFix = this + right

  /**
   * Adds `this` to the right hand side AFix value without expanding ranges or checks on value overflow
   * @param right Value to add to `this`
   * @return Sum
   */
  def +|(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), Math.min(this.exp, right.exp))

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) + _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt             + _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) + _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) + _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  /**
   * Subtracts `this` to the right hand side AFix value expanding ranges as necessary
   * @param right Value to subtract from `this`
   * @return Difference
   */
  def -(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax-rMin, lMin-rMax, Math.min(this.exp, right.exp))

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) - _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt - _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) - _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) - _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  def -^(right: AFix): AFix = this - right

  /**
   * Subtracts `this` from the right hand side AFix value without expanding ranges or checks on value underflow
   * @param right Value to subtract from `this`
   * @return Difference
   */
  def -|(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMin), lMin.min(rMax), Math.min(this.exp, right.exp))

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resize(ret.bitWidth) - _r.asUInt)
      case (false,  true) => (_l.asUInt.intoSInt - _r.asSInt.resize(ret.bitWidth))
      case ( true, false) => (_l.asSInt.resize(ret.bitWidth) - _r.asUInt.intoSInt)
      case ( true,  true) => (_l.asSInt.resize(ret.bitWidth) - _r.asSInt)
    }).asBits, ret.bitWidth)

    ret
  }

  /**
   * Mutiplies `this` by the right hand side AFix value expanding ranges as necessary
   * @param right Value to multiply `this` by
   * @return Product
   */
  def *(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = (this.maxRaw, this.minRaw, right.maxRaw, right.minRaw)
    val possibleLimits = List(lMax*rMax, lMax*rMin, lMin*rMax, lMin*rMin)
    val ret = new AFix(possibleLimits.max, possibleLimits.min, this.exp + right.exp)

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

  /**
   * Divides `this` by the right hand side AFix value expanding ranges as necessary
   * @param right Value to divide `this` by
   * @return Quotient
   */
  def /(right: AFix): AFix = {
    SpinalWarning("Fixed-point division is not finalized and not recommended for use!\n" + ScalaLocated.long)
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp + right.exp)

    val (_l, _r) = alignLR(this, right)
    ret.raw := trim(((this.signed, right.signed) match {
      case (false, false) => (_l.asUInt          / _r.asUInt).resize(ret.bitWidth)
      case (false,  true) => (_l.asUInt.intoSInt / _r.asSInt).resize(ret.bitWidth)
      case ( true, false) => (_l.asSInt          / _r.asUInt.intoSInt).resize(ret.bitWidth)
      case ( true,  true) => (_l.asSInt          / _r.asSInt).resize(ret.bitWidth)
    }).asBits, ret.bitWidth)

    ret
  }

  /**
   * Divides `this` by the right hand side AFix value expanding ranges as necessary
   * @param right Value to divide `this` by
   * @return Remainder
   */
  def %(right: AFix): AFix = {
    SpinalWarning("Fixed-point modulo is not finalized and not recommended for use!\n" + ScalaLocated.long)
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp + right.exp)

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
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          === _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized === _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          === _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          === _r.asSInt.resized)
    }
  }

  def !=(right: AFix): Bool = this =/= right
  def =/=(right: AFix): Bool = {
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          =/= _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized =/= _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          =/= _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          =/= _r.asSInt.resized)
    }
  }

  def <(right: AFix): Bool = {
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          < _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized < _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          < _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          < _r.asSInt.resized)
    }
  }

  def <=(right: AFix): Bool = {
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          <= _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized <= _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          <= _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          <= _r.asSInt.resized)
    }
  }

  def >(right: AFix): Bool = {
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          > _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized > _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          > _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          > _r.asSInt.resized)
    }
  }

  def >=(right: AFix): Bool = {
    val (_l, _r) = alignLR(this, right)
    (this.signed, right.signed) match {
      case (false, false) => (_l.asUInt.resized          >= _r.asUInt.resized)
      case (false,  true) => (_l.asUInt.intoSInt.resized >= _r.asSInt.resized)
      case ( true, false) => (_l.asSInt.resized          >= _r.asUInt.intoSInt.resized)
      case ( true,  true) => (_l.asSInt.resized          >= _r.asSInt.resized)
    }
  }

  // Shift decimal point left
  def <<(shift: Int): AFix = {
    val ret = new AFix(this.maxRaw, this.minRaw, (this.exp + shift))

    ret.raw := this.raw

    ret
  }

  // Shift decimal point right
  def >>(shift: Int): AFix = {
    val ret = new AFix(this.maxRaw, this.minRaw, (this.exp - shift))

    ret.raw := this.raw

    ret

  }

  // Shift bits and decimal point left, adding padding bits right
  def <<|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxRaw * shiftBig, this.minRaw * shiftBig, (this.exp + shift))

    ret.raw := this.raw << shift

    ret
  }

  // Shift bits and decimal point right, adding padding bits left
  def >>|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxRaw / shiftBig, this.minRaw / shiftBig, this.exp)

    if (this.signed)
      ret.raw := this.raw.dropLow(shift)
    else
      ret.raw := this.raw.dropLow(shift)

    ret
  }

  def >>(shift: AFix): AFix = {
    assert(shift.exp == 0)
    assert(shift.minRaw == 0)
    val ret = new AFix(
      this.maxRaw * (BigInt(1) << shift.maxRaw.toInt),
      this.minRaw * (BigInt(1) << shift.maxRaw.toInt),
      (this.exp - shift.maxRaw.toInt)
    )

    ret.raw := (this.raw << shift.maxRaw.toInt) >> U(shift)

    ret
  }

  //Shift right, lose lsb bits
  def >>|(shift: AFix): AFix = {
    assert(shift.exp == 0)
    assert(shift.minRaw == 0)
    val ret = cloneOf(this)

    ret.raw := this.raw >> U(shift)

    ret
  }

  //Shift left, lose MSB bits
  def |<<(shift: AFix): AFix = {
    assert(shift.exp == 0)
    assert(shift.minRaw == 0)
    val ret = cloneOf(this)

    ret.raw := this.raw |<< U(shift)

    ret
  }


  // Shift bits and decimal point left, loosing bits
  def |<<(shift: Int): AFix = {
    val width = widthOf(raw)-shift
    val ret = new AFix(this.maxRaw.min(BigInt(2).pow(width)-1), this.minRaw.max(-BigInt(2).pow(width)), (this.exp + shift))

    ret.raw := this.raw.resized

    ret
  }

  def unary_-(): AFix = negate(True)

  def negate(): AFix = negate(True)
  def negate(enable : Bool, plusOneEnable : Bool = null): AFix = {
    val ret = new AFix(-this.minRaw max this.maxRaw, -this.maxRaw min this.minRaw, this.exp)
    ret.raw := U(this.raw).twoComplement(enable, plusOneEnable).asBits
    ret
  }

  def resize(newExp : ExpNumber): AFix ={
    assert(newExp.value < exp) //for now
    val dif = exp - newExp.value
    val ret = new AFix(
      this.maxRaw * (BigInt(1) << dif),
      this.minRaw * (BigInt(1) << dif),
      newExp.value
    )

    ret.raw := (this.raw << dif).resized

    ret
  }

  def isNegative() : Bool = signed match {
    case false => False
    case true  => raw.msb
  }
  def isPositive() : Bool = signed match {
    case false => True
    case true  => !raw.msb
  }
  def isZero() : Bool = raw === 0

  def asAlwaysPositive() : AFix = {
    assert(signed)
    val ret = AFix(maxRaw = maxRaw, minRaw = 0, exp = exp exp)
    ret := this.truncated
    ret
  }


  /** Logical AND operator */
  override def &(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRangesRepr(this, right)
    val ret = new AFix(lMax.max(rMin), lMin.min(rMax), Math.min(this.exp, right.exp))
    ret.raw := this.raw & right.raw
    ret
  }

  /** Logical OR operator */
  override def |(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRangesRepr(this, right)
    val ret = new AFix(lMax.max(rMin), lMin.min(rMax), Math.min(this.exp, right.exp))
    ret.raw := this.raw | right.raw
    ret
  }

  /** Logical XOR operator */
  override def ^(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRangesRepr(this, right)
    val ret = new AFix(lMax.max(rMin), lMin.min(rMax), Math.min(this.exp, right.exp))
    ret.raw := this.raw ^ right.raw
    ret
  }

  /** Inverse bitwise operator */
  override def unary_~ : AFix = {
    val ret = new AFix(maxRepr, minRepr, exp)
    ret.raw := ~this.raw
    ret
  }

  /**
   * Saturates a number to the range of another number.
   * This accounts for decimal shifting.
   * @param af - AFix value to saturate range to
   * @return - Saturated AFix value
   */
  def sat(af: AFix): AFix = this.sat(af.maxRaw, af.minRaw, af.exp exp)

  /**
   *
   * @param satMax
   * @param satMin
   * @param exp
   * @return
   */
  def sat(satMax: BigInt, satMin: BigInt, exp: ExpNumber): AFix = {
    val expDiff = this.exp - exp.value
    if (expDiff > 0) {
      sat(satMax/BigInt(2).pow(expDiff),
          satMin/BigInt(2).pow(expDiff))
    } else if (expDiff < 0) {
      sat(satMax*BigInt(2).pow(-expDiff) + (if (satMax != 0) BigInt(2).pow(-expDiff)-1 else 0),
          satMin*BigInt(2).pow(-expDiff) + (if (satMin != 0) BigInt(2).pow(-expDiff)-1 else 0))
    } else {
      sat(satMax, satMin)
    }
  }

  /**
   * Saturates a number to a provided integer representation value range
   * @param satMax Max integer value to saturate
   * @param satMin Min integer value to saturate
   * @return - Saturated AFix value
   */
  def sat(satMax: BigInt, satMin: BigInt): AFix = {
    if (this.maxRaw < satMax || this.minRaw > satMin) {
      if (this.hasTag(tagAutoResize)) {
        val this_resized = new AFix(satMax, satMin, exp)
        this_resized := this.resized
        return this_resized.sat(satMax, satMin)
      }
    }
    val ret = new AFix(satMax.min(this.maxRaw), satMin.max(this.minRaw), exp)
    when (this > AFix(satMax, exp exp)) {
      if (ret.signed)
        ret.raw := BigIntToSInt(ret.maxRaw).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(ret.maxRaw).resize(ret.bitWidth).asBits
    } elsewhen (this < AFix(satMin, exp exp)) {
      if (ret.signed)
        ret.raw := BigIntToSInt(ret.minRaw).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(ret.minRaw).resize(ret.bitWidth).asBits
    } otherwise {
      if (ret.signed)
        ret.raw := this.raw.asSInt.resize(ret.bitWidth).asBits
      else
        ret.raw := this.raw.asUInt.resize(ret.bitWidth).asBits
    }
    ret
  }

  /**
   * Saturates the top m bits. Other AFix specific saturation functions are recommended.
   * This function is bit orientated unlike other AFix functions.
   * @param m - Number of high bits to saturate off
   * @return
   */
  def sat(m: Int): AFix = {
    if (m == 0) return this
    val newMax = BigInt(2).pow(bitWidth-m).min(this.maxRaw)
    val newMin = if (signed) (-BigInt(2).pow(bitWidth-m)).max(this.minRaw) else BigInt(0).max(this.minRaw)
    val ret = new AFix(newMax, newMin, exp)

    if (this.signed) {
      ret.raw := this.raw.asSInt.sat(m).resize(widthOf(ret.raw)).asBits
    } else {
      ret.raw := this.raw.asUInt.sat(m).resize(widthOf(ret.raw)).asBits
    }

    ret
  }

  /**
   * Trims the bottom m bits. Other AFix specific rounding functions are recommended.
   * This function is bit orientated unlike other AFix functions.
   * @param m - Number of low bits to trim off
   * @return
   */
  def trim(m: Int): AFix = {
    val newMax = BigInt(2).pow(m).min(this.maxRaw)
    val newMin = if (signed) (-BigInt(2).pow(m)).max(this.minRaw) else BigInt(0).max(this.minRaw)
    val newExp = if (m >= 0) exp+m else exp-m
    val ret = new AFix(newMax, newMin, newExp)

    if (this.signed) {
      ret.raw := this.raw.asSInt.trim(m).resize(widthOf(ret.raw)).asBits
    } else {
      ret.raw := this.raw.asUInt.trim(m).resize(widthOf(ret.raw)).asBits
    }

    ret
  }

  /**
   * Rounds a value down towards negative infinity (truncation) at the given exp point position
   * @return Rounded result
   */
  def floor(exp : Int): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.FLOOR)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.floor(drop).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.floor(drop).resize(widthOf(res.raw)).asBits
    }
    res
  }

  // Rounding which will set the LSB if any of the thrown bits is set
  def scrap(exp : Int): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val res = new AFix((this.maxRaw >> drop), this.minRaw >> drop, exp)

    res.raw := this.raw.dropLow(drop)
    res.raw.lsb setWhen(this.raw.takeLow(drop).orR)
    res
  }


  def truncate(): AFix = this.floor(0)

  /**
   * Rounds a value up towards positive infinity at the given exp point position
   * @return Rounded result
   */
  def ceil(exp : Int): AFix = ceil(exp, getTrunc.saturation)

  def ceil(exp: Int, aligned: Boolean): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.CEIL)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.ceil(drop, aligned).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.ceil(drop, aligned).resize(widthOf(res.raw)).asBits
    }

    res
  }

  /**
   * Rounds a value towards zero
   * @return Rounded result
   */
  def floorToZero(exp: Int): AFix = {
    if (this.signed) {
      val drop = exp-this.exp
      if(drop < 0) return CombInit(this)
      val (newMax, newMin) = roundedBounds(exp, RoundType.FLOORTOZERO)
      val res = new AFix(newMax, newMin, exp)

      res.raw := this.raw.asSInt.floorToZero(drop).resize(widthOf(res.raw)).asBits
      res
    } else {
      floor(exp)
    }
  }

  /**
   * Rounds a value towards negative or positive infinity
   * @return Rounded result
   */
  def ceilToInf(exp: Int): AFix = {
    ceilToInf(exp, getTrunc.saturation)
  }

  def ceilToInf(exp: Int, aligned: Boolean): AFix = {
    if (this.signed) {
      val drop = exp-this.exp
      if(drop < 0) return CombInit(this)
      val (newMax, newMin) = roundedBounds(exp, RoundType.CEILTOINF)
      val res = new AFix(newMax, newMin, exp)

      res.raw := this.raw.asSInt.ceilToInf(drop, aligned).resize(widthOf(res.raw)).asBits
      res
    } else {
      ceil(exp, aligned)
    }
  }

  /**
   * Rounds a value up (ceiling) if x >= 0.5 otherwise rounds down (floor/truncate)
   * @return Rounded result
   */
  def roundHalfUp(exp: Int): AFix = {
    roundUp(exp, getTrunc.saturation)
  }

  def roundUp(exp: Int, aligned: Boolean): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDUP)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.roundUp(drop, aligned).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.roundUp(drop, aligned).resize(widthOf(res.raw)).asBits
    }
    res
  }

  /**
   * Rounds a value down (floor/truncate) if x <= 0.5 otherwise rounds up (ceil)
   * @return Rounded result
   */
  def roundHalfDown(exp: Int): AFix = {
    roundDown(exp, getTrunc.saturation)
  }

  def roundDown(exp: Int, aligned: Boolean): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDDOWN)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.roundDown(drop, aligned).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.roundDown(drop, aligned).resize(widthOf(res.raw)).asBits
    }
    res
  }

  /**
   * Rounds a value towards zero (floor/truncate) if x <= 0.5 otherwise rounds towards infinity
   * @return Rounded result
   */
  def roundHalfToZero(exp: Int): AFix = {
    roundToZero(exp, getTrunc.saturation)
  }

  def roundToZero(exp: Int, aligned: Boolean): AFix = {
    if (this.signed) {
      val drop = exp-this.exp
      if(drop < 0) return CombInit(this)
      val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDTOZERO)
      val res = new AFix(newMax, newMin, exp)

      res.raw := this.raw.asSInt.roundToZero(drop, aligned).resize(widthOf(res.raw)).asBits
      res
    } else {
      roundDown(exp, aligned)
    }
  }

  /**
   * Rounds a value towards infinity if x >= 0.5 otherwise rounds towards zero
   * @return Rounded result
   */
  def roundHalfToInf(exp: Int): AFix = {
    roundToInf(exp, getTrunc.saturation)
  }

  def roundToInf(exp: Int, aligned: Boolean): AFix = {
    if (this.signed) {
      val drop = exp-this.exp
      if(drop < 0) return CombInit(this)
      val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDTOINF)
      val res = new AFix(newMax, newMin, exp)

      res.raw := this.raw.asSInt.roundToInf(drop, aligned).resize(widthOf(res.raw)).asBits
      res
    } else {
      roundHalfUp(exp)
    }
  }

  /**
   * Rounds a value towards the nearest even value including half values, otherwise rounds towards odd values
   * @return Rounded result
   */
  def roundHalfToEven(exp: Int): AFix = {
    roundToEven(exp, getTrunc.saturation)
  }

  def roundToEven(exp: Int, aligned: Boolean): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDTOEVEN)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.roundToEven(drop, aligned).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.roundToEven(drop, aligned).resize(widthOf(res.raw)).asBits
    }

    res
  }

  /**
   * Rounds a value towards the nearest odd value including half values, otherwise rounds towards even values
   * @return Rounded result
   */
  def roundHalfToOdd(exp: Int): AFix = {
    roundToOdd(exp, getTrunc.saturation)
  }

  def roundToOdd(exp: Int, align: Boolean): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val (newMax, newMin) = roundedBounds(exp, RoundType.ROUNDTOODD)
    val res = new AFix(newMax, newMin, exp)

    if (this.signed) {
      res.raw := this.raw.asSInt.roundToOdd(drop, false).resize(widthOf(res.raw)).asBits
    } else {
      res.raw := this.raw.asUInt.roundToOdd(drop, false).resize(widthOf(res.raw)).asBits
    }

    res
  }

  private def getTrunc: TagAFixTruncated = {
    this.getTag(classOf[TagAFixTruncated]).getOrElse(AFixRounding.get)
  }

  def round(exp: Int, aligned: Boolean = getTrunc.saturation): AFix = {
    this._round(getTrunc.rounding, exp, aligned)
  }

  def fixTo(af: AFix, roundType: RoundType): AFix = this._round(roundType, af.exp).sat(af)
  def fixTo(af: AFix): AFix = this.fixTo(af, getTrunc.rounding)
  def fixTo(Q: QFormat): AFix = {
    val res = AFix(Q.width-Q.fraction exp, -Q.fraction exp, Q.signed)
    res := this.fixTo(res)
    res
  }

  override def toString: String = {
    s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[max=${maxRaw}($maxValue), min=${minRaw}($minValue), exp=${exp}($step), bits=${raw.getWidth}]"
  }

  private def _round(roundType: RoundType, exp: Int = 0, align: Boolean = getTrunc.saturation): AFix = {
    roundType match {
      case RoundType.FLOOR       => this.floor(exp)
      case RoundType.CEIL        => this.ceil(exp, align)
      case RoundType.FLOORTOZERO => this.floorToZero(exp)
      case RoundType.CEILTOINF   => this.ceilToInf(exp, align)
      case RoundType.ROUNDUP     => this.roundUp(exp, align)
      case RoundType.ROUNDDOWN   => this.roundDown(exp, align)
      case RoundType.ROUNDTOZERO => this.roundToZero(exp, align)
      case RoundType.ROUNDTOINF  => this.roundToInf(exp, align)
      case RoundType.ROUNDTOEVEN => this.roundToEven(exp, align)
      case RoundType.ROUNDTOODD  => this.roundToOdd(exp, align)
      case RoundType.SCRAP        => this.scrap(exp)
    }
  }

  def saturated(rounding  : RoundType): AFix = this.truncated(saturation = true, overflow = false, rounding = rounding)
  def saturated: AFix = this.saturated(getTrunc.rounding)

  def truncated(saturation: Boolean, overflow: Boolean, rounding: RoundType) : AFix = {
    assert(!(saturation && overflow), s"Cannot both overflow and saturate.\n")
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(new TagAFixTruncated(
      saturation,
      overflow,
      rounding
    ))
    copy
  }
  def truncated: AFix = this.truncated(saturation = false, overflow = true, getTrunc.rounding)

  def rounded(rounding  : RoundType): AFix = truncated(saturation = false, overflow = false, rounding = rounding)
  def rounded: AFix = this.rounded(getTrunc.rounding)

  override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case af: AFix =>
        val trunc = af.getTag(classOf[TagAFixTruncated])
        if(this.exp > af.exp && trunc.isEmpty){
          PendingError(s"Cannot assign ${af} to ${this} as precision would be lost! Consider rounding before assignment.\n" + ScalaLocated.long)
          return
        }

        var af_rounded: AFix = af
        if (af.exp > this.exp) {
          af_rounded = af.resize(this.exp exp)
        } else if (af.exp < this.exp) {
          if (trunc.isDefined) {
            af_rounded = (af >> this.exp)._round(trunc.get.rounding) << this.exp
          }
        }



        var af_sat: AFix = af_rounded
        if (trunc.isDefined) {
          if (trunc.get.saturation) {
            af_sat = af_sat.sat(this)
          } else if (trunc.get.overflow) {
            af_sat = this.clone
            af_sat.raw := af_rounded.raw.resized
          }
        }

        val (du, dd, su, sd) = alignRanges(this, af_sat)
        if((du < su || dd > sd) && (trunc.isEmpty || (!trunc.get.saturation && !trunc.get.overflow))){
          PendingError(s"Cannot assign ${af} to ${this} as it would get out of range $du < $su || $dd > $sd \n" + ScalaLocated.long)
          return
        }

        (this.signed, af_sat.signed) match {
          case (true, true) => this.raw.compositAssignFrom(af_sat.raw.asSInt.resize(widthOf(this.raw)).asBits, this.raw, kind)
          case _            => this.raw.compositAssignFrom(af_sat.raw.resized, this.raw, kind)
        }

      case u: UInt => this.raw.compositAssignFrom(u.asBits, this.raw, kind)
      case s: SInt => this.raw.compositAssignFrom(s.asBits, this.raw, kind)
      case uf: UFix => this.compositAssignFrom(AFix(uf), this, kind)
      case sf: SFix => this.compositAssignFrom(AFix(sf), this, kind)
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

  def asUFix(): UFix = this.asUInt().toUFix >> -this.exp
  def asSFix(): SFix = this.asSInt().toSFix >> -this.exp
  def toAFix(that : HardType[AFix]) : AFix = {
    val ret = that()
    ret := this
    ret
  }

  def :=(u: UInt) = this assignFrom(u)
  def :=(s: SInt) = this assignFrom(s)
  def :=(u: UFix) = this assignFrom(u)
  def :=(s: SFix) = this assignFrom(s)
  def :=(a: AFix) = this assignFrom(a)

  def :=(that: BigDecimal) = {
    assert(that <= this.maxValue, s"Literal $that is too big to be assigned in $this")
    assert(that >= this.minValue, s"Literal $that is too negative to be assigned in this $this")

    val shift = -exp
    val value = if(shift >= 0)
      (that * BigDecimal(BigInt(1) << shift)).toBigInt
    else
      (that / BigDecimal(BigInt(1) << -shift)).toBigInt
    this.raw := value
  }

  def init(that: BigDecimal): this.type = {
    this.raw init AF(that, wholeWidth bit, fracWidth bit, signed).raw
    this
  }
  def init(that: AFix): this.type = {
    this.raw init that.raw
    this
  }

  override def clone: this.type = new AFix(maxRaw, minRaw, exp).asInstanceOf[this.type]

  def hasParametersOf(that : AFix) : Boolean = this.maxRaw == that.maxRaw && this.minRaw == that.minRaw && this.exp == that.exp
  override def getMuxType[T <: Data](list: TraversableOnce[T]) = {
    val p = AFix.holdingParams(list.asInstanceOf[TraversableOnce[AFix]])
    HardType(new AFix(p._1, p._2, p._3).asInstanceOf[T])
  }

  override def toMuxInput[T <: Data](muxOutput: T) : T = {
    if(this.hasParametersOf(muxOutput.asInstanceOf[AFix])) return this.asInstanceOf[T]
    val ret = cloneOf(muxOutput)
    ret.assignFrom(this)
    ret
  }
}

object AF {
  def apply(value: BigDecimal, integerWidth: BitCount, fractionWidth: BitCount, signed: Boolean): AFix = {
    val ret = if (signed) AFix.SQ(integerWidth, fractionWidth) else AFix.UQ(integerWidth, fractionWidth)
    val tmp = cloneOf(ret)
    tmp := value
    ret.raw.assignFromBits(tmp.raw)
    ret
  }
}

class TagAFixTruncated(val saturation: Boolean,
                       val overflow  : Boolean,
                       val rounding  : RoundType
                       ) extends SpinalTag{
  override def duplicative = true
  override def canSymplifyHost: Boolean = true
}


object AFixRounding extends ScopeProperty[TagAFixTruncated] {
  override def default: TagAFixTruncated = new TagAFixTruncated(false, false, RoundType.FLOOR)

  def set(saturation: Boolean = true, overflow: Boolean = false, rounding: RoundType = RoundType.FLOOR): AFixRounding.SetReturn = {
   new AFixRounding.SetReturn(new TagAFixTruncated(saturation, overflow, rounding))
  }

  def apply(saturation: Boolean = true, overflow: Boolean = false, rounding: RoundType = RoundType.FLOOR): AFixRounding.ApplyClass = {
    new AFixRounding.ApplyClass(new TagAFixTruncated(saturation, overflow, rounding))
  }
}
