package spinal.core

import scala.collection.mutable.ArrayBuffer

object AFix {

  def apply(maxValue: BigInt, minValue: BigInt, exp: ExpNumber) : AFix = new AFix(maxValue = maxValue, minValue = minValue, exp = exp.value)
  def apply(u: UInt): AFix = AFix(u, 0 exp)
  def apply(u: UInt, exp: ExpNumber): AFix = {
    val maxValue = BigInt(2).pow(u.getWidth)-1
    val ret = new AFix(maxValue, 0, exp.value)
    ret.raw := u.asBits
    ret
  }
  def apply(u: UInt, maxValue: BigInt, exp : ExpNumber): AFix = {
    val ret = new AFix(maxValue, 0, exp.value)
    ret.raw := u.asBits.resized
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
    val maxValue = BigInt(2).pow(sf.bitCount-1)-1
    val minValue = -BigInt(2).pow(sf.bitCount-1)
    val ret = new AFix(maxValue, minValue, -sf.minExp)
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
    val maxValue = BigInt(2).pow(amplitude.value-resolution.value)-1
    val minValue = if (signed) -BigInt(2).pow(amplitude.value-resolution.value) else BigInt(0)
    new AFix(maxValue, minValue, resolution.value)
  }

  def U(width: BitCount): AFix = AFix(width.value exp, 0 exp, signed = false)
  def UQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AFix(integerWidth.value exp, -fractionWidth.value exp, signed = false)
  def U(amplitude: ExpNumber, width: BitCount): AFix = AFix(amplitude, (amplitude.value - width.value) exp, false)
  def U(amplitude: ExpNumber, resolution: ExpNumber): AFix = AFix(amplitude, resolution, false)
//  def U(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits, -exp bit, false)
//  def U(maximum: BigInt, resolution: ExpNumber): AFix = {
//    assert(maximum >= 0, s"AFix.U maxValue must be non-negative! (${maximum} is not >= 0)")
//    new AFix(maximum*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)-1), 0, resolution)
//  }
//  def U(maximum: BigInt, minimum: BigInt, resolution: ExpNumber): AFix = {
//    assert(maximum >= 0, s"AFix.U maxValue must be non-negative! (${maximum} is not >= 0)")
//    assert(maximum >= 0, s"AFix.U minValue must be non-negative! (${minimum} is not >= 0)")
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
//  def S(maxValue: BigInt, minValue: BigInt, exp: ExpNumber): AFix =
//    new AFix(maxValue*BigInt(2).pow(-exp)+(BigInt(2).pow(-exp)*maxValue.signum-maxValue.signum),
//      minValue*BigInt(2).pow(-exp), exp)

  def holding(values: TraversableOnce[AFix]) : AFix = {
    val param = holdingParams(values)
    new AFix(
      maxValue = param._1,
      minValue = param._2,
      exp      = param._3
    )
  }


  def holdingParams(values: TraversableOnce[AFix]) : (BigInt, BigInt, Int) = {
    val ex = values.map(_.exp).min
    (
      values.map(e => e.maxValue << (e.exp - ex).max(0)).max,
      values.map(e => e.minValue << (e.exp - ex).max(0)).min,
      ex
    )
  }
}

class AFix(val maxValue: BigInt, val minValue: BigInt, val exp: Int) extends MultiData with Num[AFix] with MinMaxProvider {
  assert(maxValue >= minValue)

  val signed = (maxValue < 0) || (minValue < 0)
  val signWidth = if (signed) 1 else 0

  private val maxShifted = maxValue.abs - (if (maxValue < 0) signWidth else 0)
  private val maxBits = maxShifted.bitLength
  private val minShifted = minValue.abs - (if (minValue < 0) signWidth else 0)
  private val minBits = minShifted.bitLength

  // Number of bits to represent the entire value
  val bitWidth = Math.max(maxBits, minBits) + signWidth
  // Number of bits to represent the fractional value
  val fracWidth = Math.min(-exp, 0)
  // Number of bits to represent the whole value, no sign
  val wholeWidth = bitWidth - fracWidth - signWidth
  // Number of bits to represent the whole ("integer") value, with sign
  val intWidth = bitWidth - fracWidth
  // Number of bits to represent the numeric value, no sign
  val numWidth = bitWidth - signWidth

  val raw: Bits = Bits(bitWidth bit)

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
      (l.maxValue*BigInt(2).pow(expDiff),
       l.minValue*BigInt(2).pow(expDiff),
       r.maxValue, r.minValue)
    } else if (expDiff < 0) {
      (l.maxValue, l.minValue,
       r.maxValue*BigInt(2).pow(-expDiff),
       r.minValue*BigInt(2).pow(-expDiff))
    } else {
      (l.maxValue, l.minValue, r.maxValue, r.minValue)
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
    val (lMax, lMin, rMax, rMin) = (this.maxValue, this.minValue, right.maxValue, right.minValue)
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
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMin > rMax || lMax < rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          === _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized === _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          === _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          === _r.asSInt.resized)
      }
    }
  }

  def !=(right: AFix): Bool = this =/= right
  def =/=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMin > rMax || lMax < rMin) {
      True
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          =/= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized =/= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          =/= _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          =/= _r.asSInt.resized)
      }
    }
  }

  def <(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMax < rMin) {
      True
    } else if (lMin >= rMax) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          < _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized < _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          < _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          < _r.asSInt.resized)
      }
    }
  }

  def <=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMax <= rMin) {
      True
    } else if (lMin > rMax) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          <= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized <= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          <= _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          <= _r.asSInt.resized)
      }
    }
  }

  def >(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMin > rMax) {
      True
    } else if (lMax <= rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          > _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized > _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          > _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          > _r.asSInt.resized)
      }
    }
  }

  def >=(right: AFix): Bool = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)

    if (lMin >= rMax) {
      True
    } else if (lMax < rMin) {
      False
    } else {
      val (_l, _r) = alignLR(this, right)
      (this.signed, right.signed) match {
        case (false, false) => (_l.asUInt.resized          >= _r.asUInt.resized)
        case (false,  true) => (_l.asUInt.intoSInt.resized >= _r.asSInt.resized)
        case ( true, false) => (_l.asSInt.resized          >= _r.asUInt.intoSInt.resized)
        case ( true,  true) => (_l.asSInt.resized          >= _r.asSInt.resized)
      }
    }
  }

  // Shift decimal point left
  def <<(shift: Int): AFix = {
    val ret = new AFix(this.maxValue, this.minValue, (this.exp + shift))

    ret.raw := this.raw

    ret
  }

  // Shift decimal point right
  def >>(shift: Int): AFix = {
    val ret = new AFix(this.maxValue, this.minValue, (this.exp - shift))

    ret.raw := this.raw

    ret

  }

  // Shift bits and decimal point left, adding padding bits right
  def <<|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue * shiftBig, this.minValue * shiftBig, (this.exp + shift))

    ret.raw := this.raw << shift

    ret
  }

  // Shift bits and decimal point right, adding padding bits left
  def >>|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue / shiftBig, this.minValue / shiftBig, this.exp)

    if (this.signed)
      ret.raw := this.raw.asSInt.resize(ret.bitWidth).asBits
    else
      ret.raw := this.raw.asUInt.resize(ret.bitWidth).asBits

    ret
  }

  def >>(shift: AFix): AFix = {
    assert(shift.exp == 0)
    assert(shift.minValue == 0)
    val ret = new AFix(
      this.maxValue * (BigInt(1) << shift.maxValue.toInt),
      this.minValue * (BigInt(1) << shift.maxValue.toInt),
      (this.exp - shift.maxValue.toInt)
    )

    ret.raw := (this.raw << shift.maxValue.toInt) >> U(shift)

    ret
  }

  def >>|(shift: AFix): AFix = {
    assert(shift.exp == 0)
    assert(shift.minValue == 0)
    val ret = cloneOf(this)

    ret.raw := this.raw >> U(shift)

    ret
  }

  // Shift bits and decimal point left, loosing bits
  def |<<(shift: Int): AFix = {
    val width = widthOf(raw)-shift
    val ret = new AFix(this.maxValue.min(BigInt(2).pow(width)-1), this.minValue.max(-BigInt(2).pow(width)), (this.exp + shift))

    ret.raw := this.raw.resized

    ret
  }

  def unary_-(): AFix = negate()

  def negate(): AFix = {
    val ret = new AFix(-this.minValue, -this.maxValue, this.exp)

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


  def negate(enable : Bool): AFix = {
    val ret = new AFix(-this.minValue max this.maxValue, -this.maxValue min this.minValue, this.exp)

    if (this.signed) {
      ???
    } else {
      ret := U(this.raw).twoComplement(enable)
    }

    ret
  }

  def resize(newExp : ExpNumber): AFix ={
    assert(newExp.value < exp) //for now
    val dif = exp - newExp.value
    val ret = new AFix(
      this.maxValue * (BigInt(1) << dif),
      this.minValue * (BigInt(1) << dif),
      newExp.value
    )

    ret.raw := this.raw << dif

    ret
  }


  /**
   * Saturates a number to the range of another number.
   * This accounts for decimal shifting.
   * @param af - AFix value to saturate range to
   * @return - Saturated AFix value
   */
  def sat(af: AFix): AFix = this.sat(af.maxValue, af.minValue, af.exp exp)

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
    if (this.maxValue < satMax || this.minValue > satMin) {
      if (this.hasTag(tagAutoResize)) {
        val this_resized = new AFix(satMax, satMin, exp)
        this_resized := this.resized
        return this_resized.sat(satMax, satMin)
      } else {
//        SpinalWarning(s"Saturation of ${this} to range [${satMax} - ${satMin}] has been limited to the representable range of the input.\nConsider adjusting the saturation range or resizing the input.\n" + ScalaLocated.long)
      }
    }
    val ret = new AFix(satMax.min(this.maxValue), satMin.max(this.minValue), exp)
    when (this > AFix(satMax, exp exp)) {
      if (ret.signed)
        ret.raw := BigIntToSInt(ret.maxValue).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(ret.maxValue).resize(ret.bitWidth).asBits
    } elsewhen (this < AFix(satMin, exp exp)) {
      if (ret.signed)
        ret.raw := BigIntToSInt(ret.minValue).resize(ret.bitWidth).asBits
      else
        ret.raw := BigIntToUInt(ret.minValue).resize(ret.bitWidth).asBits
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
    val newMax = BigInt(2).pow(bitWidth-m).min(this.maxValue)
    val newMin = if (signed) (-BigInt(2).pow(bitWidth-m)).max(this.minValue) else BigInt(0).max(this.minValue)
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
    val newMax = BigInt(2).pow(m).min(this.maxValue)
    val newMin = if (signed) (-BigInt(2).pow(m)).max(this.minValue) else BigInt(0).max(this.minValue)
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
    val res = new AFix(this.maxValue >> drop, this.minValue >> drop, 0)

    res.raw := this.raw.dropLow(drop)
    res
  }

  // Rounding which will set the LSB if any of the thrown bits is set
  def scrap(exp : Int): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val res = new AFix(this.maxValue >> drop, this.minValue >> drop, 0)

    res.raw := this.raw.dropLow(drop)
    res.raw.lsb setWhen(this.raw.takeLow(drop).orR)
    res
  }


  def truncate(): AFix = this.floor(0)

  /**
   * Rounds a value up towards positive infinity at the given exp point position
   * @return Rounded result
   */
  def ceil(exp : Int): AFix = {
    val drop = exp-this.exp
    if(drop < 0) return CombInit(this)
    val step = BigInt(1) << drop
    val res = new AFix((this.maxValue+step-1) >> drop, (this.minValue+step-1) >> drop, exp)

    val fracOr = this.raw.takeLow(drop).orR
    if (this.signed) {
      res.raw := (this.raw.dropLow(drop).asSInt.resize(widthOf(res.raw)) + (False ## fracOr).asSInt).asBits
    } else {
      res.raw := (this.raw.dropLow(drop).asUInt.resize(widthOf(res.raw)) + fracOr.asUInt).asBits
    }
    res
  }

  def ceil(exp: Int, aligned: Boolean) = ceil(exp)

  /**
   * Rounds a value towards zero
   * @return Rounded result
   */
  def floorToZero(exp: Int): AFix = {
    assert(this.exp < 0, f"Cannot floorToZero() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val drop = exp-this.exp
      val step = BigInt(1) << drop
      val res = new AFix((this.maxValue+step-1) >> drop, (this.minValue+step-1) >> drop, exp)

      val fracOr = this.raw.takeLow(drop).orR
      val addValue = SInt(2 bit)
      when(this.raw.msb && fracOr) {
        addValue := 1
      } otherwise {
        addValue := 0
      }
      res.raw := (this.raw.dropLow(drop).asSInt.resize(widthOf(res.raw)) + addValue).asBits
      res
    } else {
      floor(0)
    }
  }

  /**
   * Rounds a value towards negative or positive infinity
   * @return Rounded result
   */
  def ceilToInf(exp: Int): AFix = {
    assert(this.exp < 0, f"Cannot ceilToInf() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val drop = exp-this.exp
      val step = BigInt(1) << drop
      val res = new AFix((this.maxValue+step-1) >> drop, (this.minValue+step-1) >> drop, exp)

      val fracOr = this.raw.takeLow(drop).orR
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
      res.raw := (this.raw.dropLow(drop).asSInt.resize(widthOf(res.raw)) + addValue).asBits
      res
    } else {
      ceil(0)
    }
  }

  def ceilToInf(exp: Int, aligned: Boolean): AFix = ceilToInf(exp)

  /**
   * Rounds a value up (ceiling) if x >= 0.5 otherwise rounds down (floor/truncate)
   * @return Rounded result
   */
  def roundHalfUp(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfUp() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

    val fracMSB = this.raw(-this.exp-1)
    val addValue = SInt(2 bit)
    when(fracMSB) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundUp(exp: Int, aligned: Boolean): AFix = roundHalfUp()

  /**
   * Rounds a value down (floor/truncate) if x <= 0.5 otherwise rounds up (ceil)
   * @return Rounded result
   */
  def roundHalfDown(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfDown() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

    val fracOr = this.raw.takeLow(-this.exp-1).orR
    val fracMSB = this.raw(-this.exp-1)
    val addValue = SInt(2 bit)
    when(fracMSB && fracOr) {
      addValue := 1
    } otherwise {
      addValue := 0
    }
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp).asUInt + addValue.asBits(0).asUInt).asBits
    }
    res
  }

  def roundDown(exp: Int, aligned: Boolean): AFix = roundHalfDown()

  /**
   * Rounds a value towards zero (floor/truncate) if x <= 0.5 otherwise rounds towards infinity
   * @return Rounded result
   */
  def roundHalfToZero(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfToZero() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
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
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
      res
    } else {
      roundHalfDown()
    }
  }

  def roundToZero(exp: Int, aligned: Boolean): AFix = roundHalfToZero()

  /**
   * Rounds a value towards infinity if x >= 0.5 otherwise rounds towards zero
   * @return Rounded result
   */
  def roundHalfToInf(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfToInf() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
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
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
      res
    } else {
      roundHalfDown()
    }
  }

  def roundToInf(exp: Int, aligned: Boolean): AFix = roundHalfToInf()

  /**
   * Rounds a value towards the nearest even value including half values, otherwise rounds towards odd values
   * @return Rounded result
   */
  def roundHalfToEven(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfToEven() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

    if (this.signed) {
      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
      val intLSB = this.raw(-this.exp)
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
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
    } else {
      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
      val intLSB = this.raw(-this.exp)
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
      res.raw := (this.raw.dropLow(-this.exp).asUInt + addValue).asBits
    }
    res
  }

  /**
   * Rounds a value towards the nearest odd value including half values, otherwise rounds towards even values
   * @return Rounded result
   */
  def roundHalfToOdd(): AFix = {
    assert(this.exp < -1, f"Cannot roundHalfToOdd() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0)

    if (this.signed) {
      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
      val intLSB = this.raw(-this.exp)
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
      res.raw := (this.raw.dropLow(-this.exp).asSInt + addValue).asBits
    } else {
      val fracOr = this.raw.takeLow(-this.exp-1).orR
      val fracMSB = this.raw(-this.exp-1)
      val intLSB = this.raw(-this.exp)
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
      res.raw := (this.raw.dropLow(-this.exp).asUInt + addValue).asBits
    }
    res
  }

  def round(exp: Int, aligned: Boolean): AFix = {
    val trunc = this.getTag(classOf[TagAFixTruncated])

    if (trunc.isDefined) {
      this._round(trunc.get.rounding)
    } else {
      roundHalfToInf()
    }
  }

  override def toString: String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[max=${maxValue}, min=${minValue}, exp=${exp}, bits=${raw.getWidth}]"

  private def _round(roundType: RoundType): AFix = {
    roundType match {
      case RoundType.FLOOR       => this.floor(0)
      case RoundType.CEIL        => this.ceil(0)
      case RoundType.FLOORTOZERO => this.floorToZero(0)
      case RoundType.CEILTOINF   => this.ceilToInf(0)
      case RoundType.ROUNDUP     => this.roundHalfUp()
      case RoundType.ROUNDDOWN   => this.roundHalfDown()
      case RoundType.ROUNDTOZERO => this.roundHalfToZero()
      case RoundType.ROUNDTOINF  => this.roundHalfToInf()
      case RoundType.ROUNDTOEVEN => this.roundHalfToEven()
      case RoundType.ROUNDTOODD  => this.roundHalfToOdd()
      case RoundType.SCRAP        => this.scrap(0)
    }
  }

  def saturated(): AFix = this.truncated(saturation = true, overflow = false)

  def truncated(saturation: Boolean = false,
                overflow  : Boolean = true,
                rounding  : RoundType = RoundType.FLOOR) : AFix = {
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

  def rounded(rounding  : RoundType = RoundType.FLOOR) = truncated(
    saturation = false,
    overflow = false,
    rounding = rounding
  )

  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
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

        if (this.signed)
          this.raw := af_sat.raw.asSInt.resize(this.bitWidth).asBits
        else
          this.raw := af_sat.raw.asUInt.resize(this.bitWidth).asBits

      case u: UInt => this.raw := u.asBits
      case s: SInt => this.raw := s.asBits
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

  override def clone: this.type = new AFix(maxValue, minValue, exp).asInstanceOf[this.type]

  def hasParametersOf(that : AFix) : Boolean = this.maxValue == that.maxValue && this.minValue == that.minValue && this.exp == that.exp
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



class TagAFixTruncated(val saturation: Boolean,
                       val overflow  : Boolean,
                       val rounding  : RoundType) extends SpinalTag{
  override def duplicative = true
  override def canSymplifyHost: Boolean = true
}