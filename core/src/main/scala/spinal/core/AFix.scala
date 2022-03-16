package spinal.core

import scala.collection.mutable.ArrayBuffer

object AFix {

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

  def apply(amplitude : ExpNumber, resolution : ExpNumber, signed : Boolean) : AFix = {
    val maxValue = BigInt(2).pow(amplitude.value-resolution.value)-1
    val minValue = if (signed) -BigInt(2).pow(amplitude.value-resolution.value) else BigInt(0)
    new AFix(maxValue, minValue, resolution)
  }

  def U(width: BitCount): AFix = AFix(width.value exp, 0 exp, signed = false)
//  def U(wholeBits: BitCount, fracBits: BitCount): AFix = AFix(wholeBits.value exp, 0 exp, signed = false)
  def U(amplitude: ExpNumber, width: BitCount): AFix = AFix(amplitude, (amplitude.value - width.value) exp, false)
  def U(amplitude: ExpNumber, resolution: ExpNumber): AFix = AFix(amplitude, resolution, false)
//  def U(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits, -exp.value bit, false)
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
//  def S(wholeBits: BitCount, fracBits: BitCount): AFix = AFix(wholeBits+(1 bit), fracBits, signed = true)
  def S(amplitude: ExpNumber, width: BitCount): AFix = AFix(amplitude, (amplitude.value - width.value + 1) exp, true)
  def S(amplitude: ExpNumber, resolution: ExpNumber): AFix = AFix(amplitude, resolution, signed = true)
//  def S(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits+(1 bit), -exp.value bit, signed = true)
//  def S(maximum: BigInt, resolution: ExpNumber): AFix =
//    new AFix(maximum.max(0)*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)*maximum.signum-maximum.signum),
//      maximum.min(0)*BigInt(2).pow(-resolution.value)+(BigInt(2).pow(-resolution.value)*maximum.signum-maximum.signum), resolution)
//  def S(maxValue: BigInt, minValue: BigInt, exp: ExpNumber): AFix =
//    new AFix(maxValue*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)*maxValue.signum-maxValue.signum),
//      minValue*BigInt(2).pow(-exp.value), exp)

}

class AFix(val maxValue: BigInt, val minValue: BigInt, val exp: ExpNumber) extends MultiData {

  val signed = (maxValue < 0) || (minValue < 0)
  val signWidth = if (signed) 1 else 0

  private val maxShifted = maxValue.abs - (if (maxValue < 0) signWidth else 0)
  private val maxBits = maxShifted.bitLength
  private val minShifted = minValue.abs - (if (minValue < 0) signWidth else 0)
  private val minBits = minShifted.bitLength

  val bitWidth = Math.max(maxBits, minBits) + signWidth
  val fracWidth = Math.min(-exp.value, 0)
  val wholeWidth = bitWidth - fracWidth - signWidth
  val intWidth = bitWidth - fracWidth

  val raw: Bits = Bits(bitWidth bit)

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  private def alignRanges(l: AFix, r: AFix): (BigInt, BigInt, BigInt, BigInt) = {
    val expDiff = l.exp.value - r.exp.value
    // Scale left or right ranges if there's a difference in precision
    if (expDiff > 0) {
      (l.maxValue*BigInt(2).pow(expDiff), l.minValue*BigInt(2).pow(expDiff), r.maxValue, r.minValue)
    } else if (expDiff < 0) {
      (l.maxValue, l.minValue, r.maxValue*BigInt(2).pow(-expDiff), r.minValue*BigInt(2).pow(-expDiff))
    } else {
      (l.maxValue, l.minValue, r.maxValue, r.minValue)
    }
  }

  private def alignLR(l: AFix, r: AFix): (Bits, Bits) = {
    val expDiff = l.exp.value - r.exp.value
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
    val ret = new AFix(lMax+rMax, lMin+rMin, Math.min(this.exp.value, right.exp.value) exp)

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
   * Adds `this` to the right hand side AFix value without expanding ranges or checks on value overflow
   * @param right Value to add to `this`
   * @return Sum
   */
  def +|(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), Math.min(this.exp.value, right.exp.value) exp)

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
    val ret = new AFix(lMax-rMin, lMin-rMax, Math.min(this.exp.value, right.exp.value) exp)

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
   * Subtracts `this` from the right hand side AFix value without expanding ranges or checks on value underflow
   * @param right Value to subtract from `this`
   * @return Difference
   */
  def -|(right: AFix): AFix = {
    val (lMax, lMin, rMax, rMin) = alignRanges(this, right)
    val ret = new AFix(lMax.max(rMin), lMin.min(rMax), Math.min(this.exp.value, right.exp.value) exp)

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
    val ret = new AFix(possibleLimits.max, possibleLimits.min, this.exp.value + right.exp.value exp)

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
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp.value + right.exp.value exp)

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
    val ret = new AFix(lMax.max(rMax), lMin.min(rMin), this.exp.value + right.exp.value exp)

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
    val ret = new AFix(this.maxValue, this.minValue, (this.exp.value - shift) exp)

    ret.raw := this.raw

    ret
  }

  // Shift decimal point right
  def >>(shift: Int): AFix = {
    val ret = new AFix(this.maxValue, this.minValue, (this.exp.value + shift) exp)

    ret.raw := this.raw

    ret

  }

  // Shift bits and decimal point left, adding padding bits right
  def <<|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue * shiftBig, this.minValue * shiftBig, (this.exp.value + shift) exp)

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

  def unary_-(): AFix = negate()

  def negate(): AFix = {
    val ret = new AFix(-this.maxValue, -this.minValue, this.exp)

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

  /**
   * Saturates a number to the range of another number.
   * This accounts for decimal shifting.
   * @param af - AFix value to saturate range to
   * @return - Saturated AFix value
   */
  def sat(af: AFix): AFix = {
    val expDiff = this.exp.value - af.exp.value
    if (expDiff > 0) {
      sat(af.maxValue/BigInt(2).pow(expDiff), af.minValue/BigInt(2).pow(expDiff))
    } else if (expDiff < 0) {
      sat(af.maxValue*BigInt(2).pow(-expDiff), af.maxValue/BigInt(2).pow(-expDiff))
    } else {
      sat(af.maxValue, af.minValue)
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
        SpinalWarning(s"Saturation of ${this} to range [${satMax} - ${satMin}] has been limited to the representable range of the input.\nConsider adjusting the saturation range or resizing the input.\n" + ScalaLocated.long)
      }
    }
    val ret = new AFix(satMax.min(this.maxValue), satMin.max(this.minValue), exp)
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

  /**
   * Rounds a value down towards negative infinity (truncation)
   * @return Rounded result
   */
  def floor(): AFix = {
    assert(this.exp.value < 0, f"Cannot floor() because number does not have enough fractional bits, needs at least -1 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

    res.raw := this.raw.dropLow(-this.exp.value)
    res
  }

  def truncate(): AFix = this.floor()

  /**
   * Rounds a value up towards positive infinity
   * @return Rounded result
   */
  def ceil(): AFix = {
    assert(this.exp.value < 0, f"Cannot ceil() because number does not have enough fractional bits, needs at least -1 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

    val fracOr = this.raw.takeLow(-this.exp.value).orR
    if (this.signed) {
      res.raw := (this.raw.dropLow(-this.exp.value).asSInt + (False ## fracOr).asSInt).asBits
    } else {
      res.raw := (this.raw.dropLow(-this.exp.value).asUInt + fracOr.asUInt).asBits
    }
    res
  }

  /**
   * Rounds a value towards zero
   * @return Rounded result
   */
  def floorToZero(): AFix = {
    assert(this.exp.value < 0, f"Cannot floorToZero() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value towards negative or positive infinity
   * @return Rounded result
   */
  def ceilToInf(): AFix = {
    assert(this.exp.value < 0, f"Cannot ceilToInf() because number does not have enough fractional bits, needs at least -1 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value up (ceiling) if x >= 0.5 otherwise rounds down (floor/truncate)
   * @return Rounded result
   */
  def roundHalfUp(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfUp() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value down (floor/truncate) if x <= 0.5 otherwise rounds up (ceil)
   * @return Rounded result
   */
  def roundHalfDown(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfDown() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value towards zero (floor/truncate) if x <= 0.5 otherwise rounds towards infinity
   * @return Rounded result
   */
  def roundHalfToZero(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToZero() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value towards infinity if x >= 0.5 otherwise rounds towards zero
   * @return Rounded result
   */
  def roundHalfToInf(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToInf() because number does not have enough fractional bits, needs at least -2 exp")
    if (this.signed) {
      val shift = BigInt(2).pow(-this.exp.value)
      val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value towards the nearest even value including half values, otherwise rounds towards odd values
   * @return Rounded result
   */
  def roundHalfToEven(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToEven() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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

  /**
   * Rounds a value towards the nearest odd value including half values, otherwise rounds towards even values
   * @return Rounded result
   */
  def roundHalfToOdd(): AFix = {
    assert(this.exp.value < -1, f"Cannot roundHalfToOdd() because number does not have enough fractional bits, needs at least -2 exp")
    val shift = BigInt(2).pow(-this.exp.value)
    val res = new AFix(this.maxValue / shift, this.minValue / shift, 0 exp)

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


  def truncated: AFix = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy
  }

  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case af: AFix =>
        if(this.exp.value > af.exp.value && !af.hasTag(tagTruncated)){
          PendingError(s"Cannot assign ${af} to ${this} as precision would be lost! Consider rounding before assignment.\n" + ScalaLocated.long)
          return
        }

        val (du, dd, su, sd) = alignRanges(this, af)
        if(du < su || dd > sd){

          PendingError(s"Cannot assign ${af} to ${this} as it would get out of range $du < $su || $dd > $sd \n" + ScalaLocated.long)
          return
        }

        if (af.bitWidth != this.bitWidth) {
          SpinalWarning(s"Assigning ${af} to ${this} required bit expansion.\n" + ScalaLocated.long)
        }

        var af_frac_expand: AFix = af
        if (af.exp.value > this.exp.value) {
          val exp_diff = af.exp.value - this.exp.value
          af_frac_expand = af <<| exp_diff
        }

        if (this.signed)
          this.raw := af_frac_expand.raw.asSInt.resize(this.bitWidth).asBits
        else
          this.raw := af_frac_expand.raw.asUInt.resize(this.bitWidth).asBits

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

  def asUFix(): UFix = this.asUInt().toUFix >> -this.exp.value
  def asSFix(): SFix = this.asSInt().toSFix >> -this.exp.value

  def :=(u: UInt) = this assignFrom(u)
  def :=(s: SInt) = this assignFrom(s)
  def :=(u: UFix) = this assignFrom(u)
  def :=(s: SFix) = this assignFrom(s)

  override def clone: this.type = new AFix(maxValue, minValue, exp).asInstanceOf[this.type]
}

