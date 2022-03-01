package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  def U(wholeBits: BitCount): AFix = AFix(wholeBits, 0 bit, signed = false)
  def U(wholeBits: BitCount, fracBits: BitCount): AFix = AFix(wholeBits, fracBits, signed = false)
  def U(msbExp: ExpNumber, bitLength: BitCount): AFix = AFix(msbExp.value bit, (bitLength.value - msbExp.value) bit, false)
  def U(msbExp: ExpNumber, lsbExp: ExpNumber): AFix = AFix(msbExp.value bit, -lsbExp.value bit, false)
  def U(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits, -exp.value bit, false)
  def U(maxValue: BigInt, exp: ExpNumber): AFix = {
    assert(maxValue >= 0, s"AFix.U maxValue must be non-negative! (${maxValue} is not >= 0)")
    new AFix(maxValue*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)-1), 0, exp)
  }
  def U(maxValue: BigInt, minValue: BigInt, exp: ExpNumber): AFix = {
    assert(maxValue >= 0, s"AFix.U maxValue must be non-negative! (${maxValue} is not >= 0)")
    assert(maxValue >= 0, s"AFix.U minValue must be non-negative! (${minValue} is not >= 0)")
    new AFix(maxValue*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)-1),
      minValue*BigInt(2).pow(-exp.value), exp)
  }

  def S(wholeBits: BitCount): AFix = AFix(wholeBits, 0 bit, signed = true)
  def S(wholeBits: BitCount, fracBits: BitCount): AFix = AFix(wholeBits+(1 bit), fracBits, signed = true)
  def S(msbExp: ExpNumber, bitLength: BitCount): AFix = AFix(msbExp.value bits, (bitLength.value - msbExp.value) bit, signed = true)
  def S(msbExp: ExpNumber, lsbExp: ExpNumber): AFix = AFix(msbExp.value+1 bit, -lsbExp.value bit, signed = true)
  def S(wholeBits: BitCount, exp: ExpNumber): AFix = AFix(wholeBits+(1 bit), -exp.value bit, signed = true)
  def S(value: BigInt, exp: ExpNumber): AFix =
    new AFix(value.max(0)*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)*value.signum-value.signum),
      value.min(0)*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)*value.signum-value.signum), exp)
  def S(maxValue: BigInt, minValue: BigInt, exp: ExpNumber): AFix =
    new AFix(maxValue*BigInt(2).pow(-exp.value)+(BigInt(2).pow(-exp.value)*maxValue.signum-maxValue.signum),
      minValue*BigInt(2).pow(-exp.value), exp)

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
  val fracWidth = Math.min(-exp.value, 0)
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
    this dependsOn right

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
    this dependsOn right

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
    this dependsOn right

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
    this dependsOn right

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
    this dependsOn right

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
    ret dependsOn this

    ret.raw := this.raw

    ret
  }

  // Shift decimal point right
  def >>(shift: Int): AFix = {
    val ret = new AFix(this.maxValue, this.minValue, (this.exp.value + shift) exp)
    ret dependsOn this

    ret.raw := this.raw

    ret

  }

  // Shift bits and decimal point left, adding padding bits right
  def <<|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue * shiftBig, this.minValue * shiftBig, (this.exp.value + shift) exp)
    ret dependsOn this

    ret.raw := this.raw << shift

    ret
  }

  // Shift bits and decimal point right, adding padding bits left
  def >>|(shift: Int): AFix = {
    val shiftBig = BigInt(2).pow(shift)
    val ret = new AFix(this.maxValue / shiftBig, this.minValue / shiftBig, this.exp)
    ret dependsOn this

    if (this.signed)
      ret.raw := this.raw.asSInt.resize(ret.bitWidth).asBits
    else
      ret.raw := this.raw.asUInt.resize(ret.bitWidth).asBits

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

  def sat(af: AFix): AFix = sat(af.maxValue, af.minValue)
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

