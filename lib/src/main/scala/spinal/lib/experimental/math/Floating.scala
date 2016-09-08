package spinal.lib.experimental.math

import spinal.core._
import spinal.lib._

/**
  * Floating point value
  * @param exponentSize Size of the exponent field
  * @param mantissaSize Size of the mantissa field with the implicit one not included
  */
case class Floating(exponentSize: Int,
                    mantissaSize: Int) extends Bundle {

  /** Mantissa field without the implicit first bit */
  val mantissa = Bits(mantissaSize bits)

  /** Exponent field (127 excess encoded) */
  val exponent = Bits(exponentSize bits)

  /** Sign field (true when negative) */
  val sign = Bool

  private def isExponentZero = exponent === 0
  private def isMantissaZero = mantissa === 0

  /** Return true if number is +/- 0 */
  def isZero = isMantissaZero && isExponentZero

  /** Return true if the number is positive */
  def isPositive = !sign

  /** return this number recoded into Berkeley encoding */
  def toRecFloating = {
    val recExponentSize = exponentSize + 1
    val recoded = RecFloating(recExponentSize, mantissaSize)

    val firstMantissaBit = mantissaSize - OHToUInt(OHMasking.last(mantissa))
    val normalizedMantissa = (mantissa << firstMantissaBit)(mantissaSize-1 downto 0)

    val denormExponent = B(recExponentSize bits, default -> True) ^ firstMantissaBit.asBits.resized
    val recodedExponent = (Mux(isExponentZero, denormExponent, exponent).asUInt +
      ((1 << recExponentSize - 2) | Mux(isExponentZero, U(3), U(1)).resized)).asBits

    val isNaN = recodedExponent(recExponentSize -1 downto recExponentSize - 2) === 3 && !isMantissaZero
    val finalExponent = ((recodedExponent(recExponentSize - 1 downto recExponentSize - 3) &
      B(3 bits, default -> !isZero)) ## recodedExponent(recExponentSize - 4 downto 0)) |
      (isNaN.asBits << (recExponentSize - 3)).resized

    recoded.sign := sign
    recoded.exponent := finalExponent
    recoded.mantissa := Mux(isExponentZero, normalizedMantissa, mantissa)
    recoded
  }

  /** Import number from a Berkeley encoded float number */
  def fromRecFloating(that: RecFloating) = that.toFloating

}

/** Half precision IEEE 754 */
object Floating16 {
  def apply() = Floating(5, 10)
}

/** Single precision IEEE 754 */
object Floating32 {
  def apply() = Floating(8, 23)
}

/** Double precision IEEE 754 */
object Floating64 {
  def apply() = Floating(11, 52)
}

/** Quad precision IEEE 754 */
object Floating128 {
  def apply() = Floating(15, 112)
}

/**
  * Floating point value recoded using Berkeley encoding
  * (see https://github.com/ucb-bar/berkeley-hardfloat)
  * @param exponentSize Recoded exponent size (1 bit wider than the IEEE754 equivalent float)
  * @param mantissaSize Mantissa field size with the implicit one not included
  */
case class RecFloating(exponentSize: Int,
                       mantissaSize: Int) extends Bundle {

  /** Mantissa field without the implicit first bit */
  val mantissa = Bits(mantissaSize bits)

  /** Exponent field (Berkeley encoded) */
  val exponent = Bits(exponentSize bits)

  /** Sign field (true when negative) */
  val sign = Bool

  private def isHighSubnormal = exponent(exponentSize - 3 downto 0).asUInt < 2

  /** Return true if the number is subnormal */
  def isSubnormal = exponent(exponentSize - 1 downto exponentSize - 3) === 1 ||
    (exponent(exponentSize - 1 downto exponentSize - 2) === 1 && isHighSubnormal)

  /** Return true if the number is normal */
  def isNormal = (exponent(exponentSize - 1 downto exponentSize - 2) === 1 && !isHighSubnormal) ||
    exponent(exponentSize - 1 downto exponentSize - 2) === 2

  /** Return true if the number is a special case (NaN or Inf) */
  def isSpecial = exponent(exponentSize - 1 downto exponentSize - 2) === 3

  /** Return true if the number is +- zero */
  def isZero = exponent(exponentSize-1 downto exponentSize-3) === 0

  /** Return true is the value is a NaN */
  def isNaN = isSpecial && exponent(exponentSize - 3)

  /** Return true if the value is a Quiet NaN */
  def isQNaN = isNaN && mantissa(mantissaSize-1)

  /** Return true if the value is a Signalling NaN */
  def isSNaN = isNaN && !mantissa(mantissaSize-1)

  /** Return true if the value is positive */
  def isPositive = !sign

  /* Return true if this value is Infinite */
  def isInfinite = isSpecial && !exponent(exponentSize - 3)

  /** Convert to classic IEEE 754 floating point number */
  def toFloating = {
    val decExponentSize = exponentSize - 1
    val decoded = Floating(decExponentSize, mantissaSize)

    val denormShift = 2 - exponent(log2Up(mantissaSize) - 1 downto 0).asUInt
    val denormMantissaShifted = ((B(1) ## mantissa) >> denormShift)(mantissaSize - 1 downto 0)
    val normalExponent = exponent(exponentSize - 2 downto 0).asUInt - ((1 << (exponentSize - 2)) + 1)

    // Take care of special exponent cases, special is not set for zero
    decoded.exponent := Mux(isNormal, normalExponent.asBits, B(decExponentSize bits, default -> isSpecial)).asBits
    decoded.mantissa := Mux(isNormal || isNaN, mantissa, Mux(isSubnormal, denormMantissaShifted, B(0)))
    decoded.sign := sign
    decoded
  }

  /** Import from an IEEE 754 floating point number */
  def fromFloating(that: Floating) = that.toRecFloating

  /** Import from UInt */
  def fromUInt(that: UInt) = {
    this.sign := False

    val exponentValue = OHToUInt(OHMasking.last(that))
    val normalizationOffset = that.getWidth - exponentValue

    val normalizedInt = (that << normalizationOffset)(that.getWidth-1 downto that.getWidth - mantissaSize)
    this.mantissa := normalizedInt.asBits

    // 0x81 is the reencoded exponent value corresponding to 0 offset in IEEE 754
    val exponent = (U(0x81 + ((1 << (exponentSize - 2)) - 1), exponentSize bits) + exponentValue.resize(exponentSize)).asBits
    val isZero = that.orR
    this.exponent := (exponent(exponent.high) && isZero) ## (exponent(exponent.high - 1) && isZero) ##
      (exponent(exponent.high - 2) && isZero) ##
      exponent(exponent.high - 3 downto 0)

    this
  }


  /**
    * Convert the Floating number to an unsigned integer
    * @param width Width of the output intger
    * @return Unsigned integer corresponding to the floating point value (truncated)
    */
  def toUInt(width: Int): UInt = {
    val isNotZero = exponent(exponentSize-1 downto exponentSize-3).orR
    val extendedMantissa = Bits(width bits)
    extendedMantissa := isNotZero ## mantissa ## B(0, width - mantissaSize - 1 bits)
    val exponentOffset = exponent.asUInt - U(0x81 + ((1 << (exponentSize - 2)) - 1))
    val shift = width - 1 - exponentOffset
    val outputMantissa = (extendedMantissa >> shift)
    outputMantissa.asUInt
  }

  /**
    * Overrides assignment operator
    * @param that Integer number that will be assigned to the converted floating value
    */
  def assignTo(that: UInt): Unit = {
    that := this.toUInt(that.getWidth)
  }

  /** Import from SInt */
  def fromSInt(that: SInt) = {
    this.sign := that(that.getWidth - 1)

    val absValue = that.abs
    val exponentValue = OHToUInt(OHMasking.last(absValue))
    val normalizationOffset = that.getWidth - exponentValue
    val normalizedInt = (absValue << normalizationOffset)(that.getWidth-1 downto that.getWidth - mantissaSize)
    this.mantissa := normalizedInt.asBits

    // 0x81 is the reencoded exponent value corresponding to 0 offset in IEEE 754
    def exponent = (U(0x81 + ((1 << (exponentSize - 2)) - 1), exponentSize bits) + exponentValue.resize(exponentSize)).asBits
    val isZero = that.orR
    this.exponent := (exponent(exponent.high) && isZero) ## (exponent(exponent.high - 1) && isZero) ##
      (exponent(exponent.high - 2) && isZero) ##
      exponent(exponent.high - 3 downto 0)

    this
  }

  /**
    * Convert the Floating number to a signed integer
    * @param width Width ouf the output integer
    * @return Signed integer corresponding to the Floating point value (truncated)
    */
  def toSInt(width: Int): SInt = {
    val isNotZero = exponent(exponentSize-1 downto exponentSize-3).orR
    val extendedMantissa = Bits(32 bits)
    extendedMantissa := isNotZero ## mantissa ## B(0, width - mantissaSize - 1 bits)
    val exponentOffset = exponent.asUInt - U(0x81 + ((1 << (exponentSize - 2)) - 1))
    val shift = 31 - exponentOffset
    val outputMantissa = (extendedMantissa >> shift)
    val signedMantissa = (outputMantissa ^ B(width bits, (default -> sign))).asSInt - sign.asSInt
    signedMantissa
  }

  /**
    * Overrides assignment operator
    * @param that Integer number that will be assigned to the converted floating value
    */
  def assignTo(that: SInt): Unit = {
    that := this.toSInt(that.getWidth)
  }

}

/** Half precision recoded Floating */
object RecFloating16 {
  def apply() = RecFloating(6, 10)
}

/** Single precision recoded Floating */
object RecFloating32 {
  def apply() = RecFloating(9, 23)
}

/** Double precision recoded Floating */
object RecFloating64 {
  def apply() = RecFloating(12, 52)
}

/** Quad precision recoded Floating */
object RecFloating128 {
  def apply() = RecFloating(16, 112)
}
