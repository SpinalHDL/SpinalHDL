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
  /** Sign field (true when negative) */
  val sign = Bool

  /** Exponent field (127 excess encoded) */
  val exponent = Bits(exponentSize bits)

  /** Mantissa field without the implicit first bit */
  val mantissa = Bits(mantissaSize bits)

  private def isExponentZero = exponent === 0
  private def isMantissaZero = mantissa === 0

  /** Return true if number is +/- 0 */
  def isZero = isMantissaZero && isExponentZero

  /** Return true if the number is positive */
  def isPositive = !sign

  /** Import number from a serialized form */
  def fromBits(word: Bits): Unit = {
    mantissa := word(mantissaSize-1 downto 0)
    exponent := word(mantissaSize+exponentSize-1 downto mantissaSize)
    sign := word(mantissaSize+exponentSize)
  }

  /** return this number recoded into Berkeley encoding */
  def toRecFloating = {
    val recExponentSize = exponentSize + 1
    val recoded = RecFloating(recExponentSize, mantissaSize)

    val firstMantissaBit = mantissaSize - OHMasking.first(OHToUInt(mantissa))
    val normalizedMantissa = (mantissa << firstMantissaBit)(mantissaSize-1 downto 0)

    val denormExponent = B(recExponentSize bits, default -> True) ^ firstMantissaBit.asBits
    val recodedExponent = (Mux(isExponentZero, denormExponent, exponent).asUInt +
      ((1 << recExponentSize - 2) | Mux(isExponentZero, U(3), U(1)))).asBits

    val isNaN = recodedExponent(recExponentSize -1 downto recExponentSize - 2) === 3 && !isMantissaZero
    val finalExponent = ((recodedExponent(recExponentSize - 1 downto recExponentSize - 3) &
      B(3 bits, default -> !isZero)) ## recodedExponent(recExponentSize - 4 downto 0)) |
      isNaN.asBits << (recExponentSize - 3)

    recoded.sign := sign
    recoded.exponent := finalExponent
    recoded.mantissa := Mux(isExponentZero, normalizedMantissa, mantissa)
    recoded
  }

  /** Import number from a Berkeley encoded float number */
  def fromRecFloating(that: RecFloating) = that.toFloating

}

/**
  * Floating point value recoded using Berkeley encoding
  * (see https://github.com/ucb-bar/berkeley-hardfloat)
  * @param exponentSize Recoded exponent size (1 bit wider than the IEEE754 equivalent float)
  * @param mantissaSize Mantissa field size with the implicit one not included
  */
case class RecFloating(exponentSize: Int,
                       mantissaSize: Int) extends Bundle {

  /** Sign field (true when negative) */
  val sign = Bool

  /** Exponent field (Berkeley encoded) */
  val exponent = Bits(exponentSize bits)

  /** Mantissa field without the implicit first bit */
  val mantissa = Bits(mantissaSize bits)

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

  /** Import number from a bit string */
  def fromBits(word: Bits): Unit = {
    mantissa := word(mantissaSize-1 downto 0)
    exponent := word(mantissaSize+exponentSize-1 downto mantissaSize)
    sign := word(mantissaSize+exponentSize)
  }

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
}
