package spinal.lib.experimental.math

import spinal.core._
import spinal.lib._

case class Floating(exponentSize: Int,
                    mantissaSize: Int) extends Bundle {
  val sign = Bool
  val exponent = Bits(exponentSize bits)
  val mantissa = Bits(mantissaSize bits)

  private def isExponentZero = exponent === 0
  private def isMantissaZero = mantissa === 0

  def isZero = isMantissaZero && isExponentZero
  def isPositive = !sign

  def fromBits(word: Bits): Unit = {
    mantissa := word(mantissaSize-1 downto 0)
    exponent := word(mantissaSize+exponentSize-1 downto mantissaSize)
    sign := word(mantissaSize+exponentSize)
  }

  def asRecFloating = {
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
}

case class RecFloating(exponentSize: Int,
                       mantissaSize: Int) extends Bundle {
  val sign = Bool
  val exponent = Bits(exponentSize bits)
  val mantissa = Bits(mantissaSize bits)

  private def isHighSubnormal = exponent(exponentSize - 3 downto 0).asUInt < 2
  def isSubnormal = exponent(exponentSize - 1 downto exponentSize - 3) === 1 ||
    (exponent(exponentSize - 1 downto exponentSize - 2) === 1 && isHighSubnormal)

  def isNormal = (exponent(exponentSize - 1 downto exponentSize - 2) === 1 && !isHighSubnormal) ||
    exponent(exponentSize - 1 downto exponentSize - 2) === 2

  def isSpecial = exponent(exponentSize - 1 downto exponentSize - 2) === 3

  def isZero = exponent(exponentSize-1 downto exponentSize-3) === 0
  def isNaN = isSpecial && exponent(exponentSize - 3)
  def isQNaN = isNaN && mantissa(mantissaSize-1)
  def isSNaN = isNaN && !mantissa(mantissaSize-1)
  def isPositive = !sign
  def isInfinite = isSpecial && !exponent(exponentSize - 3)

  def fromBits(word: Bits): Unit = {
    mantissa := word(mantissaSize-1 downto 0)
    exponent := word(mantissaSize+exponentSize-1 downto mantissaSize)
    sign := word(mantissaSize+exponentSize)
  }

  def asFloating = {
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
}
