package spinal.lib.experimental.math

import spinal.core._

/**
  * Returns absolute value of the floating point number
  */
object FloatingAbs {
  /**
    * Returns the absolute value of an IEEE754 float
    * @param that input value
    * @return outputs absolute value of the input number
    */
  def apply(that: Floating): Floating = {
    val x = cloneOf(that)
    x.sign := False
    x
  }

  /**
    * Returns the absolute value of a recoded float
    * @param that input value
    * @return outputs absolute value of the input number
    */
  def apply(that: RecFloating): RecFloating = {
    val x = cloneOf(that)
    x.sign := False
    x
  }
}

/**
  * Converts Floating to Signed integer
  */
object FloatingToSInt {
  /**
    * Convert the Floating number to a signed integer
    *
    * @param input  Recoded floating point number input
    * @param width  Width ouf the output integer
    * @param offset exponent offset value (0 for integers, decimal position for the fixed)
    * @return Signed integer corresponding to the Floating point value (truncated)
    */
  def apply(input: RecFloating, width: Int, offset: Int): SInt = {
    val isNotZero = input.exponent(input.exponentSize - 1 downto input.exponentSize - 3).orR
    val extendedMantissa = Bits(32 bits)
    extendedMantissa := (isNotZero ## input.mantissa).resizeLeft(width)
    val exponentOffset = input.exponent.asUInt - U(input.getExponentZero + input.getExponentBias)
    val shift = width - 1 - exponentOffset - offset
    val outputMantissa = (extendedMantissa >> shift)
    val signedMantissa = (outputMantissa ^ B(width bits, (default -> input.sign))).asSInt - input.sign.asSInt
    signedMantissa
  }
}

/**
  * Converts floating to Unsigned integer
  */
object FloatingToUInt {
  /**
    * Convert the Floating number to an unsigned integer
    * @param input Recoded floating point number input
    * @param width Width of the output intger
    * @param offset Exponent offset for the integer output
    * @return Unsigned integer corresponding to the floating point value (truncated)
    */
  def apply(input: RecFloating, width: Int, offset: Int): UInt = {
    val isNotZero = input.exponent(input.exponentSize-1 downto input.exponentSize-3).orR
    val extendedMantissa = Bits(width bits)
    extendedMantissa := (isNotZero ## input.mantissa).resizeLeft(width)
    val exponentOffset = input.exponent.asUInt - U(input.getExponentZero + input.getExponentBias)
    val shift = width - 1 - exponentOffset - offset
    val outputMantissa = (extendedMantissa >> shift)
    outputMantissa.asUInt
  }
}
