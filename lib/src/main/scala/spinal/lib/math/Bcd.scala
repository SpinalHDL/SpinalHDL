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
package spinal.lib.math

import spinal.core._
import spinal.lib._
import scala.collection.Seq


/** Binary-Coded Decimal (BCD) data type, where each decimal digit is stored in 4 bits.
  *
  * @example {{{
  *    val bcd = Bcd(4)                    // 4-digit BCD (0-9999)
  *    val converted = Bcd.fromUInt(myUInt) // Binary to BCD conversion
  *    val binary = bcd.toUInt             // BCD to binary conversion
  *    val sum = bcd1 + bcd2               // BCD addition
  * }}}
  *
  * @see [[https://en.wikipedia.org/wiki/Binary-coded_decimal BCD on Wikipedia]]
  */
case class Bcd(digitCount: Int) extends Bundle {
  require(digitCount >= 1, s"BCD must have at least 1 digit, got $digitCount")

  val digits = Vec(UInt(4 bits), digitCount)

  def bitWidth: Int = digitCount * 4
  def isZero: Bool = digits.map(_ === 0).reduceBalancedTree(_ && _)

  def +(that: Bcd): Bcd = {
    val n = Math.max(this.digitCount, that.digitCount)
    val result = Bcd(n + 1)
    var carry: Bool = False

    for (i <- 0 until n) {
      val a = if (i < this.digitCount) this.digits(i) else U(0, 4 bits)
      val b = if (i < that.digitCount) that.digits(i) else U(0, 4 bits)
      val sum = (a +^ U(6, 4 bits)) + b + carry.asUInt
      result.digits(i) := sum(3 downto 0)
      carry = sum(4)
      when(!carry) { result.digits(i) := (sum - 6).resize(4 bits) }
    }
    result.digits(n) := carry.asUInt.resize(4 bits)
    result
  }

  private def zipDigits(that: Bcd): Seq[(UInt, UInt)] = {
    val zero = spinal.core.U(0, 4 bits)
    this.digits.zipAll(that.digits, zero, zero)
  }

  def leadingZeroes: UInt = {
    digits.reverse
      .foldLeft((U(0, log2Up(digits.length + 1) bits), True)) {
        case ((count, allZero), digit) =>
          val isZero = digit === 0
          val newAllZero = allZero && isZero
          (newAllZero.mux(count + 1, count), newAllZero)
      }._1
  }

  def usedDigits: UInt = U(digits.length) - leadingZeroes

  private def zeros(n: Int): Seq[UInt] = Seq.fill(n)(U(0, 4 bits))

  def <<(n: Int): Bcd = buildFrom(zeros(n) ++ digits)
  def <<(n: UInt): Bcd = {
    val max = n.maxValue.toInt
    val resultLen = digitCount + max
    val padded = Vec(zeros(max) ++ digits ++ zeros(max))
    val w = log2Up(padded.length)
    buildFrom((0 until resultLen).map(i => padded((U(max + i, w bits) - n.resize(w)).resize(w))))
  }

  def >>(n: Int): Bcd = buildFrom(digits.drop(n).padTo(1, U(0, 4 bits)))
  def >>(n: UInt): Bcd = {
    val max = n.maxValue.toInt
    val padded = Vec(digits ++ zeros(max))
    val w = log2Up(padded.length)
    buildFrom((0 until digitCount).map(i => padded((n.resize(w) + U(i, w bits)).resize(w))))
  }

  private def buildFrom(newDigits: Seq[UInt]): Bcd = {
    val r = Bcd(newDigits.length)
    r.digits := Vec(newDigits)
    r
  }

  def @@(that: Bcd): Bcd = buildFrom(that.digits ++ digits)
  def resize(n: Int): Bcd = buildFrom((digits ++ zeros(n)).take(n))
  def lsd: UInt = digits.head
  def msd: UInt = digits.last

  def <(that: Bcd): Bool = {
    zipDigits(that).reverse.foldLeft((False, False)) { case ((decided, result), (a, b)) =>
      val nowDecided = decided || (a =/= b)
      (nowDecided, decided.mux(result, a < b))
    }._2
  }
  def >(that: Bcd): Bool = that < this
  def <=(that: Bcd): Bool = !(this > that)
  def >=(that: Bcd): Bool = !(this < that)
  def ===(that: Bcd): Bool = zipDigits(that).map { case (a, b) => a === b }.reduceBalancedTree(_ && _)
  def =/=(that: Bcd): Bool = !(this === that)

  def apply(offset: UInt, count: Int): Bcd = {
    val max = (1 << offset.getWidth) - 1
    val padded = Vec(digits ++ zeros(max + count))
    val w = log2Up(padded.length)
    val r = Bcd(count)
    r.digits := Vec((0 until count).map(i => padded((offset.resize(w) + U(i, w bits)).resize(w))))
    r
  }

  def toUInt: UInt = {
    val bcdWidth = digitCount * 4
    val binaryWidth = (digitCount * 10 + 2) / 3
    val bcdBits = digits.asBits.asUInt

    val (_, binary) = (0 until binaryWidth).foldLeft((bcdBits, U(0, binaryWidth bits))) {
      case ((bcd, bin), i) =>
        val newBinary = bin | (bcd(0).asUInt << i).resize(binaryWidth bits)
        val shifted = (bcd >> 1).resize(bcdWidth bits)
        val adjusted = shifted.subdivideIn(4 bits).map(d => (d >= 8).mux(d - 3, d))
        (Vec(adjusted).asBits.asUInt, newBinary)
    }
    binary
  }

  override def clone: Bcd = Bcd(digitCount)
}

object Bcd {
  def literal(value: Int): Bcd = {
    require(value >= 0, "BCD only supports non-negative values")
    val numDigits = if (value == 0) 1 else (Math.log10(value).toInt + 1)
    literal(value, numDigits)
  }

  def literal(value: Int, digitCount: Int): Bcd = {
    require(value >= 0, "BCD only supports non-negative values")
    val bcd = new Bcd(digitCount)
    var remaining = value
    for (i <- 0 until digitCount) {
      bcd.digits(i) := spinal.core.U(remaining % 10, 4 bits)
      remaining /= 10
    }
    bcd
  }

  def literal(value: BigInt): Bcd = {
    require(value >= 0, "BCD only supports non-negative values")
    val numDigits = if (value == 0) 1 else value.toString.length
    literal(value, numDigits)
  }

  def literal(value: BigInt, digitCount: Int): Bcd = {
    require(value >= 0, "BCD only supports non-negative values")
    val bcd = new Bcd(digitCount)
    var remaining = value
    for (i <- 0 until digitCount) {
      bcd.digits(i) := spinal.core.U((remaining % 10).toInt, 4 bits)
      remaining /= 10
    }
    bcd
  }

  def fromUInt(value: UInt): Bcd = {
    val numBcdDigits = (value.getWidth * 301 + 999) / 1000 + 1
    val bcdWidth = numBcdDigits * 4

    val result = value.asBools.reverse.foldLeft(spinal.core.U(0, bcdWidth bits)) { (accum, bit) =>
      val digits = accum.subdivideIn(4 bits)
      val adjusted = digits.map(g => (g >= 5).mux(g + 3, g))
      ((Vec(adjusted).asBits.asUInt << 1).resize(bcdWidth bits) | bit.asUInt.resize(bcdWidth bits))
    }

    val bcd = new Bcd(numBcdDigits)
    bcd.digits := result.subdivideIn(4 bits)
    bcd
  }
}
