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
package spinal

import spinal.core.internals._

import scala.annotation.elidable
import scala.annotation.elidable._
import scala.annotation.meta.field
import scala.collection.immutable.Range
import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros


package object core extends BaseTypeFactory with BaseTypeCast {

  import languageFeature.implicitConversions


  type dontName = spinal.core.DontName @field

  /**
    * Scala implicit
    */
  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls     = scala.language.reflectiveCalls
  implicit lazy val postfixOps          = scala.language.postfixOps

  /** Implicit clause builder for `elseWhen` */
  implicit class ElseWhenClauseBuilder(cond: Bool){
    def apply(block: => Unit): ElseWhenClause = new ElseWhenClause(cond, block)
  }


  /**
    * Implicit Int/BigInt/Double to Builder
    */
  implicit def IntToBuilder(value: Int): IntBuilder = new IntBuilder(value)
  implicit def BigIntToBuilder(value: BigInt): BigIntBuilder = new BigIntBuilder(value)
  implicit def DoubleToBuilder(value: Double): DoubleBuilder = new DoubleBuilder(value)


  /**
    * Implicit enum conversion
    */
  implicit def EnumEtoEnumE2[T <: SpinalEnum, T2 <: T](element: SpinalEnumElement[T2]) = element.asInstanceOf[SpinalEnumElement[T]]
  implicit def EnumCtoEnumC2[T <: SpinalEnum, T2 <: T](craft: SpinalEnumCraft[T2])     = craft.asInstanceOf[SpinalEnumCraft[T]]

  implicit def EnumEtoEnumE3[T <: SpinalEnum, T2 <: T](element: SpinalEnumElement[T]) = element.asInstanceOf[SpinalEnumElement[T2]]
  implicit def EnumCtoEnumC3[T <: SpinalEnum, T2 <: T](craft: SpinalEnumCraft[T])     = craft.asInstanceOf[SpinalEnumCraft[T2]]

  implicit def EnumElementToCraft[T <: SpinalEnum](element: SpinalEnumElement[T]): SpinalEnumCraft[T] = element()

  val  DefaultFixPointConfig = FixPointConfig(RoundType.ROUNDTOINF, false)
  val  LowCostFixPointConfig = FixPointConfig(RoundType.ROUNDUP, true)
  /**
    * Integer Builder
    */
  class IntBuilder(val i: Int) extends AnyVal {

    def downto(start: Int): Range.Inclusive = Range.inclusive(i, start, -1)

    def bit    = new BitCount(i)
    def bits   = new BitCount(i)
    def exp    = new ExpNumber(i)
    def pos    = new PosCount(i)
    def slices = new SlicesCount(i)

    /** Time */
    def hr  = TimeNumber(i * BigDecimal(3600.0))
    def mn  = TimeNumber(i * BigDecimal(60.0))
    def sec = TimeNumber(i * BigDecimal(1.0))
    def ms  = TimeNumber(i * BigDecimal(1e-3))
    def us  = TimeNumber(i * BigDecimal(1e-6))
    def ns  = TimeNumber(i * BigDecimal(1e-9))
    def ps  = TimeNumber(i * BigDecimal(1e-12))
    def fs  = TimeNumber(i * BigDecimal(1e-15))

    /** Frequency */
    def THz = HertzNumber(i * BigDecimal(1e12))
    def GHz = HertzNumber(i * BigDecimal(1e9))
    def MHz = HertzNumber(i * BigDecimal(1e6))
    def kHz = HertzNumber(i * BigDecimal(1e3))
    def Hz  = HertzNumber(i * BigDecimal(1e0))

    /** Size */
    def Byte   = BigInt(i)
    def Bytes  = BigInt(i)
    def KiB    = BigInt(i) << 10
    def MiB    = BigInt(i) << 20
    def GiB    = BigInt(i) << 30
    def TiB    = BigInt(i) << 40
    def PiB    = BigInt(i) << 50
    def EiB    = BigInt(i) << 60
    def ZiB    = BigInt(i) << 70
    def YiB    = BigInt(i) << 80

    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def kB     = BigInt(i) << 10
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def MB     = BigInt(i) << 20
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def GB     = BigInt(i) << 30
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def TB     = BigInt(i) << 40


    /** Number of cycles */
    def cycles = new CyclesCount(i)
  }


  /**
    * BigInt Builder
    */
  case class BigIntBuilder(i: BigInt) {

    def bit    = new BitCount(i.toInt)
    def bits   = new BitCount(i.toInt)
    def exp    = new ExpNumber(i.toInt)
    def pos    = new PosCount(i.toInt)
    def slices = new SlicesCount(i.toInt)

    /** Size */
    def Byte = (i)
    def Bytes = (i)
    def KiB  = (i) << 10
    def MiB  = (i) << 20
    def GiB  = (i) << 30
    def TiB  = (i) << 40
    def PiB  = (i) << 50
    def EiB  = (i) << 60
    def ZiB  = (i) << 70
    def YiB  = (i) << 80

    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def kB   = (i) << 10
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def MB   = (i) << 20
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def GB   = (i) << 30
    @deprecated("Deprecated in favor of IEC units", "SpinalHDL 1.3.1")
    def TB   = (i) << 40

    /** Number of cycles */
    def cycles = new CyclesCount(i)
  }


  /**
    * Double Builder
    */
  case class DoubleBuilder(d: Double) {

    /** Time */
    def hr  = TimeNumber(d * BigDecimal(3600.0))
    def mn  = TimeNumber(d * BigDecimal(60.0))
    def sec = TimeNumber(d * BigDecimal(1.0))
    def ms  = TimeNumber(d * BigDecimal(1e-3))
    def us  = TimeNumber(d * BigDecimal(1e-6))
    def ns  = TimeNumber(d * BigDecimal(1e-9))
    def ps  = TimeNumber(d * BigDecimal(1e-12))
    def fs  = TimeNumber(d * BigDecimal(1e-15))

    /** Frequency */
    def THz = HertzNumber(d * BigDecimal(1e12))
    def GHz = HertzNumber(d * BigDecimal(1e9))
    def MHz = HertzNumber(d * BigDecimal(1e6))
    def kHz = HertzNumber(d * BigDecimal(1e3))
    def  Hz = HertzNumber(d * BigDecimal(1e0))
  }


  /**
    * BigDecimal Builder
    */
  implicit class BigDecimalBuilder(d: BigDecimal) {

    /** Time */
    def hr  = TimeNumber(d  * BigDecimal(3600.0))
    def mn  = TimeNumber(d * BigDecimal(60.0))
    def sec = TimeNumber(d * BigDecimal(1.0))
    def ms  = TimeNumber(d  * BigDecimal(1e-3))
    def us  = TimeNumber(d  * BigDecimal(1e-6))
    def ns  = TimeNumber(d  * BigDecimal(1e-9))
    def ps  = TimeNumber(d  * BigDecimal(1e-12))
    def fs  = TimeNumber(d  * BigDecimal(1e-15))

    /** Frequency */
    def THz = HertzNumber(d * BigDecimal(1e12))
    def GHz = HertzNumber(d * BigDecimal(1e9))
    def MHz = HertzNumber(d * BigDecimal(1e6))
    def kHz = HertzNumber(d * BigDecimal(1e3))
    def  Hz = HertzNumber(d * BigDecimal(1e0))
  }


  /**
    * True / False definition
    */
  def True  = Bool(true) //Should be def, not val, else it will create cross hierarchy usage of the same instance
  def False = Bool(false)


  /**
    * Implicit conversion from Int/BigInt/String to UInt/SInt/Bits
    */
  implicit def IntToUInt(that: Int): UInt = U(that)
  implicit def LongToUInt(that: Long): UInt = U(that)
  implicit def BigIntToUInt(that: BigInt): UInt = U(that)
  implicit def IntToSInt(that: Int): SInt = S(that)
  implicit def LongToSInt(that: Long): SInt = S(that)
  implicit def BigIntToSInt(that: BigInt): SInt = S(that)
  implicit def IntToBits(that: Int): Bits = B(that)
  implicit def LongToBits(that: Long): Bits = B(that)
  implicit def BigIntToBits(that: BigInt): Bits = B(that)


  /**
    * Literal builder S/U/B"[[size']base]value”
    *
    * e.g. : B"8'xFF"
    */
  implicit class LiteralBuilder(private val sc: StringContext) {

    def B(args: Any*): Bits = bitVectorStringParser(spinal.core.B, getString(args), signed = false)
    def U(args: Any*): UInt = bitVectorStringParser(spinal.core.U, getString(args), signed = false)
    def S(args: Any*): SInt = bitVectorStringParser(spinal.core.S, getString(args), signed = true)
    def M(args: Any*): MaskedLiteral = MaskedLiteral(sc.parts(0))
    def L(args: Any*): List[Any] ={
      val ret = ArrayBuffer[Any]()
      for((s,i) <- sc.parts.zipWithIndex){
        ret += s
        if(args.size > i) ret += args(i)
      }
      ret.toList
    }

    def Bits(args: Any*): Bits = B(args)
    def UInt(args: Any*): UInt = U(args)
    def SInt(args: Any*): SInt = S(args)

    private def getString(args: Any*): String = {

      val pi   = sc.parts.iterator
      val ai   = args.iterator
      val bldr = new StringBuilder(pi.next().toString)

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      bldr.result.replace("_", "")
    }
  }

  /**
    * Parsing the bitVector literal and building a Spinal BitVector
    */
  private[core] def bitVectorStringParser[T <: BitVector](builder: BitVectorLiteralFactory[T], arg: String, signed: Boolean): T = {

    def error() = SpinalError(s"$arg literal is not well formed [bitCount'][radix]value")

    def strBinToInt(valueStr: String, radix: Int, bitCount: Int) = if (!signed) {
      BigInt(valueStr, radix)
    } else {
      val v = BigInt(valueStr, radix)
      val bitCountPow2 = BigInt(1) << bitCount
      if (v >= bitCountPow2) SpinalError("Value is bigger than bit count")
      if (!v.testBit(bitCount - 1)) v else -bitCountPow2 + v
    }

    var str = arg.replace("_", "").toLowerCase
    if (str == "") return builder(0, 0 bit)

    var bitCount: Int = -1

    if (str.contains('\'')) {
      val split = str.split('\'')
      bitCount = split(0).toInt
      str = split(1)
    }

    var radix = -1

    if ("01".contains(str.charAt(0))) {
      radix = 2
    } else {
      radix = str.charAt(0) match {
        case 'x' => 16
        case 'h' => 16
        case 'd' => 10
        case 'o' => 8
        case 'b' => 2
        case c   => SpinalError(s"$c is not a valid radix specification. x-d-o-b are allowed")
      }
      str = str.tail
    }

    val minus = if (str.charAt(0) == '-') {
      str = str.tail
      if (radix != 10) SpinalError("Can't have minus on non decimal values")
      true
    } else {
      false
    }

    val digitCount = str.length
    if (bitCount == -1) bitCount = radix match {
      case 16 => digitCount * 4
      case 8  => digitCount * 3
      case 2  => digitCount
      case _  => -1
    }

    val value = radix match {
      case 10 => if (minus) -BigInt(str, radix) else BigInt(str, radix)
      case _  => strBinToInt(str, radix, bitCount)
    }

    if (bitCount == -1) {
      builder(value)
    } else {
      builder(value, bitCount bit)
    }
  }

  /**
    * Implicit Data helper
    */
  implicit def DataPimped[T <: Data](that: T): DataPimper[T] = new DataPimper(that)

  /**
    * Implicit SInt helper
    */
  implicit class SIntPimper(self: SInt) {
    def toSFix: SFix = {
      val width = self.getWidth
      val fix = SFix(width - 1 exp, width bit)
      fix.raw := self
      fix
    }
    /**
      * Absolute value of a SInt
      * @example {{{ myUInt := mySInt.abs }}}
      * @return a UInt assign with the absolute value of the SInt
      */
    def abs: UInt = Mux(self.msb, ~self, self).asUInt + self.msb.asUInt
    /** Return the absolute value of the SInt when enable is True */
    def abs(enable: Bool): UInt = Mux(self.msb && enable, ~self, self).asUInt + (self.msb && enable).asUInt
    /** symmetric abs
      * @example {{{ S(-128,8 bits).absWithSym got U(127,7 bits) }}}
      * @return a UInt assign with the absolute value save 1 bit
      */
    def absWithSym: UInt = self.symmetry.abs.resize(self.getWidth-1)
  }

  /**
    * Implicit UInt helper
    */
  implicit class UIntPimper(pimped: UInt) {
    def toUFix: UFix = {
      val width = pimped.getWidth
      val fix = UFix(width exp, width bit)
      fix.raw := pimped
      fix
    }
  }

  /**
    * Implicit Range helper
    */
  implicit class RangePimper(pimped: Range) {
    def high: Int = {
      if(pimped.step > 0) {
        pimped.end + (if (pimped.isInclusive) 0 else -1)
      }else {
        pimped.start
      }
    }

    def low: Int = {
      if(pimped.step < 0) {
        pimped.end + (if (pimped.isInclusive) 0 else 1)
      }else {
        pimped.start
      }
    }

  }



  implicit def BooleanPimped[T <: Data](that : Boolean) = new BooleanPimped(that)


  //For backward compatibility
  type IClockDomainFrequency = ClockDomain.ClockFrequency
  type FixedFrequency = ClockDomain.FixedFrequency
  type UnknownFrequency = ClockDomain.UnknownFrequency
  def FixedFrequency(value: HertzNumber) = ClockDomain.FixedFrequency(value)
  def UnknownFrequency() = ClockDomain.UnknownFrequency()



  /**
    * Assertion
    */
  @elidable(ASSERTION)
  def assert(assertion: Boolean) = scala.Predef.assert(assertion)

  @elidable(ASSERTION) @inline
  final def assert(assertion: Boolean, message: => Any) = scala.Predef.assert(assertion,message)


  def assume(assertion: Bool) = AssertStatementHelper(assertion, Nil, ERROR, AssertStatementKind.ASSUME)
  def cover(assertion: Bool) = AssertStatementHelper(assertion, Nil, ERROR, AssertStatementKind.COVER)

  def assert(assertion: Bool) = AssertStatementHelper(assertion, Nil, FAILURE, AssertStatementKind.ASSERT)
  def assert(assertion: Bool, severity: AssertNodeSeverity) = AssertStatementHelper(assertion, Nil, severity, AssertStatementKind.ASSERT)

  def assert(assertion: Bool, message: String)   = AssertStatementHelper(assertion, message, FAILURE, AssertStatementKind.ASSERT)
  def assert(assertion: Bool, message: Seq[Any]) = AssertStatementHelper(assertion, message, FAILURE, AssertStatementKind.ASSERT)

  def assert(assertion: Bool, message: String,   severity: AssertNodeSeverity) = AssertStatementHelper(assertion, message, severity, AssertStatementKind.ASSERT)
  def assert(assertion: Bool, message: Seq[Any], severity: AssertNodeSeverity) = AssertStatementHelper(assertion, message, severity, AssertStatementKind.ASSERT)

  def report(message: String)   = assert(False, message, NOTE)
  def report(message: Seq[Any]) = assert(False, message, NOTE)

  def report(message: String,   severity: AssertNodeSeverity) = assert(False, message, severity)
  def report(message: Seq[Any], severity: AssertNodeSeverity) = assert(False, message, severity)


  class TuplePimperBase(product: Product){
    def elements = product.productIterator.asInstanceOf[Iterator[Data]]
    def := (right : Bits): Unit ={
      val leftWidth = elements.map(_.getBitsWidth).sum
      var rightOp = right
      if(right.hasTag(tagAutoResize)){
        rightOp = right.resize(leftWidth bits)
      }
      assert(rightOp.getWidth == leftWidth, s"Width missmatch (${right.getWidth} != $leftWidth")
      var offset = 0
      for(e <- elements.toList.reverse){
        e.assignFromBits(rightOp(offset, e.getBitsWidth bits))
        offset += e.getBitsWidth
      }
    }

    def asBits = {
      Cat(elements.toList.reverse)
    }
  }

  implicit class Tuple2Pimper(pimped : Tuple2[Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple3Pimper(pimped : Tuple3[Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple4Pimper(pimped : Tuple4[Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple5Pimper(pimped : Tuple5[Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple6Pimper(pimped : Tuple6[Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple7Pimper(pimped : Tuple7[Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple8Pimper(pimped : Tuple8[Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple9Pimper(pimped : Tuple9[Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple10Pimper(pimped : Tuple10[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple11Pimper(pimped : Tuple11[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
}