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
import scala.languageFeature._
import spinal.idslplugin.Location


package object core extends BaseTypeFactory with BaseTypeCast {

  import languageFeature.implicitConversions

  type Module = spinal.core.Component
  type dontName = spinal.core.DontName @field

  def Bool : Bool = new Bool
  def Bool(u: DummyTrait = DummyObject) : Bool = new Bool
  def Bool(value: Boolean): Bool = BoolLiteral(value, this.Bool().setAsTypeNode())

  /**
    * Scala implicit
    */
  implicit lazy val implicitConversions : implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls     : reflectiveCalls = scala.language.reflectiveCalls
  implicit lazy val postfixOps          : postfixOps = scala.language.postfixOps

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
    * Implicit senum conversion
    */
  implicit def EnumEtoEnumE2[T <: SpinalEnum, T2 <: T](element: SpinalEnumElement[T2]) : SpinalEnumElement[T] = element.asInstanceOf[SpinalEnumElement[T]]
  implicit def EnumCtoEnumC2[T <: SpinalEnum, T2 <: T](craft: SpinalEnumCraft[T2])     : SpinalEnumCraft[T] = craft.asInstanceOf[SpinalEnumCraft[T]]

  implicit def EnumEtoEnumE3[T <: SpinalEnum, T2 <: T](element: SpinalEnumElement[T]) : SpinalEnumElement[T2] = element.asInstanceOf[SpinalEnumElement[T2]]
  implicit def EnumCtoEnumC3[T <: SpinalEnum, T2 <: T](craft: SpinalEnumCraft[T])     : SpinalEnumCraft[T2] = craft.asInstanceOf[SpinalEnumCraft[T2]]

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

    /**
     * AFix literal builder
     */
    def SQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AF(d, integerWidth, fractionWidth, signed = true)
    def UQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AF(d, integerWidth, fractionWidth, signed = false)
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

    /**
     * AFix literal builder
     */
    def SQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AF(d, integerWidth, fractionWidth, signed = true)
    def UQ(integerWidth: BitCount, fractionWidth: BitCount): AFix = AF(d, integerWidth, fractionWidth, signed = false)
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
    class LList extends ArrayBuffer[Any]{
      def stripMargin = {
        for((e, i) <- this.zipWithIndex) e match{
          case s : String => this(i) = s.stripMargin
          case _ =>
        }
        this
      }
    }
    def L(args: Any*): LList ={
      val ret = new LList()
      for((s,i) <- sc.parts.zipWithIndex){
        ret += s
        if(args.size > i) ret += args(i)
      }
      ret
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
  implicit def BaseTypePimped[T <: BaseType](that: T): BaseTypePimper[T] = new BaseTypePimper(that)
  implicit def VecBitwisePimped[T <: Data with BitwiseOp[T]](that: Vec[T]): VecBitwisePimper[T] = new VecBitwisePimper(that)

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

    def toAFix: AFix = AFix(self)

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

    def toAFix: AFix = AFix(pimped)
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



  implicit def BooleanPimped(that : Boolean) : BooleanPimped = new BooleanPimped(that)
  implicit def IntPimped(that : Int)         : IntPimped = new IntPimped(that)


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
  final def assert(assertion: Boolean, message: => Any)(implicit loc: Location) = scala.Predef.assert(assertion,message)

  def assumeInitial(assertion: Bool)(implicit loc: Location) = AssertStatementHelper(assertion, Nil, ERROR, AssertStatementKind.ASSUME, AssertStatementTrigger.INITIAL, loc)

  def assume(assertion: Bool)(implicit loc: Location) = AssertStatementHelper(assertion, Nil, ERROR, AssertStatementKind.ASSUME, AssertStatementTrigger.CLOCKED, loc)
  def cover(assertion: Bool)(implicit loc: Location) = AssertStatementHelper(assertion, Nil, ERROR, AssertStatementKind.COVER, AssertStatementTrigger.CLOCKED, loc)

  def assert(assertion: Bool)(implicit loc: Location) = AssertStatementHelper(assertion, Nil, FAILURE, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)
  def assert(assertion: Bool, severity: AssertNodeSeverity)(implicit loc: Location) = AssertStatementHelper(assertion, Nil, severity, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)

  def assert(assertion: Bool, message: String)(implicit loc: Location)   = AssertStatementHelper(assertion, message, FAILURE, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)
  def assert(assertion: Bool, message: Seq[Any])(implicit loc: Location) = AssertStatementHelper(assertion, message, FAILURE, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)

  def assert(assertion: Bool, message: String,   severity: AssertNodeSeverity)(implicit loc: Location) = AssertStatementHelper(assertion, message, severity, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)
  def assert(assertion: Bool, message: Seq[Any], severity: AssertNodeSeverity)(implicit loc: Location) = AssertStatementHelper(assertion, message, severity, AssertStatementKind.ASSERT, AssertStatementTrigger.CLOCKED, loc)

//  def apply(cond: Bool)(block: => Unit)(implicit loc: Location): WhenContext = {
//    if(cond.dlcIsEmpty || !cond.head.source.isInstanceOf[Operator.Formal.InitState])
//      cond.setName("when_" + loc.file + "_l" + loc.line, Nameable.REMOVABLE)

  def report(message: String)   = assert(False, message, NOTE)
  def report(message: Seq[Any]) = assert(False, message, NOTE)

  def report(message: String,   severity: AssertNodeSeverity) = assert(False, message, severity)
  def report(message: Seq[Any], severity: AssertNodeSeverity) = assert(False, message, severity)


  class TuplePimperBase(product: Product){
    def elements = product.productIterator.asInstanceOf[Iterator[Data]]
    def := [T<:Data](_right : T): Unit ={
      val right = _right.asBits
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
    def := [T<:Data](_rights : T*): Unit = this := Cat(_rights.reverse)
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
  implicit class Tuple12Pimper(pimped: Tuple12[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple13Pimper(pimped: Tuple13[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple14Pimper(pimped: Tuple14[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple15Pimper(pimped: Tuple15[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple16Pimper(pimped: Tuple16[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple17Pimper(pimped: Tuple17[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple18Pimper(pimped: Tuple18[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple19Pimper(pimped: Tuple19[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple20Pimper(pimped: Tuple20[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple21Pimper(pimped: Tuple21[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)
  implicit class Tuple22Pimper(pimped: Tuple22[Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data]) extends TuplePimperBase(pimped)

  implicit def tupleBunder2Pimp[T1 <: Data,T2 <: Data](pimped: Tuple2[T1, T2]): TupleBundle2[T1, T2] = TupleBundle(pimped._1, pimped._2)
  implicit def tupleBunder3Pimp[T1 <: Data,T2 <: Data,T3 <: Data](pimped: Tuple3[T1, T2, T3]): TupleBundle3[T1, T2, T3] = TupleBundle(pimped._1, pimped._2, pimped._3)
  implicit def tupleBunder4Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data](pimped: Tuple4[T1, T2, T3, T4]): TupleBundle4[T1, T2, T3, T4] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4)
  implicit def tupleBunder5Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data](pimped: Tuple5[T1, T2, T3, T4, T5]): TupleBundle5[T1, T2, T3, T4, T5] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5)
  implicit def tupleBunder6Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data](pimped: Tuple6[T1, T2, T3, T4, T5, T6]): TupleBundle6[T1, T2, T3, T4, T5, T6] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6)
  implicit def tupleBunder7Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data](pimped: Tuple7[T1, T2, T3, T4, T5, T6, T7]): TupleBundle7[T1, T2, T3, T4, T5, T6, T7] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7)
  implicit def tupleBunder8Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data](pimped: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): TupleBundle8[T1, T2, T3, T4, T5, T6, T7, T8] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8)
  implicit def tupleBunder9Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data](pimped: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): TupleBundle9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9)
  implicit def tupleBunder10Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data](pimped: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): TupleBundle10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10)
  implicit def tupleBunder11Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data](pimped: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): TupleBundle11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11)
  implicit def tupleBunder12Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data](pimped: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): TupleBundle12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12)
  implicit def tupleBunder13Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data](pimped: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): TupleBundle13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13)
  implicit def tupleBunder14Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data](pimped: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): TupleBundle14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14)
  implicit def tupleBunder15Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data](pimped: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): TupleBundle15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15)
  implicit def tupleBunder16Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data](pimped: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): TupleBundle16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16)
  implicit def tupleBunder17Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data](pimped: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): TupleBundle17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17)
  implicit def tupleBunder18Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data,T18 <: Data](pimped: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): TupleBundle18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17, pimped._18)
  implicit def tupleBunder19Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data,T18 <: Data,T19 <: Data](pimped: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): TupleBundle19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17, pimped._18, pimped._19)
  implicit def tupleBunder20Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data,T18 <: Data,T19 <: Data,T20 <: Data](pimped: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): TupleBundle20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17, pimped._18, pimped._19, pimped._20)
  implicit def tupleBunder21Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data,T18 <: Data,T19 <: Data,T20 <: Data,T21 <: Data](pimped: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): TupleBundle21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17, pimped._18, pimped._19, pimped._20, pimped._21)
  implicit def tupleBunder22Pimp[T1 <: Data,T2 <: Data,T3 <: Data,T4 <: Data,T5 <: Data,T6 <: Data,T7 <: Data,T8 <: Data,T9 <: Data,T10 <: Data,T11 <: Data,T12 <: Data,T13 <: Data,T14 <: Data,T15 <: Data,T16 <: Data,T17 <: Data,T18 <: Data,T19 <: Data,T20 <: Data,T21 <: Data,T22 <: Data](pimped: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): TupleBundle22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = TupleBundle(pimped._1, pimped._2, pimped._3, pimped._4, pimped._5, pimped._6, pimped._7, pimped._8, pimped._9, pimped._10, pimped._11, pimped._12, pimped._13, pimped._14, pimped._15, pimped._16, pimped._17, pimped._18, pimped._19, pimped._20, pimped._21, pimped._22)

}