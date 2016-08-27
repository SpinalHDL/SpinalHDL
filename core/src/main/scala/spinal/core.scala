package spinal

import scala.annotation.elidable
import scala.annotation.elidable._
import scala.collection.immutable.Range
import scala.language.experimental.macros

package object core extends BaseTypeFactory with BaseTypeCast {

  import languageFeature.implicitConversions

  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls = scala.language.reflectiveCalls
  implicit lazy val postfixOps = scala.language.postfixOps

  implicit def IntToBuilder(value: Int) : IntBuilder = new IntBuilder(value)

  implicit def BigIntToBuilder(value: BigInt) : BigIntBuilder = new BigIntBuilder(value)

  implicit def DoubleToBuilder(value: Double) : DoubleBuilder = new DoubleBuilder(value)

  def enum(param: Symbol*): Any = macro MacroTest.enum_impl
  //def enum(param: Symbol*) = MacroTest.enum(param)

  implicit def EnumEtoEnumE2[T <: SpinalEnum,T2 <: T](element : SpinalEnumElement[T2])  = element.asInstanceOf[SpinalEnumElement[T]]
  implicit def EnumCtoEnumC2[T <: SpinalEnum,T2 <: T](craft : SpinalEnumCraft[T2])  = craft.asInstanceOf[SpinalEnumCraft[T]]

  implicit def EnumEtoEnumE3[T <: SpinalEnum,T2 <: T](element : SpinalEnumElement[T])  = element.asInstanceOf[SpinalEnumElement[T2]]
  implicit def EnumCtoEnumC3[T <: SpinalEnum,T2 <: T](craft : SpinalEnumCraft[T])  = craft.asInstanceOf[SpinalEnumCraft[T2]]


  implicit def EnumElementToCraft[T <: SpinalEnum](element: SpinalEnumElement[T]): SpinalEnumCraft[T] = element()
  //  implicit def EnumElementToCraft[T <: SpinalEnum](enumDef : T) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
  //  implicit def EnumElementToCraft2[T <: SpinalEnum](enumDef : SpinalEnumElement[T]) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]

  class IntBuilder(val i: Int) extends AnyVal {
    //    def x[T <: Data](dataType : T) : Vec[T] = Vec(dataType,i)

    def downto(start: Int): Range.Inclusive = Range.inclusive(i,start,-1)

    def bit = new BitCount(i)
    def bits = new BitCount(i)
    def exp = new ExpNumber(i)
    def pos = new PosCount(i)

    def hr = (i * BigDecimal(3600.0))
    def mn = (i * BigDecimal(60.0))
    def sec = (i * BigDecimal(1.0))
    def ms = (i * BigDecimal(1e-3))
    def us = (i * BigDecimal(1e-6))
    def ns = (i * BigDecimal(1e-9))
    def ps = (i * BigDecimal(1e-12))
    def fs = (i * BigDecimal(1e-15))


    def THz = (i * BigDecimal(1e12))
    def GHz = (i * BigDecimal(1e9))
    def MHz = (i * BigDecimal(1e6))
    def kHz = (i * BigDecimal(1e3))
    def Hz = (i * BigDecimal(1e0))

    def Byte = BigInt(i)
    def kB = BigInt(i) << 10
    def MB = BigInt(i) << 20
    def GB = BigInt(i) << 30
    def TB = BigInt(i) << 40

  }

  case class BigIntBuilder(i: BigInt) {
    def bit = new BitCount(i.toInt)
    def exp = new ExpNumber(i.toInt)
    def pos = new PosCount(i.toInt)
  }

  case class DoubleBuilder(d: Double) {
    def hr =  (d  * BigDecimal(3600.0))
    def mn = (d * BigDecimal(60.0))
    def sec = (d * BigDecimal(1.0))
    def ms =  (d  * BigDecimal(1e-3))
    def us =  (d  * BigDecimal(1e-6))
    def ns =  (d  * BigDecimal(1e-9))
    def ps =  (d  * BigDecimal(1e-12))
    def fs =  (d  * BigDecimal(1e-15))


    def THz = (d * BigDecimal(1e12))
    def GHz = (d * BigDecimal(1e9))
    def MHz = (d * BigDecimal(1e6))
    def kHz = (d * BigDecimal(1e3))
    def  Hz = (d * BigDecimal(1e0))
  }

  implicit class BigDecimalBuilder(d: BigDecimal) {
    def hr =  (d  * BigDecimal(3600.0))
    def mn = (d * BigDecimal(60.0))
    def sec = (d * BigDecimal(1.0))
    def ms =  (d  * BigDecimal(1e-3))
    def us =  (d  * BigDecimal(1e-6))
    def ns =  (d  * BigDecimal(1e-9))
    def ps =  (d  * BigDecimal(1e-12))
    def fs =  (d  * BigDecimal(1e-15))


    def THz = (d * BigDecimal(1e12))
    def GHz = (d * BigDecimal(1e9))
    def MHz = (d * BigDecimal(1e6))
    def kHz = (d * BigDecimal(1e3))
    def  Hz = (d * BigDecimal(1e0))
  }

  def True = Bool(true) //Should be def, not val, else it will create cross hierarchy usage of the same instance
  def False = Bool(false)

  // implicit def RegRefToReg[T <: Data](that : RegRef[T]) : T = that.getReg
  implicit def IntToUInt(that: Int) : UInt = U(that)
  implicit def BigIntToUInt(that: BigInt) : UInt = U(that)
  implicit def IntToSInt(that: Int) : SInt = S(that)
  implicit def BigIntToSInt(that: BigInt) : SInt = S(that)
  implicit def IntToBits(that: Int) : Bits = B(that)
  implicit def BigIntToBits(that: BigInt) : Bits = B(that)
  implicit def StringToBits(that: String) : Bits = bitVectorStringParser(spinal.core.B, that, signed = false)
  implicit def StringToUInt(that: String) : UInt = bitVectorStringParser(spinal.core.U, that, signed = false)
  implicit def StringToSInt(that: String) : SInt = bitVectorStringParser(spinal.core.S, that, signed = true)

  implicit class LiteralBuilder(private val sc: StringContext) {
    def B(args: Any*): Bits = bitVectorStringParser(spinal.core.B, getString(args), signed = false)
    def U(args: Any*): UInt = bitVectorStringParser(spinal.core.U, getString(args), signed = false)
    def S(args: Any*): SInt = bitVectorStringParser(spinal.core.S, getString(args), signed = true)
    def M(args: Any*): MaskedLiteral = MaskedLiteral(sc.parts(0))
    def Bits(args: Any*): Bits = B(args)
    def UInt(args: Any*): UInt = U(args)
    def SInt(args: Any*): SInt = S(args)

    private def getString(args: Any*): String = {
      // println(sc.parts.size + " " + args.size)
      // println(sc.parts.head + "-----" + args.head)
      // sc.standardInterpolator(_.toString(), args)

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      //println(bldr.result)
      bldr.result.replace("_", "")
    }
  }

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

    var bitCount = -1

    if (str.contains(''')) {
      val split = str.split(''')
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
        case c => SpinalError(s"$c is not a valid radix specification. x-d-o-b are allowed")
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
      case 8 => digitCount * 3
      case 2 => digitCount
      case _ => -1
    }

    val value = radix match {
      case 10 => if (minus) -BigInt(str, radix) else BigInt(str, radix)
      case _ => strBinToInt(str, radix, bitCount)
    }

    if (bitCount == -1) {
      builder(value)
    } else {
      builder(value, bitCount bit)
    }
  }

  implicit def DataPimped[T <: Data](that: T) : DataPimper[T] = new DataPimper(that)

  // @TODO Should those be removed ?
  // implicit def UIntToLitBuilder(sc: StringContext) = new UIntLitBuilder(sc)
  // implicit def IntToUInt(that : Int) = UInt(that lit)
  // implicit def BigIntToUInt(that : BigInt) = UInt(that lit)
  // implicit def BooleanToBool(that : Boolean) = Bool(that)
  // implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
  // implicit def autoCast[T <: Data](that: T): T#SSelf = that.asInstanceOf[T#SSelf]

  implicit class SIntPimper(pimped: SInt) {
    def toSFix: SFix = {
      val width = pimped.getWidth
      val fix = SFix(width - 1 exp, width bit)
      fix.raw := pimped
      fix
    }
  }

  implicit class UIntPimper(pimped: UInt) {
    def toUFix: UFix = {
      val width = pimped.getWidth
      val fix = UFix(width exp, width bit)
      fix.raw := pimped
      fix
    }
  }

  implicit class RangePimper(pimped: Range) {
    def high: Int = {
      if(pimped.step > 0)
        pimped.end + (if(pimped.isInclusive) 0 else -1)
      else
        pimped.start
    }

    def low: Int = {
      if(pimped.step < 0)
        pimped.end + (if(pimped.isInclusive) 0 else 1)
      else
        pimped.start
    }

  }


  @elidable(ASSERTION)
  def assert(assertion: Boolean) = scala.Predef.assert(assertion)

  @elidable(ASSERTION) @inline
  final def assert(assertion: Boolean, message: => Any) = scala.Predef.assert(assertion,message)

  def assert(assertion: Bool) = AssertNode(assertion,null,ERROR)
  def assert(assertion: Bool,message : String) = AssertNode(assertion,message,ERROR)
  def assert(assertion: Bool,severity: AssertNodeSeverity) = AssertNode(assertion,null,severity)
  def assert(assertion: Bool,message : String,severity: AssertNodeSeverity) = AssertNode(assertion,message,severity)
  def report(message : String,severity: AssertNodeSeverity=NOTE) = assert(True,message,severity)

}