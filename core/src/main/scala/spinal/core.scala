package spinal

import scala.collection.mutable.ArrayBuffer


package object core extends BaseTypeFactory with BaseTypeCast{
  import languageFeature._
  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls = scala.language.reflectiveCalls
  implicit lazy val postfixOps = scala.language.postfixOps


  implicit def IntToBuilder(value: Int) = new IntBuilder(value)
  implicit def BigIntToBuilder(value: BigInt) = new BigIntBuilder(value)
  implicit def DoubleToBuilder(value: Double) = new DoubleBuilder(value)

  //implicit def EnumElementToCraft[T <: SpinalEnum](element : SpinalEnumElement[T]) : SpinalEnumCraft[T] = element()
//  implicit def EnumElementToCraft[T <: SpinalEnum](enumDef : T) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
//  implicit def EnumElementToCraft2[T <: SpinalEnum](enumDef : SpinalEnumElement[T]) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]

  case class IntBuilder(i: Int) {
//    def x[T <: Data](dataType : T) : Vec[T] = Vec(dataType,i)


    def bit = new BitCount(i)
    def hr = new STime(i * 3600)
    def min = new STime(i * 60)
    def sec = new STime(i * 1)
    def ms = new STime(i * 1e-3)
    def us = new STime(i * 1e-6)
    def ns = new STime(i * 1e-9)
    def ps = new STime(i * 1e-12)
    def fs = new STime(i * 1e-15)
  }

  case class BigIntBuilder(i: BigInt) {
    def bit = new BitCount(i.toInt)
  }

  case class DoubleBuilder(d: Double) {
    def hr = new STime(d * 3600)
    def min = new STime(d * 60)
    def sec = new STime(d * 1)
    def ms = new STime(d * 1e-3)
    def us = new STime(d * 1e-6)
    def ns = new STime(d * 1e-9)
    def ps = new STime(d * 1e-12)
    def fs = new STime(d * 1e-15)
  }

  def True = Bool(true)
  def False = Bool(false)


 // implicit def RegRefToReg[T <: Data](that : RegRef[T]) : T = that.getReg


  implicit def IntToUInt(that: Int) = U(that)
  implicit def BigIntToUInt(that: BigInt) = U(that)

  implicit def IntToSInt(that: Int) = S(that)
  implicit def BigIntToSInt(that: BigInt) = S(that)

  implicit def IntToBits(that: Int) = B(that)
  implicit def BigIntToBits(that: BigInt) = B(that)


  implicit def StringToBits(that: String) = parser(spinal.core.B, that)
  implicit def StringToUInt(that: String) = parser(spinal.core.U, that)
  implicit def StringToSInt(that: String) = parser(spinal.core.S, that)

  implicit class LiteralBuilder(private val sc: StringContext) extends AnyVal {
    def B(args: Any*): Bits = parser(spinal.core.B, getString(args))
    def U(args: Any*): UInt = parser(spinal.core.U, getString(args))
    def S(args: Any*): SInt = parser(spinal.core.S, getString(args))

    def Bits(args: Any*): Bits = B(args)
    def UInt(args: Any*): UInt = U(args)
    def SInt(args: Any*): SInt = S(args)

    private def getString(args: Any*): String = {
     // println(sc.parts.size + " " + args.size)
     // println(sc.parts.head + "-----" + args.head)
    //  sc.standardInterpolator(_.toString(), args)

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)
      while (ai.hasNext) {
        if(ai.hasNext && !ai.next.isInstanceOf[List[_]])bldr append ai.next
        if(pi.hasNext && !pi.next.isInstanceOf[List[_]])bldr append pi.next
      }
      //println(bldr.result)
      bldr.result.replace("_", "")
    }
  }


  private def parser[T <: BitVector](builder: BitVectorLiteralFactory[T], arg: String): T = {
    val argSize = arg.size
    var last = 0;
    var idx = 0
    val split = ArrayBuffer[String]()
    while (idx != argSize) {
      val c = arg.charAt(idx)
      if (!c.isDigit && !c.isUpper) {
        collect
        idx = idx + 1
        collect
      }
      idx = idx + 1
    }
    split += arg.substring(last, argSize)

    split.size match {
      case 1 => {
        val value = BigInt(split(0))
        return builder(value)
      }
      case 2 => {
        val radix = getRadix(split(0))
        val value = BigInt(split(1), radix)
        val minus = split(1).charAt(0) == '-'
        val digitCount = split(1).size - (if(minus) 1 else 0)
        radix match{
          case 16 => return builder(value,digitCount*4 bit)
          case 10 => return builder(value)
          case 8 => return builder(value,digitCount*3 bit)
          case 2 => return builder(value,digitCount bit)
        }
        return ???
      }
      case 3 => {
        val bitCount = split(0).toInt
        val radix = getRadix(split(1))
        val value = BigInt(split(2), radix)
        return builder(value, new BitCount(bitCount))
      }
      case _ =>
    }

    def getRadix(that: String): Int = that match {
      case "x" => 16
      case "h" => 16
      case "d" => 10
      case "o" => 8
      case "b" => 2
      case _ => SpinalError(s"$that is not a valid radix specification. x-d-o-b are allowed")
    }


    def collect: Unit = {
      if (idx == last) return
      split += arg.substring(last, idx)
      last = idx
    }

    return SpinalError(s"$arg literal is not well formed [bitCount][radix]value")
  }

  //implicit def UIntToLitBuilder(sc: StringContext) = new UIntLitBuilder(sc)

  //  implicit def IntToUInt(that : Int) = UInt(that lit)
  //  implicit def BigIntToUInt(that : BigInt) = UInt(that lit)
  //
  // implicit def BooleanToBool(that : Boolean) = Bool(that)
  implicit def DataPimped[T <: Data](that : T) = new DataPimper(that)
  implicit def BitVectorPimped[T <: BitVector](that : T) = new BitVectorPimper(that)

//    implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
//  implicit def autoCast[T <: Data](that: T): T#SSelf = that.asInstanceOf[T#SSelf]


}