package spinal

import scala.collection.mutable.ArrayBuffer


package object core {
  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls = scala.language.reflectiveCalls
  implicit lazy val postfixOps = scala.language.postfixOps


  //  implicit def implicitLiteralInt(value: Int) = new LiteralInt(value)
  //  implicit def implicitLiteralInt(value: BigInt) = new LiteralInt(value)
  implicit def IntToBuilder(value: Int) = new IntBuilder(value)
  implicit def BigIntToBuilder(value: BigInt) = new BigIntBuilder(value)

  // implicit def EnumElementToCraft[T <: SpinalEnum](element : SpinalEnumElement[T]) : SpinalEnumCraft[T] = element()

  case class IntBuilder(i: Int) {
    def bit = new BitCount(i)
    def lit = new LiteralInt(i)
  }

  case class BigIntBuilder(i: BigInt) {
    def bit = new BitCount(i.toInt)
    def lit = new LiteralInt(i)
  }


  def True = Bool(true)
  def False = Bool(false)


 // implicit def RegRefToReg[T <: Data](that : RegRef[T]) : T = that.getReg


  implicit def IntToUInt(that: Int) = u(that)
  implicit def BigIntToUInt(that: BigInt) = u(that)

  implicit def IntToSInt(that: Int) = s(that)
  implicit def BigIntToSInt(that: BigInt) = s(that)

  implicit def IntToBits(that: Int) = b(that)
  implicit def BigIntToBits(that: BigInt) = b(that)


  implicit def StringToBits(that: String) = parser(spinal.core.b, that)
  implicit def StringToUInt(that: String) = parser(spinal.core.u, that)
  implicit def StringToSInt(that: String) = parser(spinal.core.s, that)

  implicit class LiteralBuilder(private val sc: StringContext) extends AnyVal {
    def b(args: Any*): Bits = parser(spinal.core.b, getString(args))
    def u(args: Any*): UInt = parser(spinal.core.u, getString(args))
    def s(args: Any*): SInt = parser(spinal.core.s, getString(args))

    def Bits(args: Any*): Bits = b(args)
    def UInt(args: Any*): UInt = u(args)
    def SInt(args: Any*): SInt = s(args)

    private def getString(args: Any*): String = {
      println(sc.parts.size + " " + args.size)
      println(sc.parts.head + "-----" + args.head)
    //  sc.standardInterpolator(_.toString(), args)

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)
      while (ai.hasNext) {
        if(ai.hasNext && !ai.next.isInstanceOf[List[_]])bldr append ai.next
        if(pi.hasNext && !pi.next.isInstanceOf[List[_]])bldr append pi.next
      }
      println(bldr.result)
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
        return builder(value)
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
}