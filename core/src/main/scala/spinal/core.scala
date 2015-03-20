package spinal


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



//  implicit def IntToUInt(that : Int) = UInt(that lit)
//  implicit def BigIntToUInt(that : BigInt) = UInt(that lit)
//
 // implicit def BooleanToBool(that : Boolean) = Bool(that)
}