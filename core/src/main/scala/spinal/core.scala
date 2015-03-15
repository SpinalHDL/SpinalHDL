package spinal


package object core {
  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls = scala.language.reflectiveCalls
  implicit lazy val postfixOps = scala.language.postfixOps



  implicit def IntToBuilder(value: Int) = new IntBuilder(value)
 // implicit def EnumElementToCraft[T <: SpinalEnum](element : SpinalEnumElement[T]) : SpinalEnumCraft[T] = element()

  case class IntBuilder(i: Int) {
    def bit = new BitCount(i)
  }

}