package object spinal {
  implicit def IntToBuilder(value: Int) = new IntBuilder(value)

  case class IntBuilder(i: Int) {
    def bit = new BitCount(i)
  }
}