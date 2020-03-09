package spinal.lib.dsptool

case class IntList(list: List[Int])
case class LongList(list: List[Long])
case class DoubleList(list: List[Double])

object IntList{
  implicit def Il(list: List[Int]) = IntList(list)
}
object LongList{
  implicit def Ll(list: List[Long]) = LongList(list)
}
object DoubleList{
  implicit def Dl(list: List[Double]) = DoubleList(list)
}
//object ComplexList{
//  implicit def Cl(list: List[Complex]) = ComplexList(list)
//}
