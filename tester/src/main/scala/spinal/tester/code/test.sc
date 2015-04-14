
import spinal.core._

U(2)
U"x1492"
Bool(true)


spinal.core.log2Up(0)
spinal.core.log2Up(1)
spinal.core.log2Up(2)
spinal.core.log2Up(3)
BigInt(1).bitLength
BigInt(0).bitLength
BigInt(-1).bitLength
BigInt(-2).bitLength
class BBB {
  def apply(blocks: (Int,() => Unit)*): Unit = {
    blocks.foreach(_._2)
  }
  def __(b : BBB) : BBB = this
}
val b = new BBB
b __ b __ b
//BBB(1 -> () => {
//  println("BBB block hallo1")
//},() => 2 -> {
//  println("BBB block hallo2")
//})
