package spinal.tester.code

import scala.language.{postfixOps, _}
import spinal.core._

object TupleTest {
  case class tt() extends Module {
    val a = in UInt(3 bit)
    val b = in Bits(14 bit)
    val c = in SInt(16 bit)

    val d, e = out Bits(16 bit)
    val f = Bits(4 bit)

    val de = (d, e)
    de.setName("de_game")
    // assign {d, e, f} = {2{a}, b, c};
    (d, e, f) := (B(2, a), b, c)
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(tt())
  }
}
