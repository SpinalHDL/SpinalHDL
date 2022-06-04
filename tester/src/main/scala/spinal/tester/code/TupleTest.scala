package spinal.tester.code

import scala.language._
import spinal.core._

object TupleTest {
  case class tt() extends Module {
    val a = in UInt(3 bit)
    val b = in Bits(14 bit)
    val c = in SInt(16 bit)

    val d, e = out Bits(16 bit)
    val f = Bool()

    val de = (d, e)
    de.setName("de_game")
    (d, e, f) := (a, b, c)
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(tt())
  }
}
