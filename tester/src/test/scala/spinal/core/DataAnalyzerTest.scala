package spinal.core

import spinal.tester.SpinalAnyFunSuite

class DataAnalyzerTest extends SpinalAnyFunSuite {

  case class TestModule() extends Module {
    val io = new Bundle {
      val a = in Bits (3 bit)
      val b = out Bits (3 bit)
    }

    val a0 = io.a(0)
    val a1 = RegNext(io.a(1))
    val a2_1 = RegNext(io.a(2))
    val a2 = RegNext(a2_1)

    io.b := (a2, a1, a0).asBits

  }

  test("test fanin") {
    import spinal.lib.tools.DataAnalyzer.toAnalyzer
    val rtl = SpinalVerilog(TestModule())
    val actualFanIn = rtl.toplevel.io.b.getFanIn.toSet
    val expectedFanin = Set(
      rtl.toplevel.a2,
      rtl.toplevel.a1,
      rtl.toplevel.a0
    )
    assert(actualFanIn == expectedFanin)
  }

  test("test fanout") {
    import spinal.lib.tools.DataAnalyzer._
    val rtl = SpinalVerilog(TestModule())
    val actualFanout = rtl.toplevel.io.a.getFanOut.toSet
    val expectedFanout = Set(
      rtl.toplevel.a0,
      rtl.toplevel.a1,
      rtl.toplevel.a2_1
    )
    assert(actualFanout == expectedFanout)
  }

}
