package landa

import spinal.core.SimManagedVerilator


object DebugTest {
  object Rtl {
    import spinal.core._
    class Dut extends Component {
      val io = new Bundle {
        val a, b, c = in UInt (7 bits)
        val result = out UInt (7 bits)
      }
      io.result := RegNext(io.a + io.b - io.c)
    }
  }

  def main(args: Array[String]): Unit = {
    import spinal.core.SimManagedApi._
    SimManagedVerilator(new Rtl.Dut) { dut =>
      println(dut.io.a.toLong)
      dut.io.a #= 42l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
      dut.io.a #= 54l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
    }
  }
}
