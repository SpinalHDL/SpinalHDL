package spinal.lib

import spinal.tester.code.SpinalAnyFunSuite

class MiscTester extends SpinalAnyFunSuite{
  import spinal.core.sim._
  import spinal.core._
  import spinal.lib._
  test("napot"){
    SimConfig.doSim(new Component {
      val value = in Bits (8 bits)
      val napot = out(Napot(value))
    }) { dut =>
      for (v <- 0 until 256) {
        dut.value #= v
        sleep(10)
        val dt = dut.napot.toInt
        val id = Integer.numberOfTrailingZeros(~v) min 8
        val ref = ~((1 << 1+id) - 1) & 0x1ff
        assert(dt == ref)
      }
    }
  }
}
