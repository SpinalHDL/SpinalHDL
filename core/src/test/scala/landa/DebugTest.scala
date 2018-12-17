package landa

import spinal.core._
import spinal.sim._
import spinal.core.sim._




object DebugTest2 {

  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (7 bits)
      val result = out UInt (7 bits)
    }
    val tmp = Bool
    io.result := RegNext(io.a + io.b - io.c)
  }
  def main(args: Array[String]): Unit = {
    SimConfig.allOptimisation.compile(rtl = new Dut).doSimUntilVoid{ dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.forkSimSpeedPrinter()
      for(repeat <- 0 to 9) fork {
        while (true) {
          dut.clockDomain.waitSampling()
        }
      }
    }
  }
}
