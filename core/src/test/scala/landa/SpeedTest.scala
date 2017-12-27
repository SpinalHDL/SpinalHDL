package landa


import spinal.core._
import spinal.core.sim._

object SpeedTest {

  import spinal.core._

  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := (0 until 1).map(_ => RegNext(io.a + io.b - io.c)).reduce(_ | _)
  }

  def main(args: Array[String]): Unit = {

    import spinal.core.sim._
    SimConfig.doSim(rtl = new Dut){ dut =>
      val t1 = fork {
        while (true) {
          dut.clockDomain.waitRisingEdge
          dut.io.b #= (dut.io.b.toLong + 1) & 0xFF
        }
      }

      val times = 1000000
      BenchSim(times) {
        val clkGen = fork {
          var idx = 0
          while (idx < times) {
            dut.clockDomain.fallingEdge
            sleep(5)
            dut.clockDomain.risingEdge
            sleep(5)
            idx += 1
          }
        }
        clkGen.join()
      }
    }
  }
}
