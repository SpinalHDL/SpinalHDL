package landa;
import spinal.core._

object SpeedTest {
  object Rtl {
    import spinal.core._
    class Dut extends Component {
      val io = new Bundle {
        val a, b, c = in UInt (8 bits)
        val result = out UInt (8 bits)
      }
      io.result := RegNext(io.a + io.b - io.c)
    }
  }
  def main(args: Array[String]): Unit = {
    val times = 1000000
    Bench(times) {
      import spinal.core.SimManagedApi._
      SimManagedVerilator(new Rtl.Dut) { dut =>
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

        val t1 = fork {
          var idx = 0
          while (idx < times) {
            dut.clockDomain.waitRisingEdge
            dut.io.b #= (dut.io.b.toLong + 1) & 0xFF
            idx += 1
          }
        }

        clkGen.join()
      }
    }
  }
}
