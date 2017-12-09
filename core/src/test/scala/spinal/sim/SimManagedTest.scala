package spinal.sim

import SpinalSimManagedApi._
import spinal.core.UInt

import scala.util.continuations.suspendable


object SimManagedTest {
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
    SpinalSimManagedVerilator(new Rtl.Dut) { dut =>
      val t1 = fork {
        var idx = 0
        sleep(2)
        while (idx < 20) {
          dut.io.a := dut.io.a.toLong + 1
          sleep(10)
          println(idx)
          idx += 1
        }
      }


      val clkGen = fork {
        var idx = 0
        while (idx < 200) {
          dut.clockDomain.fallingEdge
          sleep(10)
          dut.clockDomain.risingEdge
          sleep(10)
          idx += 1
        }
      }

      val t2 = fork {
        var idx = 0
        while (idx < 20) {
          dut.clockDomain.waitRisingEdge
          dut.io.b := dut.io.b.toLong + 1
          idx += 1
        }
      }

      val t3 = fork {
        sleep(2)
        var idx = 0
        while (idx < 20) {
          dut.io.c := dut.io.c.toLong + 1
          sleep(30)
          idx += 1
        }
      }

      val t4 = fork{
        waitUntil(dut.io.a.toLong == 66)
        dut.io.b := 77l
        waitUntil(dut.io.a.toLong == 88)
        dut.io.b := 99l
        sleep(10)
      }

      t1.join()
      sleep(50)
      dut.io.a := 42l
      sleep(10)

      def doStuff(bt: UInt, value : Long): Long@suspendable = {
        bt := value
        sleep(40)
        bt.toLong + 1
      }

      t3.join()
      println(doStuff(dut.io.a, 66))
      println(doStuff(dut.io.a, 88))
    }
  }
}
