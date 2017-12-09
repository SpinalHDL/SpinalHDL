package spinal.sim

import spinal.sim.SimManagerApi._
import spinal.core._
import scala.util.continuations.suspendable


object SimManagedTest {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c)
  }

  def main(args: Array[String]): Unit = {
    SimVerilatorManaged(new Dut) { dut =>
      val t1 = fork {
        var idx = 0
        while (idx < 20) {
          dut.io.a :<< dut.io.a.toLong + 1
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
          dut.io.b :<< dut.io.b.toLong + 1
          sleep(20)
          idx += 1
        }
      }

      val t3 = fork {
        var idx = 0
        while (idx < 20) {
          dut.io.c :<< dut.io.c.toLong + 1
          sleep(30)
          idx += 1
        }
      }


      t1.join()
      sleep(50)
      dut.io.a :<< 42
      sleep(10)

      def doStuff(bt: UInt, value : Int): Long@suspendable = {
        bt :<< value
        sleep(40)
        bt.toLong + 1
      }

      println(doStuff(dut.io.a, 66))
      println(doStuff(dut.io.a, 88))
    }
  }
}
