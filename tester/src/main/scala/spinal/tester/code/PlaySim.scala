package spinal.tester.code
import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

object SimSynchronouExample {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init(0)
  }

  def main(args: Array[String]): Unit = {
    SimConfig(rtl = new Dut).withWave.doManagedSim("choubaka"){ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0
      while(idx < 100) {
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        dut.clockDomain.waitActiveEdge()
        assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
        idx += 1
      }
    }
    SimConfig(rtl = new Dut).withWave.doManagedSim("choubaka"){ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0
      while(idx < 100) {
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        dut.clockDomain.waitActiveEdge()
        assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
        idx += 1
      }
    }
  }
}
