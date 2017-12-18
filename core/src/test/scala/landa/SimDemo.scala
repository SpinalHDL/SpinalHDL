package landa

import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object SimDemo {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init(0)
  }

  def main(args: Array[String]): Unit = {
    //For alternatives ways of running the sim, see note at the end of the file
    SimConfig(new Dut)
      .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)))
      .withWave
      .doManagedSim{ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      Suspendable.repeat(times = 100) {
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        dut.clockDomain.waitActiveEdge()
        if(dut.clockDomain.isResetDisasserted) assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
      }
    }
  }
}

//Note that there is two ways to run the sim :
// SimConfig(rtl = new Dut).withWave.doManagedSim{ dut =>
// SimConfig(rtl = SpinalVerilog(new Dut)).withWave.doManagedSim{ dut =>