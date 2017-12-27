package landa

import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object CoreSimTest {
  class Dut extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    val tmpClk, tmpReset = Bool
    tmpClk := io.mClk
    tmpReset := io.mReset
    ClockDomain(tmpClk, tmpReset){
      io.result := RegNext(io.a + io.b - io.c) init(0)
    }
  }

  def main(args: Array[String]): Unit = {
    //For alternatives ways of running the sim, see note at the end of the file
    SimConfig
      .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)))
      .withWave
      .compile(new Dut)
      .doSim{ dut =>
//        dut.clockDomain.forkStimulus(period = 10)
        val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
        cd.forkStimulus(period = 10)

        Suspendable.repeat(times = 100) {
          val a, b, c = Random.nextInt(256)
          dut.io.a #= a
          dut.io.b #= b
          dut.io.c #= c
          cd.waitActiveEdge()
          if(cd.isResetDisasserted) assert(dut.io.result.toInt == ((a+b -c) & 0xFF))
        }
      }
  }
}

//Note that there is two ways to run the sim :
// SimConfig(rtl = new Dut).withWave.doSim{ dut =>
// SimConfig(rtl = SpinalVerilog(new Dut)).withWave.doSim{ dut =>