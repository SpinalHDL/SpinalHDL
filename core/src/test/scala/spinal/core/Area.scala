package spinal.core

import org.scalatest.funsuite.AnyFunSuite

import spinal.core.sim._

class AreaTester extends AnyFunSuite {
  test("SlowArea") {
    SimConfig
      .withConfig(
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz))
      )
      .compile(new Component {
        val counter = out(RegInit(U"0000"))
        counter := counter + 1
        assert(clockDomain.samplingRate.getValue.toInt == 4000)

        val slowArea = new SlowArea(4) {
          val counter = out(RegInit(U"0000"))
          counter := counter + 1
          assert(clockDomain.samplingRate.getValue.toInt == 1000)
        }
      })
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)

        for (i <- 0 until 1000) {
          dut.clockDomain.waitSampling()
          assert(dut.counter.toInt == i % 16)
          assert(dut.slowArea.counter.toInt == (i - 1) / 4 % 16)
        }
      }
  }
}
