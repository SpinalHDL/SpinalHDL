package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.language.postfixOps

class IsUnknownTester extends SpinalAnyFunSuite {
  test("Test if isUnknown is supported") {
    class Dut extends Component {
      val io = new Bundle {
        val input = in Bits(16 bits)
        val isUnknown = out Bool()
      }

      io.isUnknown := io.input(0).isUnknown
    }

    SimConfig.withIVerilog.addSimulatorFlag("-g2012").addSimulatorFlag("-D__HAS_SYSTEM_VERILOG__")
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        assert(dut.io.isUnknown.toBoolean)
        dut.clockDomain.waitSampling()
        dut.io.input #= 0
        dut.clockDomain.waitSampling()
        assert(!dut.io.isUnknown.toBoolean)
      }

    SimConfig.withIVerilog
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()

        dut.clockDomain.waitSampling()
        dut.io.input #= 0
        dut.clockDomain.waitSampling()
      }

    SimConfig.withGhdl
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        dut.clockDomain.waitSampling()
        dut.io.input #= 0
        dut.clockDomain.waitSampling()
      }

  }
}
