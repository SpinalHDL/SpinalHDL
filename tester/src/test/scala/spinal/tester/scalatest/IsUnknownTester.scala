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
        val input = slave Flow(Bits(16 bits))
        val reset0 = in Bool()
        val isUnknown0 = out Bool()
        val isUnknown = out Bool()
      }

      val inputReg = RegNextWhen(io.input.payload, io.input.fire)
      when(io.reset0) {
        inputReg(0) := False
      }

      io.isUnknown0 := inputReg(0).isUnknown
      io.isUnknown := inputReg.isUnknown
    }

    SimConfig.withIVerilog
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz), mode = SystemVerilog))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.input.valid #= false
        dut.io.reset0 #= false
        dut.clockDomain.waitSampling()
        assert(dut.io.isUnknown.toBoolean)
        assert(dut.io.isUnknown0.toBoolean)
        dut.clockDomain.waitSampling()
        dut.io.reset0 #= true

        dut.clockDomain.waitSampling(2)
        assert(dut.io.isUnknown.toBoolean)
        assert(!dut.io.isUnknown0.toBoolean)
        dut.clockDomain.waitSampling()
        dut.io.input.payload #= 0
        dut.io.input.valid #= true

        dut.clockDomain.waitSampling(2)
        assert(!dut.io.isUnknown.toBoolean)
        assert(!dut.io.isUnknown0.toBoolean)
      }

    SimConfig.withIVerilog
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        assert(!dut.io.isUnknown.toBoolean) // Since the user did not ask for system verilog, this is always false.
      }

    SimConfig.withGhdl
      .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz)))
      .compile(new Dut())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        assert(!dut.io.isUnknown.toBoolean) // Since the user is using vhdl, this is always false.
      }

  }
}
