package spinal.core

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.tester.SpinalSimTester

import java.io.File
import java.nio.file.Paths

// Checks whether the simulators included in SpinalSimTester (at time
// of this writing, December 2024: GHDL, IVerilog, Verilator) all
// put their wave files in the per-test location expected.
class SpinalSimWaveLocationTester extends AnyFunSuite {
  class Comp extends Component {
    val input = in(UInt(6 bits))
    val output = out(UInt(6 bits)).setAsReg()
    output := output + input
  }

  SpinalSimTester { env =>
    println(f"env: ${env}")
    val compiled = env.SimConfig.withVcdWave.compile(new Comp())

    for (testName <- Seq("test1", "test2")) {
      var testPath: String = null

      compiled.doSim(testName) { dut =>
        testPath = currentTestPath()
        dut.clockDomain.forkStimulus(10)
        dut.input #= 5
        dut.clockDomain.waitSampling(10)
        simSuccess()
      }

      val expWavePath = Paths.get(testPath, "wave.vcd").toString
      if (!new File(expWavePath).isFile) {
        fail(f"expected wave file to be at '${expWavePath}' but did not find it")
      }
    }
  }
}