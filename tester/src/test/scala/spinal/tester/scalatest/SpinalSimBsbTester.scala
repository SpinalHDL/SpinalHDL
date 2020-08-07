package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.HardType
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.bsb.{BsbDownSizerSparse, BsbParameter, BsbUpSizerSparse}
import spinal.lib.bus.bsb.sim.BsbBridgeTester
import spinal.lib.system.dma.sg.{DmaSg, DmaSgTester, SgDmaTestsParameter}

class SpinalSimBsbTester extends FunSuite{
  test("upsizerSparse"){
    SimConfig.doSim(new BsbUpSizerSparse(
      p = BsbParameter(
        byteCount   = 2,
        sourceWidth = 3,
        sinkWidth   = 4
      ),
      outputBytes = 8
    ){val reg = RegNext(False)}) { dut =>
      dut.clockDomain.forkStimulus(10)
      new BsbBridgeTester(
        input = dut.io.input,
        output = dut.io.output,
        inputCd = dut.clockDomain,
        outputCd = dut.clockDomain
      )
    }
  }

  test("downSizerSparse"){
    SimConfig.doSim(new BsbDownSizerSparse(
      p = BsbParameter(
        byteCount   = 8,
        sourceWidth = 3,
        sinkWidth   = 4
      ),
      outputBytes = 2
    ){val reg = RegNext(False)}) { dut =>
      dut.clockDomain.forkStimulus(10)
      new BsbBridgeTester(
        input = dut.io.input,
        output = dut.io.output,
        inputCd = dut.clockDomain,
        outputCd = dut.clockDomain
      )
    }
  }
}
