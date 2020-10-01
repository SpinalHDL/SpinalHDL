package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.HardType
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerAlignedMultiWidth, BsbDownSizerSparse, BsbParameter, BsbUpSizerDense, BsbUpSizerSparse}
import spinal.lib.bus.bsb.sim.BsbBridgeTester
import spinal.lib._
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
    SimConfig.compile(new BsbDownSizerSparse(
      p = BsbParameter(
        byteCount   = 8,
        sourceWidth = 3,
        sinkWidth   = 4
      ),
      outputBytes = 2
    ){
      val buffer = slave(Bsb(p))
      io.input.setAsDirectionLess.allowDirectionLessIo << buffer.stage()
    }).doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(10)
      new BsbBridgeTester(
        input = dut.buffer,
        output = dut.io.output,
        inputCd = dut.clockDomain,
        outputCd = dut.clockDomain
      )
    }
  }

  for(id <- List(0,1)) {
    test(s"BsbDownSizerAlignedMultiWidth0_$id") {
      SimConfig.withWave.compile(new BsbDownSizerAlignedMultiWidth(
        p = BsbParameter(
          byteCount = 8,
          sourceWidth = 3,
          sinkWidth = 4
        ),
        outputBytes = List(2, 4)
      ) {
        val buffer = slave(Bsb(p))
        io.input.setAsDirectionLess.allowDirectionLessIo << buffer.stage()
      }).doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.sel #= id
        new BsbBridgeTester(
          input = dut.buffer,
          output = dut.io.output,
          inputCd = dut.clockDomain,
          outputCd = dut.clockDomain
        )
      }
    }
  }

  for(id <- List(0,1)) {
    test(s"BsbDownSizerAlignedMultiWidth1_$id") {
      SimConfig.withWave.compile(new BsbDownSizerAlignedMultiWidth(
        p = BsbParameter(
          byteCount = 8,
          sourceWidth = 3,
          sinkWidth = 4
        ),
        outputBytes = List(1, 8)
      ) {
        val buffer = slave(Bsb(p))
        io.input.setAsDirectionLess.allowDirectionLessIo << buffer.stage()
      }).doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.sel #= id
        new BsbBridgeTester(
          input = dut.buffer,
          output = dut.io.output,
          inputCd = dut.clockDomain,
          outputCd = dut.clockDomain
        )
      }
    }
  }




  test("upSizerDense"){
    SimConfig.compile(new BsbUpSizerDense(
      p = BsbParameter(
        byteCount   = 2,
        sourceWidth = 2,
        sinkWidth   = 1
      ),
      outputBytes = 8
    ){val reg = RegNext(False)}).doSim(seed = 42) { dut =>
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
