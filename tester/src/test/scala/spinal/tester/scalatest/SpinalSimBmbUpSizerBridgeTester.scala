package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbUpSizerBridge, BmbParameter}

class SpinalSimBmbUpSizerBridgeTester extends FunSuite{
  test("test1"){
    SimConfig.compile{
      BmbUpSizerBridge(
        inputParameter = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 5,
          sourceWidth = 4,
          contextWidth = 3,
          canRead =  true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.BYTE
        ),
        outputParameter = BmbParameter(
          addressWidth = 16,
          dataWidth = 64,
          lengthWidth = 5,
          sourceWidth = 0,
          contextWidth = 3+2+4,
          canRead =  true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.BYTE
        )
      )
    }.doSimUntilVoid("test", 42) { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain
      )
    }
  }

  test("test2"){
    SimConfig.compile{
      val inputParameter = BmbParameter(
        addressWidth = 16,
        dataWidth = 16,
        lengthWidth = 5,
        sourceWidth = 4,
        contextWidth = 3,
        canRead =  true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )
      BmbUpSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(inputParameter, 64)
      )
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain
      )
    }
  }

  test("test3"){
    SimConfig.compile{
      val inputParameter = BmbParameter(
        addressWidth = 16,
        dataWidth = 32,
        lengthWidth = 5,
        sourceWidth = 4,
        contextWidth = 3,
        canRead =  true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )
      BmbUpSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(inputParameter, 256)
      )
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain
      )
    }
  }
}

