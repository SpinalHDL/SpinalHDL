package spinal.lib.bus.bmb

import spinal.tester.SpinalSimFunSuite

import spinal.core.sim._
import spinal.lib.bus.bmb.sim.BmbBridgeTester

class SpinalSimBmbDownSizerBridgeTester extends SpinalSimFunSuite{
  test("test1"){
    SimConfig.compile{
      BmbDownSizerBridge(
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
          dataWidth = 16,
          lengthWidth = 5,
          sourceWidth = 4,
          contextWidth = 3+1,
          canRead =  true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.BYTE
        )
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

  test("test2"){
    SimConfig.compile{
      val inputParameter = BmbParameter(
        addressWidth = 16,
        dataWidth = 64,
        lengthWidth = 5,
        sourceWidth = 4,
        contextWidth = 3,
        canRead =  true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )
      BmbDownSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbDownSizerBridge.outputParameterFrom(inputParameter.access, 16).toBmbParameter()
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
        dataWidth = 256,
        lengthWidth = 5,
        sourceWidth = 4,
        contextWidth = 3,
        canRead =  true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )
      BmbDownSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbDownSizerBridge.outputParameterFrom(inputParameter.access, 32).toBmbParameter()
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

