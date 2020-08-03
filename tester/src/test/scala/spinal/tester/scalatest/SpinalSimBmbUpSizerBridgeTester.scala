package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbAccessParameter, BmbParameter, BmbSourceParameter, BmbUpSizerBridge}

import scala.collection.mutable

class SpinalSimBmbUpSizerBridgeTester extends SpinalSimFunSuite{
  test("test1") {
    SimConfig.compile {
      val inputParameter =  BmbAccessParameter(
        addressWidth = 16,
        dataWidth = 32
      ).addSources(16, BmbSourceParameter(
        lengthWidth = 5,
        contextWidth = 3,
        canRead = true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )).toBmbParameter()

      BmbUpSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(inputParameter.access, 64).toBmbParameter()
      )
    }.doSimUntilVoid("test", 42) { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        rspCountTarget = (300*durationFactor).toInt
      )
    }
  }

  test("test2") {
    SimConfig.compile {
      val inputParameter = BmbAccessParameter(
        addressWidth = 16,
        dataWidth = 16
      ).addSources(16, BmbSourceParameter(
        lengthWidth = 5,
        contextWidth = 3,
        canRead = true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )).toBmbParameter()
      BmbUpSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(inputParameter.access, 64).toBmbParameter()
      )
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        rspCountTarget = (300*durationFactor).toInt
      )
    }
  }

  test("test3") {
    SimConfig.compile {
      val inputParameter = BmbAccessParameter(
        addressWidth = 16,
        dataWidth = 32
      ).addSources(16, BmbSourceParameter(
        lengthWidth = 5,
        contextWidth = 3,
        canRead = true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.BYTE
      )).toBmbParameter()
      BmbUpSizerBridge(
        inputParameter = inputParameter,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(inputParameter.access, 256).toBmbParameter()
      )
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        rspCountTarget = (300*durationFactor).toInt
      )
    }
  }
}

