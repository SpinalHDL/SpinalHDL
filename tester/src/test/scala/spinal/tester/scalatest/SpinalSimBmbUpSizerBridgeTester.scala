package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbAccessParameter, BmbParameter, BmbSourceParameter, BmbUpSizerBridge}

import scala.collection.mutable

class SpinalSimBmbUpSizerBridgeTester extends SpinalSimFunSuite{
  for((canRead, canWrite) <- List((true,true),(true, false), (false, true))) {
    val prefix = (if(canRead) "R" else "") + (if(canWrite) "W" else "") + "_"
    test(prefix + "test1") {
      SimConfig.compile {
        val inputParameter = BmbAccessParameter(
          addressWidth = 16,
          dataWidth = 32
        ).addSources(16, BmbSourceParameter(
          lengthWidth = 5,
          contextWidth = 3,
          canRead  = canRead,
          canWrite = canWrite,
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
          rspCountTarget = (300 * durationFactor).toInt
        )
      }
    }

    test(prefix + "test2") {
      SimConfig.compile {
        val inputParameter = BmbAccessParameter(
          addressWidth = 16,
          dataWidth = 16
        ).addSources(16, BmbSourceParameter(
          lengthWidth = 5,
          contextWidth = 3,
          canRead  = canRead,
          canWrite = canWrite,
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
          rspCountTarget = (300 * durationFactor).toInt
        )
      }
    }

    test(prefix + "test3") {
      SimConfig.compile {
        val inputParameter = BmbAccessParameter(
          addressWidth = 16,
          dataWidth = 32
        ).addSources(16, BmbSourceParameter(
          lengthWidth = 5,
          contextWidth = 3,
          canRead  = canRead,
          canWrite = canWrite,
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
          rspCountTarget = (300 * durationFactor).toInt
        )
      }
    }
  }
}

