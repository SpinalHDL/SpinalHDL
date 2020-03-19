package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim.SimConfig
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbAlignedSpliter, BmbParameter}

class SpinalSimBmbLengthSpliterTester extends FunSuite {
  test("bypass") {
    SimConfig.compile {
      val c = BmbAlignedSpliter(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 3,
          alignmentMin = 0,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        lengthMax = 4
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.alignmentMin
      )
    }
  }

  test("8") {
    SimConfig.compile {
      val c = BmbAlignedSpliter(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 8,
          alignmentMin = 0,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        lengthMax = 8
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.alignmentMin
      )
    }
  }

  test("16") {
    SimConfig.compile {
      val c = BmbAlignedSpliter(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 8,
          alignmentMin = 0,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        lengthMax = 16
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.alignmentMin
      )
    }
  }
}
