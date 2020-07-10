package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbAligner, BmbLengthFixer, BmbParameter}

class SpinalSimBmbLengthFixerTester extends SpinalSimFunSuite {
  test("bypass") {
    SimConfig.compile {
      val c = BmbLengthFixer(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 3,
          alignmentMin = 2,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        fixedWidth = 2
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.access.alignmentMin
      )
    }
  }

  test("3") {
    SimConfig.compile {
      val c = BmbLengthFixer(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 3,
          alignmentMin = 3,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        fixedWidth = 3
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.access.alignmentMin
      )
    }
  }

  test("4") {
    SimConfig.compile {
      val c = BmbLengthFixer(
        ip = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 3,
          alignmentMin = 4,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        ),
        fixedWidth = 4
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.ip.access.alignmentMin
      )
    }
  }

}
