package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim.SimConfig
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbAlignedSpliter, BmbParameter}

class SpinalSimBmbLengthSpliterTester extends FunSuite {
  for(w <- List(false, true); r <- List(false, true);   if w || r) {
    val header = "_" + (if (w) "w" else "") + (if (r) "r" else "")
    test("bypass" + header) {
      SimConfig.compile {
        val c = BmbAlignedSpliter(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 3,
            alignmentMin = 0,
            canRead = r,
            canWrite = w,
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

    test("8" + header) {
      SimConfig.compile {
        val c = BmbAlignedSpliter(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 8,
            alignmentMin = 0,
            canRead = r,
            canWrite = w,
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

    test("16" + header) {
      SimConfig.compile {
        val c = BmbAlignedSpliter(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 8,
            alignmentMin = 0,
            canRead = r,
            canWrite = w,
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
}
