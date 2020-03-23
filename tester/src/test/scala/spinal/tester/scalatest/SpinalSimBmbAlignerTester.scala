package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim.SimConfig
import spinal.lib.bus.bmb.{BmbAligner, BmbDownSizerBridge, BmbParameter}
import spinal.lib.bus.bmb.sim.BmbBridgeTester

class SpinalSimBmbAlignerTester extends FunSuite {
  for(w <- List(false, true); r <- List(false, true);   if w || r) {
    val header = "_" + (if(w) "w" else "") + (if(r) "r" else "")
    test("BmbAligner_bypass" + header) {
      SimConfig.compile {
        val c = BmbAligner(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 3,
            canRead = r,
            canWrite = w,
            alignment = BmbParameter.BurstAlignement.WORD
          ),
          alignmentWidth = 2
        )
        c.rework {
          RegNext(True) init (False) setName ("dummy")
        }
        c
      }.doSimUntilVoid("test") { dut =>
        new BmbBridgeTester(
          master = dut.io.input,
          masterCd = dut.clockDomain,
          slave = dut.io.output,
          slaveCd = dut.clockDomain
        )
      }
    }

    test("BmbAligner_4" + header) {
      SimConfig.compile {
        BmbAligner(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 3,
            canRead = r,
            canWrite = w,
            alignment = BmbParameter.BurstAlignement.WORD
          ),
          alignmentWidth = 4
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

    test("BmbAligner_3" + header) {
      SimConfig.compile {
        BmbAligner(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 3,
            canRead = r,
            canWrite = w,
            alignment = BmbParameter.BurstAlignement.WORD
          ),
          alignmentWidth = 3
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

    test("BmbAligner_1" + header) {
      SimConfig.compile {
        val c = BmbAligner(
          ip = BmbParameter(
            addressWidth = 16,
            dataWidth = 32,
            lengthWidth = 6,
            sourceWidth = 4,
            contextWidth = 3,
            canRead = r,
            canWrite = w,
            alignment = BmbParameter.BurstAlignement.WORD
          ),
          alignmentWidth = 1
        )
        c.rework {
          RegNext(True) init (False) setName ("dummy")
        }
        c
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
}
