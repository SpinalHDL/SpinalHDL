package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim.SimConfig
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.bus.bmb.{BmbLengthFixer, BmbParameter, BmbUnburstify}

class SpinalSimBmbUnburstifyTester extends FunSuite {
  test("miaou") {
    SimConfig.withWave.compile {
      val c = BmbUnburstify(
        inputParameter = BmbParameter(
          addressWidth = 16,
          dataWidth = 32,
          lengthWidth = 6,
          sourceWidth = 4,
          contextWidth = 3,
          alignmentMin = 2,
          canRead = true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.WORD
        )
      )
      c
    }.doSimUntilVoid("test") { dut =>
      new BmbBridgeTester(
        master = dut.io.input,
        masterCd = dut.clockDomain,
        slave = dut.io.output,
        slaveCd = dut.clockDomain,
        alignmentMinWidth = dut.inputParameter.alignmentMin,
        rspCountTarget = 1000
      )
    }
  }
}
