//package spinal.tester.scalatest
//
//import org.scalatest.FunSuite
//import spinal.core._
//import spinal.core.sim.SimConfig
//import spinal.lib.bus.bmb.sim.BmbBridgeTester
//import spinal.lib.bus.bmb.{BmbAligner, BmbLengthFixer, BmbParameter}
//
//class SpinalSimBmbLengthFixerTester extends FunSuite {
//  test("bypass") {
//    SimConfig.compile {
//      val c = BmbLengthFixer(
//        ip = BmbParameter(
//          addressWidth = 16,
//          dataWidth = 32,
//          lengthWidth = 6,
//          sourceWidth = 4,
//          contextWidth = 3,
//          canRead = true,
//          canWrite = true,
//          alignment = BmbParameter.BurstAlignement.WORD
//        ),
//        fixedWidth = 2
//      )
//      c.rework{ RegNext(True) init(False) setName("dummy") }
//      c
//    }.doSimUntilVoid("test") { dut =>
//      new BmbBridgeTester(
//        master = dut.io.input,
//        masterCd = dut.clockDomain,
//        slave = dut.io.output,
//        slaveCd = dut.clockDomain
//      )
//    }
//  }
//}
