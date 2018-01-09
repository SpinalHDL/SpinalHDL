//package spinal.tester.scalatest
//
//import org.scalatest.FunSuite
//import spinal.core._
//import spinal.sim._
//import spinal.core.sim._
//import spinal.tester.scalatest
//import spinal.tester.scalatest.SpinalSimVerilatorIoTest.SpinalSimVerilatorIoTestTop
//
//import scala.concurrent.{Await, Future}
//import scala.util.Random
//
//object SpinalSimPerfTester {
//
//  class SpinalSimPerfTesterDut extends Component {
//    val io = new Bundle {
//      val a, b, c = in UInt (8 bits)
//      val result = out UInt (8 bits)
//    }
//
//    io.result := RegNext(io.a + io.b - io.c) init (0)
//  }
//
//}
//
//class SpinalSimPerfTester extends FunSuite {
//
//  var compiled: SimCompiled[SpinalSimPerfTester.SpinalSimPerfTesterDut] = null
//
//  test("compile") {
//    compiled = SimConfig
//      .allOptimisation
//      .compile(new SpinalSimPerfTester.SpinalSimPerfTesterDut())
//  }
//
//  test("TestStdSimInt") {
//    compiled.doSim { dut =>
//      dut.clockDomain.forkStimulus(period = 10)
//      dut.clockDomain.forkSimSpeedPrinter(0.2)
//
//      var model = 0
//      Suspendable.repeat(times = 4) {
//        val times = 1000000
//        val startAt = System.nanoTime
//        Suspendable.repeat(times = times) {
//          dut.io.a #= Random.nextInt(256)
//          dut.io.b #= Random.nextInt(256)
//          dut.io.c #= Random.nextInt(256)
//          dut.clockDomain.waitActiveEdge()
//          if (dut.clockDomain.isResetDisasserted) {
//            assert(dut.io.result.toInt == model)
//            model = ((dut.io.a.toInt + dut.io.b.toInt - dut.io.c.toInt) & 0xFF)
//          }
//        }
//        val endAt = System.nanoTime
//        System.out.println((endAt - startAt) * 1e-6 + " ms")
//      }
//    }
//  }
//
//  test("TestStdSimIntx2") {
//    compiled.doSim { dut =>
//      dut.clockDomain.forkStimulus(period = 10)
//
//      Suspendable.repeat(times = 4) {
//        val times = 1000000
//        val startAt = System.nanoTime
//        val t1, t2 = fork {
//          val rand = new Random(1)
//          Suspendable.repeat(times = times) {
//            val a, b, c = rand.nextInt(256)
//            dut.io.a #= a
//            dut.io.b #= b
//            dut.io.c #= c
//            dut.clockDomain.waitActiveEdge(); sleep(0)
//            if (dut.clockDomain.isResetDisasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
//          }
//        }
//
//        t1.join();t2.join()
//        val endAt = System.nanoTime
//        System.out.println((endAt - startAt) * 1e-6 + " ms")
//      }
//    }
//  }
//
//
//  test("TestStdSimBigInt") {
//    compiled.doSim { dut =>
//      dut.clockDomain.forkStimulus(period = 10)
//
//      Suspendable.repeat(times = 4) {
//        val times = 1000000
//        val startAt = System.nanoTime
//        Suspendable.repeat(times = times) {
//          val a, b, c = BigInt(Random.nextInt(256))
//          dut.io.a #= a
//          dut.io.b #= b
//          dut.io.c #= c
//          dut.clockDomain.waitActiveEdge(); sleep(0)
//          if (dut.clockDomain.isResetDisasserted) assert(dut.io.result.toBigInt == ((a + b - c) & 0xFF))
//        }
//        val endAt = System.nanoTime
//        System.out.println((endAt - startAt) * 1e-6 + " ms")
//      }
//    }
//  }
//
//  test("TestSleep0") {
//    compiled.doSim { dut =>
//      dut.clockDomain.forkStimulus(period = 10)
//
//      Suspendable.repeat(times = 4) {
//        val times = 1000000
//        val startAt = System.nanoTime
//        Suspendable.repeat(times = times) {
//          sleep(0)
//        }
//        val endAt = System.nanoTime
//        System.out.println((endAt - startAt) * 1e-6 + " ms")
//      }
//    }
//
//  }
//
//}
