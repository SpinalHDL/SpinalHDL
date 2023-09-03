package spinal.core

import spinal.sim._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

import scala.concurrent.{Await, Future}
import scala.util.Random

object SpinalSimClockDomainTest{
  class SpinalSimClockDomainTest1 extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    val tmpClk, tmpReset = Bool()
    tmpClk := io.mClk
    tmpReset := io.mReset
    ClockDomain(tmpClk, tmpReset){
      io.result := RegNext(io.a + io.b - io.c) init(0)
    }
  }

  class SpinalSimClockDomainTest2 extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    ClockDomain(io.mClk, io.mReset){
      io.result := RegNext(io.a + io.b - io.c) init(0)
    }
  }
  class SpinalSimClockDomainTest3 extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    io.result := RegNext(io.a + io.b - io.c) init(0)
  }

  class SpinalSimClockDomainTest4 extends Component {
    val io = new Bundle {
      val enable = in Bool()
      val result = out UInt (8 bits)
    }

    val reg = RegInit(U(42, 8 bits))
    when(io.enable){
      reg := reg + 1
    }
    io.result := reg
  }
}

class SpinalSimClockDomainTest extends SpinalSimFunSuite {
  val resetKinds = List(SYNC,ASYNC)
  test("Test1") {
    for (resetKind <- resetKinds) {
      val compiled = SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile(new SpinalSimClockDomainTest.SpinalSimClockDomainTest1().setDefinitionName("SpinalSimClockDomainTest1" + resetKind.getClass.getSimpleName.toString.take(4)))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          for (repeat2 <- 0 until 10000) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge(); sleep(0)
            if (cd.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
          }
        }
    }
  }

  test("TestDeltaCycle wake") {
    for (resetKind <- resetKinds) {
      val compiled = SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile(new SpinalSimClockDomainTest.SpinalSimClockDomainTest1().setDefinitionName("SpinalSimClockDomainTest1" + resetKind.getClass.getSimpleName.toString.take(4)))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          dut.io.a #= 0
          dut.io.b #= 0
          dut.io.c #= 0
          sleep(10)
          cd.forkStimulus(period = 10)
          cd.waitSampling()
          cd.waitSampling()
          dut.io.a #= 42
          cd.waitSampling()
          assert(dut.io.result.toInt == 0) //Wakeup while rising edge, but before FF got the result out
          sleep(0)
          assert(dut.io.result.toInt == 42)
        }
    }
  }


  test("Test2") {
    for (resetKind <- resetKinds) {
      SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile((new SpinalSimClockDomainTest.SpinalSimClockDomainTest2().setDefinitionName("SpinalSimClockDomainTest2" + resetKind.getClass.getSimpleName.toString.take(4))))
        .doSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          var counter = 0
          while (true) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge();
            sleep(0)
            if (cd.isResetDeasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
            counter += 1
            if (counter == 10000) simSuccess()
          }
        }
    }
  }

  test("Test3") {
    for (resetKind <- resetKinds) {
      SimConfig
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .compile((new SpinalSimClockDomainTest.SpinalSimClockDomainTest3().setDefinitionName("SpinalSimClockDomainTest3" + resetKind.getClass.getSimpleName.toString.take(4))))
        .doSim(resetKind.toString) { dut =>
          dut.clockDomain.forkStimulus(period = 10)
          var model = BigInt(0)
          for (repeat <- 0 until 10000) {
            dut.io.a.randomize()
            dut.io.b.randomize()
            dut.io.c.randomize()
            dut.clockDomain.waitActiveEdge()
            if (dut.clockDomain.isResetDeasserted) {
              assert(dut.io.result.toInt == model)
              model = ((dut.io.a.toBigInt + dut.io.b.toLong - dut.io.c.toInt) & 0xFF)
            }
          }
        }
    }
  }


  test("Test4") {
    SimConfig
      .doSim(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        for (repeat <- 0 until 10000) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge(); sleep(0)
          if (dut.io.enable.toBoolean) model = (model + 1) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }

  test("Test5") {
    SimConfig
      .doSim(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        dut.io.enable #= false
        dut.clockDomain.waitActiveEdge(1)
        for (repeat <- 0 until 10000) {
          dut.io.enable.randomize()
          val waited = Random.nextInt(10)
          dut.clockDomain.waitActiveEdge(waited); sleep(0)
          if (dut.io.enable.toBoolean) model = (model + waited) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }
}
