package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.tester.scalatest
import spinal.tester.scalatest.SpinalSimVerilatorIoTest.SpinalSimVerilatorIoTestTop

import scala.concurrent.{Await, Future}
import scala.util.Random

object SpinalSimClockDomainTest{
  class SpinalSimClockDomainTest1 extends Component {
    val io = new Bundle {
      val mClk, mReset = in Bool()
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    val tmpClk, tmpReset = Bool
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
      val enable = in Bool
      val result = out UInt (8 bits)
    }

    val reg = RegInit(U(42, 8 bits))
    when(io.enable){
      reg := reg + 1
    }
    io.result := reg
  }
}

class SpinalSimClockDomainTest extends FunSuite {
  val resetKinds = List(SYNC,ASYNC)
  test("Test1"){
    for(resetKind <- resetKinds) {
      val compiled = SimConfig(new scalatest.SpinalSimClockDomainTest.SpinalSimClockDomainTest1().setDefinitionName("SpinalSimClockDomainTest1" + resetKind.getClass.getSimpleName.toString.take(4)))
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .doManagedSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          Suspendable.repeat(times = 10000) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge()
            if (cd.isResetDisasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
          }
        }
    }
  }

  test("Test2"){
    for(resetKind <- resetKinds) {
      SimConfig(new scalatest.SpinalSimClockDomainTest.SpinalSimClockDomainTest2().setDefinitionName("SpinalSimClockDomainTest2" + resetKind.getClass.getSimpleName.toString.take(4)))
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind)))
        .doManagedSim(resetKind.toString) { dut =>
          //        dut.clockDomain.forkStimulus(period = 10)
          val cd = ClockDomain(dut.io.mClk, dut.io.mReset)
          cd.forkStimulus(period = 10)

          Suspendable.repeat(times = 10000) {
            val a, b, c = Random.nextInt(256)
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            cd.waitActiveEdge()
            if (cd.isResetDisasserted) assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
          }
        }
    }
  }

  test("Test3"){
    for(resetKind <- resetKinds) {
      SimConfig(new scalatest.SpinalSimClockDomainTest.SpinalSimClockDomainTest3().setDefinitionName("SpinalSimClockDomainTest3" + resetKind.getClass.getSimpleName.toString.take(4)))
        .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = resetKind))).withWave
        .doManagedSim(resetKind.toString) { dut =>
          dut.clockDomain.forkStimulus(period = 10)

          Suspendable.repeat(times = 10000) {
            dut.io.a.randomize()
            dut.io.b.randomize()
            dut.io.c.randomize()
            dut.clockDomain.waitActiveEdge()
            if (dut.clockDomain.isResetDisasserted) assert(dut.io.result.toInt == ((dut.io.a.toBigInt + dut.io.b.toLong - dut.io.c.toInt) & 0xFF))
          }
        }
    }
  }


  test("Test4"){
    SimConfig(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4)
      .withWave
      .doManagedSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        Suspendable.repeat(times = 10000) {
          dut.io.enable.randomize()
          dut.clockDomain.waitActiveEdge()
          if(dut.io.enable.toBoolean) model = (model + 1) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }

  test("Test5"){
    SimConfig(new SpinalSimClockDomainTest.SpinalSimClockDomainTest4)
      .doManagedSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var model = 42
        dut.io.enable #= false
        dut.clockDomain.waitActiveEdge(1)
        Suspendable.repeat(times = 10000) {
          dut.io.enable.randomize()
          val waited = Random.nextInt(10)
          dut.clockDomain.waitActiveEdge(waited)
          if(dut.io.enable.toBoolean) model = (model + waited) & 0xFF
          assert(dut.io.result.toInt == model)
        }
      }
  }
}
