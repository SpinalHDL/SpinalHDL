package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalFifoCCTester extends SpinalFormalFunSuite {
  def formalContext(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = new Area {
    val back2backCycles = 2
    val fifoDepth = 4

    val pushClock = ClockDomain.current
    val reset = pushClock.isResetActive
    val popClock = ClockDomain.external("pop")
    val popReset = popClock.isResetActive

    val inValue = in(UInt(3 bits))
    val inValid = in(Bool())
    val outReady = in(Bool())
    val gclk = GlobalClock()

    assumeInitial(reset)
    assumeInitial(popReset)

    gclk.assumeClockTiming(pushClock, pushPeriod)
    gclk.assumeResetReleaseSync(pushClock)
    if (!seperateReset) {
      gclk.keepBoolLeastCycles(reset, popPeriod)
    }

    gclk.assumeClockTiming(popClock, popPeriod)
    if (seperateReset) {
      gclk.assumeResetReleaseSync(popClock)
      gclk.alignAsyncResetStart(pushClock, popClock)
    }

    val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock, !seperateReset))
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.valid)
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.payload)
    gclk.assumeIOSync2Clock(popClock, dut.io.pop.ready)

    dut.io.push.payload := inValue
    dut.io.push.valid := inValid
    dut.io.pop.ready := outReady

    // assume no valid while reset and one clock later.
    when(reset || past(reset)) {
      assume(inValid === False)
    }

    val globalArea = new ClockingArea(gclk.domain) {
      when(dut.io.push.ready) { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth - 1) }
        .otherwise { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth) }

      assert(dut.popCC.popPtrGray === toGray(dut.popCC.popPtr))
      assert(fromGray(dut.popCC.pushPtrGray) - dut.popCC.popPtr <= fifoDepth)
    }

    val pushArea = new ClockingArea(pushClock) {
      dut.io.push.withAssumes()
      dut.io.push.withCovers()

      when(changed(dut.pushCC.popPtrGray)) {
        assert(fromGray(dut.pushCC.popPtrGray) - past(fromGray(dut.pushCC.popPtrGray)) <= fifoDepth)
      }
      assert(dut.pushCC.pushPtrGray === toGray(dut.pushCC.pushPtr))
      assert(dut.pushCC.pushPtr - fromGray(dut.pushCC.popPtrGray) <= fifoDepth)
    }

    // back to back transaction cover test.
    val popCheckDomain = if (seperateReset) popClock else popClock.copy(reset = reset)
    val popArea = new ClockingArea(popCheckDomain) {
      dut.io.pop.withCovers(back2backCycles)
      dut.io.pop.withAsserts()

      when(changed(dut.popCC.pushPtrGray)) {
        assert(fromGray(dut.popCC.pushPtrGray) - past(fromGray(dut.popCC.pushPtrGray)) <= fifoDepth)
      }
    }
  }

  def testMain(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = {
    val proveCycles = 8
    val coverCycles = 10
    val maxPeriod = Math.max(pushPeriod, popPeriod)

    FormalConfig
      .withProve(maxPeriod * proveCycles)
      .withCover(maxPeriod * coverCycles)
      .withAsync
      .doVerify(new Component {
        val context = formalContext(pushPeriod, popPeriod, seperateReset)
      })
  }

  test("fifo-verify fast pop") {
    testMain(5, 3)
  }

  test("fifo-verify fast push") {
    testMain(3, 5)
  }

//   test("fifo-verify ultra fast pop") {
//     testMain(11, 2)
//   }

//   test("fifo-verify ultra fast push") {
//     testMain(2, 11)
//   }

  test("fifo-verify fast pop reset seperately") {
    testMain(5, 3, true)
  }

  test("fifo-verify fast push reset seperately") {
    testMain(3, 5, true)
  }

//   test("fifo-verify ultra fast pop reset seperately") {
//     testMain(11, 2, true)
//   }

//   test("fifo-verify ultra fast push reset seperately") {
//     testMain(2, 11, true)
//   }
}