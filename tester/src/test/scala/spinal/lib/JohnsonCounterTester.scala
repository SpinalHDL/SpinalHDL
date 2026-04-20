package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable

class JohnsonCounterTester extends SpinalAnyFunSuite {

  // Build the 2*width legal Johnson states starting from all-zeros
  def legalSequence(width: Int): Seq[BigInt] = {
    val mask = (BigInt(1) << width) - 1
    val seq = mutable.ArrayBuffer[BigInt](BigInt(0))
    for (_ <- 0 until 2 * width - 1) {
      val prev = seq.last
      val msbInv = BigInt(1) - ((prev >> (width - 1)) & 1)
      seq += ((prev << 1) | msbInv) & mask
    }
    seq.toSeq
  }

  def illegalStates(width: Int): Seq[BigInt] = {
    val legal = legalSequence(width).toSet
    (0 until (1 << width)).map(BigInt(_)).filterNot(legal.contains)
  }

  // Test dut: a Johnson counter with optional state-forcing for illegal-state injection
  class Dut(val width: Int) extends Component {
    val inc = in Bool()
    val clr = in Bool()
    val forceEn = in Bool()
    val forceVal = in Bits (width bits)
    val value = out Bits (width bits)
    val willOverflow = out Bool()
    val clkDiv = out Bool()

    val jc = JohnsonCounter(width)
    when(inc) { jc.increment() }
    when(clr) { jc.clear() }
    when(forceEn) { jc.value := forceVal } // override for illegal-state injection

    value := jc.value
    willOverflow := jc.willOverflow
    clkDiv := jc.clkDiv
  }

  /**
   * Advance past reset and stabilize inputs. Returns with value == 0, all control inputs low,
   * and reset released. Subsequent `#=` changes take effect on the *second* clock edge after
   * they are set (SpinalSim schedules input changes after the next edge's sampling point), so
   * tests that want to observe the first effect of a change must burn one extra waitSampling.
   */
  def startAndExitReset(dut: Dut): Unit = {
    dut.inc #= false
    dut.clr #= false
    dut.forceEn #= false
    dut.forceVal #= 0
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(20)
  }

  test("post-reset value is 0") {
    for (w <- Seq(2, 3, 4, 8)) {
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        assert(dut.value.toBigInt == 0, s"width=$w: post-reset value ${dut.value.toBigInt}")
      }
    }
  }

  test("walks the legal sequence under free-running increment") {
    for (w <- Seq(2, 3, 4, 5, 8)) {
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        val legal = legalSequence(w)
        dut.inc #= true
        dut.clockDomain.waitSampling() // burn cycle: inc change not yet visible at register
        for (i <- 1 to 3 * legal.size) {
          dut.clockDomain.waitSampling()
          val expected = legal(i % legal.size)
          assert(dut.value.toBigInt == expected,
            s"width=$w cycle $i: got ${dut.value.toBigInt.toString(2)}, expected ${expected.toString(2)}")
        }
      }
    }
  }

  test("willOverflow pulses exactly once per 2*width cycles") {
    for (w <- Seq(2, 4, 5, 8)) {
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        dut.inc #= true
        dut.clockDomain.waitSampling() // settle input
        // align to a known phase: wait until value returns to 0 so our window is an integer number of periods
        while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
        val periods = 7
        val cycles = 2 * w * periods
        var pulses = 0
        for (_ <- 0 until cycles) {
          dut.clockDomain.waitSampling()
          if (dut.willOverflow.toBoolean) pulses += 1
        }
        assert(pulses == periods, s"width=$w: got $pulses pulses in $cycles cycles, expected $periods")
      }
    }
  }

  test("clkDiv is a 50% duty cycle divide-by-(2*width)") {
    for (w <- Seq(2, 3, 4, 5, 6, 8)) {
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        dut.inc #= true
        dut.clockDomain.waitSampling()
        while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
        val cycles = 2 * w * 20 // whole number of periods
        var highs = 0
        for (_ <- 0 until cycles) {
          dut.clockDomain.waitSampling()
          if (dut.clkDiv.toBoolean) highs += 1
        }
        assert(highs * 2 == cycles, s"width=$w: clkDiv high on $highs/$cycles cycles (expected ${cycles / 2})")
      }
    }
  }

  test("clear returns the counter to 0 from a legal non-zero state") {
    val w = 4
    SimConfig.compile(new Dut(w)).doSim { dut =>
      startAndExitReset(dut)
      val legal = legalSequence(w)

      dut.inc #= true
      dut.clockDomain.waitSampling() // burn
      for (_ <- 0 until 3) dut.clockDomain.waitSampling() // three increments → legal(3) = 0111
      assert(dut.value.toBigInt == legal(3), s"setup: got ${dut.value.toBigInt}, expected ${legal(3)}")

      // Deassert inc, assert clr. Burn one cycle for the input change to reach the register.
      dut.inc #= false
      dut.clr #= true
      dut.clockDomain.waitSampling() // burn: old inputs still in effect at this edge
      dut.clockDomain.waitSampling() // clr now sampled, register captures 0
      assert(dut.value.toBigInt == 0, s"after clr: got ${dut.value.toBigInt}, expected 0")
    }
  }

  test("clear beats increment when both asserted") {
    SimConfig.compile(new Dut(4)).doSim { dut =>
      startAndExitReset(dut)
      dut.inc #= true
      dut.clockDomain.waitSampling() // burn
      for (_ <- 0 until 3) dut.clockDomain.waitSampling()
      assert(dut.value.toBigInt != 0, "setup: counter should have advanced")

      dut.clr #= true // inc stays true
      dut.clockDomain.waitSampling() // burn clr change
      dut.clockDomain.waitSampling() // clr sampled with inc; clr wins
      assert(dut.value.toBigInt == 0, s"clear+inc: got ${dut.value.toBigInt}, expected 0")
    }
  }

  test("self-recovers from every illegal state within 2*width increments") {
    for (w <- Seq(3, 4, 5, 6)) {
      val legal = legalSequence(w).toSet
      val illegal = illegalStates(w)
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        val limit = 2 * w
        for (state <- illegal) {
          // Clear and inject the illegal state via the force override
          dut.inc #= false
          dut.forceEn #= true
          dut.forceVal #= state
          dut.clockDomain.waitSampling() // burn: forceEn change not yet sampled
          dut.clockDomain.waitSampling() // edge: forceEn sampled, register = forceVal
          assert(dut.value.toBigInt == state,
            s"width=$w: inject of ${state.toString(2)} failed, got ${dut.value.toBigInt.toString(2)}")

          // Free-run and look for recovery into the legal set
          dut.forceEn #= false
          dut.inc #= true
          dut.clockDomain.waitSampling() // burn: new inputs not yet in effect
          var steps = 0
          var recovered = false
          while (!recovered && steps < limit) {
            dut.clockDomain.waitSampling()
            steps += 1
            if (legal.contains(dut.value.toBigInt)) recovered = true
          }
          assert(recovered,
            s"width=$w: state ${state.toString(2)} did not re-enter legal set within $limit increments " +
              s"(stuck at ${dut.value.toBigInt.toString(2)})")
        }
      }
    }
  }

  test("illegal-state recovery eventually reaches 0") {
    // Stronger property: after recovery, the legal walk brings us back to 0 quickly
    for (w <- Seq(3, 4, 5, 6)) {
      SimConfig.compile(new Dut(w)).doSim { dut =>
        startAndExitReset(dut)
        for (state <- illegalStates(w)) {
          dut.inc #= false
          dut.forceEn #= true
          dut.forceVal #= state
          dut.clockDomain.waitSampling()
          dut.clockDomain.waitSampling()

          dut.forceEn #= false
          dut.inc #= true
          dut.clockDomain.waitSampling()
          var steps = 0
          var hitZero = false
          val limit = 4 * w
          while (!hitZero && steps < limit) {
            dut.clockDomain.waitSampling()
            steps += 1
            if (dut.value.toBigInt == 0) hitZero = true
          }
          assert(hitZero,
            s"width=$w: state ${state.toString(2)} never reached 0 within $limit increments")
        }
      }
    }
  }

  test("freeRun() increments without an external enable") {
    val w = 4
    SimConfig.compile(new Component {
      val jc = JohnsonCounter(w).freeRun()
      val value = out Bits (w bits)
      value := jc.value
    }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(20)
      // Align: freeRun started advancing as soon as reset released. Wait for a known anchor.
      while (dut.value.toBigInt != 0) dut.clockDomain.waitSampling()
      val legal = legalSequence(w)
      for (i <- 1 to 2 * legal.size) {
        dut.clockDomain.waitSampling()
        val expected = legal(i % legal.size)
        assert(dut.value.toBigInt == expected,
          s"freeRun cycle $i: got ${dut.value.toBigInt}, expected $expected")
      }
    }
  }
}
