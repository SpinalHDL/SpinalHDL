package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalStreamExtender extends SpinalFormalFunSuite {
  def counterTester() {
    FormalConfig
      // .withBMC(10)
      .withProve(10)
      .withCover(10)
      .doVerify(new Component {
        val inStream = slave Stream (UInt(2 bits))
        val outStream = master Stream (UInt(2 bits))
        val count = in UInt (3 bits)
        val dut = FormalDut(StreamTransactionCounter(inStream, outStream, count, false))

        val inReady = in Bool ()
        inStream.ready := inReady
        val outValid = in Bool ()
        outStream.valid := outValid
        val outPayload = in UInt (2 bits)
        outStream.payload := outPayload

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val countHist = History(count, 2, inStream.fire, init = count.getZero)
        when(!dut.io.available) { assume(inReady === False) }

        when(pastValid & past(!reset & inStream.fire)) { assert(dut.io.working) }
        when(pastValid & past(dut.io.done & !inStream.fire)) { assert(!dut.io.working) }

        when(dut.io.done) { assert(dut.counter.value >= countHist(1)) }
        when(dut.io.working) {
          assert(countHist(1) === dut.expected) // key to sync verification logic and internal logic.
          when(dut.counter.value === countHist(1) & outStream.fire) { assert(dut.io.done) }
          when(!dut.io.done) { assert(!dut.io.available) }
          .otherwise { assert(dut.io.available) }
        }
        .otherwise { assert(dut.io.available) }
        val counterHelper = dut.withAsserts()

        cover(inStream.fire & outStream.fire & dut.io.done)
        cover(pastValid & past(dut.io.working) & !dut.io.working)

        inStream.withAssumes()
        outStream.withAssumes()

        for(i <- 1 until 2) {
          inStream.withCovers(i)
          outStream.withCovers(i)
        }
      })
  }

  def counterNoDelayTester() {
    FormalConfig
      // .withBMC(10)
      .withProve(10)
      .withCover(10)
      .doVerify(new Component {
        val inStream = slave Stream (UInt(2 bits))
        val outStream = master Stream (UInt(2 bits))
        val count = in UInt (3 bits)
        val dut = FormalDut(StreamTransactionCounter(inStream, outStream, count, true))

        val inReady = in Bool ()
        inStream.ready := inReady
        val outValid = in Bool ()
        outStream.valid := outValid
        val outPayload = in UInt (2 bits)
        outStream.payload := outPayload

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val countHist = History(count, 2, inStream.fire, init = count.getZero)
        val expected = countHist(1).getAheadValue()
        when(!dut.io.available) { assume(inReady === False) }

        when(inStream.fire) { assert(dut.io.working) }
        when(pastValid & past(dut.io.done) & !inStream.fire) { assert(!dut.io.working) }

        when(dut.io.done) { assert(dut.counter.value >= expected) }
        when(dut.io.working) {
          assert(expected === dut.expected) // key to sync verification logic and internal logic.
          when(dut.counter.value === expected & outStream.fire) { assert(dut.io.done) }
          when(!inStream.fire) { assert(!dut.io.available) }
          .otherwise { assert(dut.io.available) }
        }
        .otherwise { assert(dut.io.available) }
        cover(pastValid & past(dut.io.done) & inStream.fire)
        cover(pastValid & past(dut.io.working) & !dut.io.working)

        inStream.withAssumes()
        outStream.withAssumes()

        for(i <- 1 until 2) {
          inStream.withCovers(i)
          outStream.withCovers(i)
        }
      })
  }

  def extenderTester() {
    FormalConfig
      // .withBMC(10)
      .withProve(10)
      .withCover(10)
      .doVerify(new Component {
        val inStream = slave Stream (UInt(2 bits))
        val outStream = master Stream (UInt(2 bits))
        val count = in UInt (3 bits)
        val dut = FormalDut(StreamTransactionExtender(inStream, outStream, count) { (_, p, _) => p })

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val countHist = History(count, 2, inStream.fire, init = count.getZero)

        when(pastValid & past(!reset & inStream.fire)) { assert(dut.io.working) }
        when(pastValid & past(dut.io.done & !inStream.fire)) { assert(!dut.io.working) }

        when(dut.io.done) { assert(dut.counter.counter.value >= countHist(1)) }
        when(dut.io.working) {
          assert(countHist(1) === dut.counter.expected) // key to sync verification logic and internal logic.
          when(dut.counter.counter.value === countHist(1) & outStream.fire) { assert(dut.io.done) }
          when(!dut.io.done) { assert(!inStream.ready) }
        }
        cover(inStream.fire & outStream.fire & dut.io.done)
        cover(pastValid & past(dut.io.working) & !dut.io.working)

        inStream.withAssumes()
        outStream.withAssumes()

        for(i <- 1 until 2) {
          inStream.withCovers(i)
          outStream.withCovers(i)
        }
      })
  }
  

  def extenderNoDelayTester() {
    FormalConfig
      // .withBMC(10)
      .withProve(10)
      .withCover(10)
      .doVerify(new Component {
        val inStream = slave Stream (UInt(2 bits))
        val outStream = master Stream (UInt(2 bits))
        val count = in UInt (3 bits)
        val dut = FormalDut(StreamTransactionExtender(inStream, outStream, count, true) { (_, p, _) => p })

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val countHist = History(count, 2, inStream.fire, init = count.getZero)
        val expected = countHist(1).getAheadValue()

        when(inStream.fire) { assert(dut.io.working) }
        when(pastValid & past(dut.io.done) & !inStream.fire) { assert(!dut.io.working) }

        when(dut.io.done) { assert(dut.counter.counter.value >= expected) }
        when(dut.io.working) {
          assert(expected === dut.counter.expected) // key to sync verification logic and internal logic.
          when(dut.counter.counter.value === expected & outStream.fire) { assert(dut.io.done) }
          when(!inStream.fire) { assert(!dut.counter.io.available) }
          .otherwise { assert(dut.counter.io.available) }
          when(!dut.io.input.fire) { assert(!inStream.ready) }
          when(dut.io.input.fire) { assert(dut.io.input.payload === dut.io.output.payload) }
          .otherwise { assert(dut.io.output.payload === dut.payloadReg) }
        }
        cover(pastValid & past(dut.io.done) & inStream.fire)
        cover(pastValid & past(dut.io.working) & !dut.io.working)

        inStream.withAssumes()
        outStream.withAssumes()

        for(i <- 1 until 2) {
          inStream.withCovers(i)
          outStream.withCovers(i)
        }
      })
  }

  test("transaction counter") {
    counterTester()
  }

  test("transaction counter without delay") {
    counterNoDelayTester()
  }

  test("transaction extender") {
    extenderTester()
  }

  test("transaction extender without delay") {
    extenderNoDelayTester()
  }
}
