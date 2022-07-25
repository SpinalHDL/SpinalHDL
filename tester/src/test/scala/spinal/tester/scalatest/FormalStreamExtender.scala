package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalStreamExtender extends SpinalFormalFunSuite {
  def counterTester(noDelay: Boolean = false) {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val inStream = slave Stream(UInt(2 bits))
        val outStream = master Stream(UInt(2 bits))
        val count = in UInt(3 bits)
        val dut = FormalDut(StreamTransactionCounter(inStream, outStream, count, noDelay))
        
        val inReady = in Bool()
        inStream.ready := inReady
        val outValid = in Bool()
        outStream.valid := outValid
        val outPayload = in UInt(2 bits)
        outStream.payload := outPayload

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val countHist = History(count, 2, inStream.fire, init = count.getZero)
        when(dut.io.working && !dut.io.done) { assume(inReady === False) }

        when(pastValid & past(inStream.fire)){ assert(dut.io.working) }
        when(past(dut.io.done & !inStream.fire)) { assert(!dut.io.working) }

        val d1 = anyconst(cloneOf(count))
        when(dut.io.done) { assert(dut.counter.value === countHist(1)) }
        when(dut.io.working) {
          assert(countHist(1) === dut.expected) // key to sync verification logic and internal logic.
          when(dut.counter.value === countHist(1) & outStream.fire) { assert(dut.io.done) }
          val updateCond = inStream.fire && (dut.io.count === d1) && (d1 < dut.counter.value)
          when(updateCond) { assert(dut.io.done) }
          cover(updateCond)
        }
        cover(inStream.fire & outStream.fire & dut.io.done)
        cover(past(dut.io.working) & !dut.io.working)

        inStream.withAssumes()
        outStream.withAssumes()

        inStream.withCovers()
        outStream.withCovers()
      })
  }

  def extenderTester() {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val inStream = slave Stream(UInt(2 bits))
        val outStream = master Stream(UInt(2 bits))
        val count = in UInt(4 bits)
        val dut = FormalDut(StreamTransactionExtender(inStream, outStream, count){(_, p, _) => p})

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)
      })
  }
  
  test("transaction counter") {
    counterTester()
  }
  
//   test("transaction extender") {
//     extenderTester()
//   }
}
