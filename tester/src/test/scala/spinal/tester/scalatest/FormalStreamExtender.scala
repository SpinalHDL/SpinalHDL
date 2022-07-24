package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._

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
        when(dut.io.working) {
            assume(inReady === False)
        }

        val verifyCounter = Counter(4 bits, inc = outStream.fire & dut.io.working)
        when(dut.io.done) {
            assert(verifyCounter.value === countHist(1))
            verifyCounter.clear()
        }
        when(dut.io.working) {
            assert(countHist(1) === dut.expected)
            assert(verifyCounter.value === dut.counter.value)
        }
        when(!dut.io.working) { assume(verifyCounter.value === 0) }
        assert(dut.counter.value <= dut.expected)

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
