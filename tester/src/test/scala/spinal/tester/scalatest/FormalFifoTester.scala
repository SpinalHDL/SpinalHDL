package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib.{StreamFifo, History, OHToUInt}
import spinal.lib.formal._

class FormalFifoTester extends SpinalFormalFunSuite {
  //TODO fix me for other cases
  test("fifo-verify all") {
    val initialCycles = 2
    val inOutDelay = 3
    val depth = 4
    val coverCycles = depth * 2 + initialCycles + inOutDelay
    val asyncRead = false
    val fMax = true
    FormalConfig
      .withBMC(14)
      .withProve(14)
      .withCover(coverCycles)
      // .withDebug
      .doVerify(new Component {
        //val depth = 4
        val dut = FormalDut(new StreamFifo(UInt(7 bits), depth, withAsyncRead=asyncRead, withBypass=false,  forFMax=true))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inValue = anyseq(UInt(7 bits))
        val inValid = anyseq(Bool())
        val outReady = anyseq(Bool())
        dut.io.push.payload := inValue
        dut.io.push.valid := inValid
        dut.io.pop.ready := outReady

        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.formalAssumesSlave()
        dut.io.pop.formalAssertsMaster()

        dut.io.push.formalCovers()
        // back to back transaction cover test.
        dut.io.pop.formalCovers(coverCycles - initialCycles - inOutDelay - 1)

        val d1 = anyconst(UInt(7 bits))
        val d2 = anyconst(UInt(7 bits))

        // assume first d1, then d2 enters the StreamFifo
        val (d1_in, d2_in) = dut.io.push.formalAssumesOrder(d1, d2)
        // assert first d1, then d2 leaves the StreamFifo
        val (d1_out, d2_out) = dut.io.pop.formalAssertsOrder(d1, d2)

        // make sure d1 is not already in the StreamFifo before it enters
        when(!d1_in) { assume(!dut.formalContains(d1)) }
        // make sure d1 only exists once inside the StreamFifo after it entered and before it left
        when(d1_in && !d1_out) { assert(dut.formalCount(d1) === 1) }

        when(!d2_in) { assume(!dut.formalContains(d2)) }
        when(d2_in && !d2_out) { assert(dut.formalCount(d2) === 1) }

        // make sure that d2 does not leave before d1 leaves the StreamFifo
        when(d1_in && d2_in && !d1_out) { assert(!d2_out) }

        // cover the case that FIFO first goes to full, then to empty
        dut.formalFullToEmpty()

        // TODO for .withProve() induction prove to pass, relationship between push/pop distance and some fill rate
        // must be asserted. Currently only asyncRead=false, withBypass=false, withFMax=true use case is supported.
        if (fMax) {
          // initially (1 << log2Up(depth)), minus what is inside the StreamFifo
          assert(dut.logic.ptr.arb.fmax.emptyTracker.value === ((1 << log2Up(depth)) - (dut.logic.ptr.push - dut.logic.ptr.pop)))
          // initially (1 << log2Up(depth)) - depth, plus what is inside the StreamFifo and the optional output stage
          assert(dut.logic.ptr.arb.fmax.fullTracker.value ===  ((1 << log2Up(depth)) - depth + (dut.logic.ptr.push - dut.logic.ptr.pop) + U(dut.io.pop.valid & !Bool(asyncRead))))
          assert((dut.logic.ptr.push - dut.logic.ptr.pop) <= depth)
        // TODO broken?
        } else if (dut.withExtraMsb) {
          assert(dut.logic.ptr.occupancy === (dut.logic.ptr.push - dut.logic.ptr.popOnIo))
          assert(dut.logic.ptr.full === ((dut.logic.ptr.push ^ dut.logic.ptr.popOnIo ^ depth) === 0))
          assert(dut.logic.ptr.empty === (dut.logic.ptr.push === dut.logic.ptr.pop))
        }
        // get order index of x
        // x with lowest index leaves StreamFifo first
        def getCompId(x: UInt): UInt = {
          // index in RAM
          val id = OHToUInt(dut.formalCheckRam(_ === x.pull()).asBits)
          // offset all RAM order indexes by 1, as order index 0 is reserved for m2sPipe()
          // assume wrapped; add RAM size (depth) to location
          val extId = id +^ U(depth + 1)
          val compId = CombInit(extId)
          // for all non wrapped locations
          when(id >= (dut.logic.ptr.pop.resize(log2Up(depth)))) {
            // offset all RAM order indexes by 1, as order index 0 is reserved for m2sPipe()
            compId := (id +^ U(1)).resized
          }
          if (!asyncRead) {
            when(dut.formalCheckOutputStage(_ === x.pull())) {
              // order index 0 for m2sPipe register
              compId := 0
            }
          }
          compId
        }
        when(d1_in && d2_in && !d1_out && !d2_out) {
          assert(getCompId(d1) < getCompId(d2))
        }
      })
  }
}