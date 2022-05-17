package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._

class FormalHistoryModifyableTester extends SpinalFormalFunSuite {
  test("pop_any") {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val outOnly = false
        val depth = 4
        val input = anyseq(Flow(UInt(6 bits)))
        val dut = HistoryModifyable(input, depth)
        val results = Vec(master(Stream(input.payloadType)), depth - 1)
        val controls = Vec(slave(Stream(input.payloadType)), depth - 1)
        dut.io.outStreams.zip(results).map(x => x._1 >> x._2)
        dut.io.inStreams.zip(controls).map(x => x._2 >> x._1)

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        when(input.valid) { results.map(x => assume(input.payload =/= x.payload)) }

        val exists = CountOne(results.map(_.valid))
        val outFired = CountOne(results.map(_.fire))
        results.map(x => when(past(x.fire)) { assert(exists === past(exists - outFired + U(input.valid))) })

        val dataOut = anyconst(cloneOf(input.payload))
        def diffPayload(target: Vec[Stream[UInt]])(x: Stream[UInt]) = {
          target.filter(y => x != y).map(y => x.payload =/= y.payload).reduce(_ && _)
        }
        // assert(results.map(diffPayload(results)).reduce(_ && _))

        if (outOnly) {
          controls.map(x => assume(x.valid === False))
        } else {
          assume(controls.map(diffPayload(controls)).reduce(_ && _))
          when(input.valid) { assume(controls.map(x => input.payload =/= x.payload).reduce(_ && _)) }
          controls.map(y => assume(!results.sExist(x => x.valid && x.payload === y.payload)))

          controls.map(x => cover(x.fire))
          results.zip(controls).map(x => cover(x._1.fire && x._2.fire))
        }

        results.map(x =>
          when(past(x.payload === dataOut && x.fire)) {
            assert(!results.sExist(x => x.valid && x.payload === dataOut))
          }
        )
        results.map(x => cover(x.fire))
        results.map(x => when(x.valid) { assume(x.payload =/= input.payload) })
        cover(results(0).fire && results(2).fire)
      })
  }
}
