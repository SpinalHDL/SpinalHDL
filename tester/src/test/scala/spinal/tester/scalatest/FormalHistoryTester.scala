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
        val input = anyseq(Flow(UInt(4 bits)))
        val dut = HistoryModifyable(input, depth)
        val results = Vec(master(Stream(input.payloadType)), depth - 1)
        val controls = Vec(slave(Stream(input.payloadType)), depth - 1)
        dut.io.outStreams.zip(results).map(x => x._1 >> x._2)
        dut.io.inStreams.zip(controls).map(x => x._2 >> x._1)

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        results.map(x => when(input.valid) { assume(input.payload =/= x.payload) })

        val exists = CountOne(results.map(_.valid))
        val outFired = CountOne(results.map(_.fire))
        results.map(x => when(past(x.fire)){ assert(exists === past(exists - outFired + U(input.valid))) })

        if (outOnly) {
          controls.map(x => assume(x.valid === False))
        } else {
          controls.map(x => cover(x.fire))
          cover(results(1).fire && controls(1).fire)
        }

        results.map(x => cover(x.fire))
        results.map(x => when(x.valid) { assume(x.payload =/= input.payload) })
        cover(results(0).fire && results(2).fire)
      })
  }
}
