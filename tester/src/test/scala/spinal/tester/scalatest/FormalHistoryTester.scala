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

        when(input.valid) { results.map(x => when(x.valid) { assume(input.payload =/= x.payload) }) }

        val exists = CountOne(results.map(_.valid))
        val outFired = CountOne(results.map(_.fire))
        results.map(x => when(past(x.fire)) { assert(exists === past(exists - outFired + U(input.valid))) })

        val dataOut = anyconst(cloneOf(input.payload))
        val dataIn = anyconst(cloneOf(input.payload))
        assume(dataOut =/= dataIn)

        if (outOnly) {
          controls.map(x => assume(x.valid === False))
        } else {
          assume(controls.map(x => x.payload =/= dataOut).reduce(_ && _))

          controls
            .zip(results)
            .map(x => {
              val inputFire = if (x._1 == controls.last) input.valid else False
              when(past(x._1.payload === dataIn && x._1.fire && !x._2.fire && !inputFire)) {
                assert(results.sExist(y => y.valid && y.payload === dataIn))
              }
            })

          controls.map(x => cover(x.fire))
          results.zip(controls).map(x => cover(x._1.fire && x._2.fire))
        }

        val inputCount = U(input.valid && input.payload === dataOut)
        val validCount = results.sCount(x => x.valid && x.payload === dataOut)

        assert(
          (input.valid && results.sCount(_.valid) === depth - 1 && results.sCount(_.fire) === 0) === dut.io.willOverflow
        )
        val overflowCondition = dut.io.willOverflow && results.last.payload === dataOut
        def modifying(in: Stream[UInt], out: Stream[UInt]) = {
          out.valid && !out.ready && out.payload === dataOut && in.fire
        }
        val modifyCount = CountOne(
          controls
            .filter(_ != controls.last)
            .zip(results.filter(_ != results.last))
            .map(x => modifying(x._1, x._2))
        ) + U(modifying(controls.last, results.last) && !overflowCondition)
        val overflowCount = U(overflowCondition)
        val fireCount = results.sCount(x => x.fire && x.payload === dataOut)

        when(withPast()) {
          assert(past(validCount - fireCount + inputCount - overflowCount - modifyCount) === validCount)
        }
        results.map(x => cover(x.fire))
        cover(results(0).fire && results(2).fire)
      })
  }
}
