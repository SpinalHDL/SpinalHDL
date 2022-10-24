package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

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
        val results = Vec(master(Stream(input.payloadType)), depth)
        val controls = Vec(slave(Stream(input.payloadType)), depth)
        dut.io.outStreams.zip(results).map { case (from, to) => from >> to }
        dut.io.inStreams.zip(controls).map { case (to, from) => from >> to }

        def outCount(data: UInt) = { results.sCount(x => x.valid && x.payload === data) }
        def outExists(data: UInt) = { results.sExist(x => x.valid && x.payload === data) }

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        when(input.valid) {
          assume(!outExists(input.payload))
        }
        when(pastValid & past(!reset & input.valid)) { assert(results(0).valid && results(0).payload === past(input.payload)) }

        val dataOverflow = anyconst(cloneOf(input.payload))
        assert(
          (input.valid && results.sCount(_.valid) === depth && results.sCount(_.fire) === 0) === dut.io.willOverflow
        )
        val overflowModify = controls.sExist(x => x.fire && x.payload === dataOverflow)
        val overflowCount = outCount(dataOverflow)
        when(past(dut.io.willOverflow && results.last.payload === dataOverflow && !overflowModify)) {
          assert(results.last.valid && past(overflowCount) > overflowCount)
        }

        val dataOut = anyconst(cloneOf(input.payload))
        val dataIn = anyconst(cloneOf(input.payload))

        if (outOnly) {
          controls.map(x => assume(x.valid === False))
        } else {
          assume(controls.map(x => x.payload =/= dataOut).reduce(_ && _))

          controls
            .zip(results)
            .map {
              case (in, out) => {
                val inputFire = if (in == controls.last) input.valid else False
                when(past(in.payload === dataIn && in.fire && !out.fire && !inputFire) && past(!outExists(dataIn))) {
                  assert(outExists(dataIn))
                }
              }
            }

          controls.map(x => cover(x.fire))
          results.zip(controls).map { case (out, in) => cover(in.fire && out.fire) }
        }

        val inputCount = U(input.valid && input.payload === dataOut)
        val validCount = outCount(dataOut)

        val overflowCondition = dut.io.willOverflow && results.last.payload === dataOut
        def modifying(in: Stream[UInt], out: Stream[UInt]) = {
          out.valid && !out.ready && out.payload === dataOut && in.fire
        }
        val modifyCount = CountOne(
          controls
            .filter(_ != controls.last)
            .zip(results.filter(_ != results.last))
            .map { case (in, out) => modifying(in, out) }
        ) +^ U(modifying(controls.last, results.last) && !overflowCondition)
        val outOverflowCount = U(overflowCondition)
        val fireCount = results.sCount(x => x.fire && x.payload === dataOut)

        when(pastValid & !past(reset)) {
          assert(past(validCount - fireCount + inputCount - outOverflowCount - modifyCount) === validCount)
        }
        results.map(x => cover(x.fire))
        cover(results(0).fire && results(2).fire)
      })
  }
}
