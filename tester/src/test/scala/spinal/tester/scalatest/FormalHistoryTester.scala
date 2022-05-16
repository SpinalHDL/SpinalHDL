package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._

class FormalHistoryModifyableTester extends SpinalFormalFunSuite {
  test("pop_any") {
    FormalConfig
    //   .withBMC(10)
    //   .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val depth = 4
        val input = anyseq(Flow(UInt(4 bits)))
        val dut = HistoryModifyable(input, depth)
        val result = Vec(master(Stream(input.payloadType)), depth - 1)
        val control = Vec(slave(Stream(input.payloadType)), depth - 1)
        dut.io.outStreams.zip(result).map(x => x._1 >> x._2)
        dut.io.inStreams.zip(control).map(x => x._2 >> x._1)
        
        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        when(input.valid) { assume(input.payload =/= past(input.payload)) }

        dut.io.inStreams.map(x => cover(x.fire))
        // control.map(x => assume(x.valid === False)) // no modify.
        dut.io.outStreams.map(x => cover(x.fire))
        dut.io.outStreams.map(x => when(x.valid) { assume(x.payload =/= input.payload) })
        // dut.map(x => x.withAsserts())
        cover(dut.io.outStreams(1).fire && dut.io.inStreams(1).fire)
        cover(dut.io.outStreams(0).fire && dut.io.outStreams(2).fire)
      })
  }
}