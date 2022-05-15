package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._

class FormalHistoryPopAnyTester extends SpinalFormalFunSuite {
  test("pop_any") {
    FormalConfig
    //   .withBMC(10)
    //   .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val input = anyseq(UInt(4 bits))
        val cond = anyseq(Bool())
        val dut = HistoryPopAny(input, 4, cond=cond, init=U(0, 4 bits))
        val result = Vec(master(Stream(input.clone)), 3)
        dut.zip(result).map(x => x._1 >> x._2)
        
        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        when(cond) { assume(input =/= past(input)) }

        dut.map(x => cover(x.fire))
        dut.map(x => when(x.valid) { assume(x.payload =/= input) })
        // dut.map(x => x.withAsserts())
        cover(dut(0).fire && dut(2).fire)
      })
  }
}