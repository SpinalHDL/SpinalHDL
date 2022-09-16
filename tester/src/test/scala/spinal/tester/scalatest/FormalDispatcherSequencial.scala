package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import scala.util.Random

class FormalDispatcherSequencialTester extends SpinalFormalFunSuite {
  test("DispatcherSequencial-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val reset = ClockDomain.current.isResetActive
        val dut = FormalDut(new StreamDispatcherSequencial(dataType, portCount))

        assumeInitial(reset)

        val muxInput = slave(Stream(dataType))
        val muxOutputs = Vec(master(Stream(dataType)), portCount)

        muxInput >> dut.io.input
        for (i <- 0 until portCount) {
          muxOutputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(muxInput.valid === False)
        }

        cover(muxInput.fire)

        muxInput.withAssumes()
        for (i <- 0 until portCount) {
          cover(dut.counter.value === i)
          muxOutputs(i).withAsserts()
        }

        for (i <- 0 until portCount) {
          when(dut.counter.value =/= i) {
            assert(muxOutputs(i).valid === False)
          } otherwise {
            assert(muxOutputs(i) === muxInput)
          }
        }
      })
  }
}
