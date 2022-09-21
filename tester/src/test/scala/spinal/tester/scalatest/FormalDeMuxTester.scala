package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import scala.util.Random

class FormalDeMuxTester extends SpinalFormalFunSuite {
  def formaldemux(selWithCtrl: Boolean = false) = {
    FormalConfig
      .withBMC(30)
      .withProve(20)
      .withCover(30)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val muxSelect = anyseq(UInt(log2Up(portCount) bit))
        val muxInput = slave(Stream(dataType))
        val muxOutputs = Vec(master(Stream(dataType)), portCount)

        dut.io.select := muxSelect
        muxInput >> dut.io.input

        when(reset || past(reset)) {
          assume(muxInput.valid === False)
        }

        if (selWithCtrl) {
          val selStableCond = muxOutputs(muxSelect).isStall || past(muxOutputs(muxSelect).isStall)
          cover(selStableCond)
          when(selStableCond) {
            assume(stable(muxSelect))
          }
        }

        assumeInitial(muxSelect < portCount)
        cover(muxInput.fire)
        muxInput.withAssumes()

        for (i <- 0 until portCount) {
          muxOutputs(i) << dut.io.outputs(i)
          muxOutputs(i).withAsserts()
        }

        for (i <- 0 until portCount) {
          assert(muxOutputs(i).payload === muxInput.payload)
          when(i =/= muxSelect) {
            assert(muxOutputs(i).valid === False)
          }
        }
        when(muxSelect < portCount) {
          assert(muxOutputs(muxSelect) === muxInput)
        }
      })
  }
  test("demux_sel_with_control") {
    formaldemux(true)
  }
  test("demux_sel_without_control") {
    shouldFail(formaldemux(false))
  }
}
