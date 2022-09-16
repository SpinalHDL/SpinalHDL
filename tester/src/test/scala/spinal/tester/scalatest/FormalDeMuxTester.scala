package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import scala.util.Random

class FormalDeMuxTester extends SpinalFormalFunSuite {
  test("demux-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))

        val muxSelect = anyseq(UInt(log2Up(portCount) bit))
        val muxInput = slave(Stream(dataType))
        val muxOutputs = Vec(master(Stream(dataType)), portCount)

        dut.io.select := muxSelect
        muxInput >> dut.io.input
        for (i <- 0 until portCount) {
          muxOutputs(i) << dut.io.outputs(i)
        }

        cover(muxInput.fire)

        for (i <- 0 until portCount) {
          cover(dut.io.select === i)
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
}
