package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._
import scala.util.Random

class FormalMuxTester extends SpinalFormalFunSuite {
  test("mux-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamMux(dataType, portCount))

        val muxSelect = anyseq(UInt(log2Up(portCount) bit))
        val muxInputs = Vec(slave(Stream(dataType)), portCount)
        val muxOutput = master(Stream(dataType))

        dut.io.select := muxSelect
        muxOutput << dut.io.output
        for (i <- 0 until portCount) {
          muxInputs(i) >> dut.io.inputs(i)
        }

        cover(muxOutput.fire)

        for (i <- 0 until portCount) {
          cover(dut.io.select === i)
          muxInputs(i).withAssumes()
        }

        when(muxSelect < portCount) {
          assert(muxOutput === muxInputs(muxSelect))
        }
      })
  }
}
