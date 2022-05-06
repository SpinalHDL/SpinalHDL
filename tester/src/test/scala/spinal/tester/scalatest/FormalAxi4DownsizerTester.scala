package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib.{History, Counter}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Config, Axi4Downsizer, Axi4WriteOnlyDownsizer, Axi4ReadOnlyDownsizer}

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def tester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
    //   .withBMC(10)
    //   .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new Axi4WriteOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        anyseq(dut.io.input.aw.payload)
        anyseq(dut.io.input.w.payload)
        anyseq(dut.io.input.b.ready)
        anyseq(dut.io.input.w.valid)
        anyseq(dut.io.input.aw.valid)

        anyseq(dut.io.output.aw.ready)
        anyseq(dut.io.output.w.ready)
        anyseq(dut.io.output.b.payload)
        anyseq(dut.io.output.b.valid)
        
        // dut.io.input.aw.withAssumes()
        // dut.io.input.w.withAssumes()
        // dut.io.input.b.withAsserts()

        // dut.io.output.b.withAssumes()
        // dut.io.output.aw.withAsserts()
        // dut.io.output.w.withAsserts()
        
        // when(reset || past(reset)) {
        //   assume(dut.io.input.w.valid === False)
        //   assume(dut.io.input.aw.valid === False)
        //   assume(dut.io.output.b.valid === False)
        // }

        // when(dut.io.input.aw.valid){
        //     dut.io.input.aw.payload.withAssumes()
        // }
        // when(dut.io.output.aw.valid){
        //     dut.io.output.aw.payload.withAsserts()
        // }
        dut.io.input.withAssumes()
        dut.io.output.withAsserts()
        
        val maxStall = 20
        val inTimeOut = Counter(maxStall, dut.io.input.aw.isStall)
        when(dut.io.input.aw.fire) {
            inTimeOut.clear()
        } otherwise {
            assert(!inTimeOut.willOverflow)
        }

        val outTimeOut = Counter(maxStall, dut.io.output.aw.isStall)
        when(dut.io.output.aw.fire) {
            outTimeOut.clear()
        } elsewhen(outTimeOut.willOverflow) {
            assume(dut.io.output.aw.ready === True)
        }

        dut.io.output.withCovers()
        dut.io.input.withCovers()
        // cover(dut.io.output.b.fire)
        // cover(dut.io.input.b.fire)
      })
  }

  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false)
  test("64_32") {
    tester(inConfig, outConfig)
  }
}
