package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalForkTester extends SpinalFormalFunSuite {
  test("fork-verify sync") {
    FormalConfig
      .withBMC(20)
      // .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new StreamFork(UInt(8 bits), 2, true))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)
        val input = slave(Stream(UInt(8 bits)))
        val output_0 = master(Stream(UInt(8 bits)))
        val output_1 = master(Stream(UInt(8 bits)))

        input >> dut.io.input
        output_0 << dut.io.outputs(0)
        output_1 << dut.io.outputs(1)

        when(reset || past(reset)) {
          assume(input.valid === False)
        }

        assert(output_0.payload === input.payload)
        assert(output_0.fire === input.fire)
        assert(output_1.payload === input.payload)
        assert(output_1.fire === input.fire)
        cover(output_0.fire && output_1.fire)

        dut.io.outputs(0).withAsserts()
        dut.io.outputs(1).withAsserts()

      })
  }
  test("fork-verify async") {
    FormalConfig
      .withBMC(20)
      // .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new StreamFork(UInt(8 bits), 2, false))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)
        val input = slave(Stream(UInt(8 bits)))
        val output_0 = master(Stream(UInt(8 bits)))
        val output_1 = master(Stream(UInt(8 bits)))
        val input_fire = input.fire

        input >> dut.io.input
        output_0 << dut.io.outputs(0)
        output_1 << dut.io.outputs(1)

        when(reset || past(reset)) {
          assume(input.valid === False)
        }

        val fired = Vec(RegInit(False), 2) // store fire status, 1: result fired, 0: didn't fired
        when(input.ready) {
          fired(0) := False
          fired(1) := False
        }

        when((!input.ready) && output_0.fire) {
          fired(0) := True
          assert(output_0.payload === input.payload)
        }

        when((!input.ready) && output_1.fire) {
          fired(1) := True
          assert(output_1.payload === input.payload)
        }

        assert(fired(0) === !dut.logic.linkEnable(0)) // used for prove
        assert(fired(1) === !dut.logic.linkEnable(1)) // used for prove

        val async = (fired(0) =/= fired(1))
        val async_output0_fired = async && fired(0) && past(fired(0)) // for a signle transform. o0 fired but o1 didn't
        val async_output1_fired = async && fired(1) && past(fired(1)) // for a signle transform. o1 fired but o0 didn't
        val input_fire_sync_output0_fire = ~(input_fire ^ output_0.fire)
        val input_fire_sync_output1_fire = ~(input_fire ^ output_1.fire)

        cover(async_output0_fired && !output_1.fire)
        cover(async_output1_fired && !output_0.fire)

        when(async) {
          assert(!past(input.fire))
        }

        when(async_output0_fired) {
          assert(input_fire_sync_output1_fire) // if output0 fired, input fire should be sync with output1 fire
          assert(!output_0.fire)
        }

        when(async_output1_fired) {
          assert(input_fire_sync_output0_fire) // if output1 fired, input fire should be sync with output0 fire
          assert(!output_1.fire)
        }
      })
  }
}
