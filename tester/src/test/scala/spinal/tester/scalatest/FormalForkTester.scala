package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib.{StreamFork, History}
import spinal.lib.formal._


class FormalForkTester extends SpinalFormalFunSuite {
  test("fork-verify sync") {    
    FormalConfig
      .withBMC(20)
      .withProve(20)
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
        
        dut.io.outputs(0).withAsserts()
        dut.io.outputs(1).withAsserts()  

      })
  }
  test("fork-verify async") {    
    FormalConfig
      .withBMC(20)
      .withProve(20)
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

        val fetched = Vec(RegInit(False),2)    //store fire status, 1: result fetched, 0: didn't fetch
        when (input.ready) {
          fetched(0) := False
          fetched(1) := False
        }
        when ((!input.ready) && output_0.fire) {
          fetched(0) := True
          assert (output_0.payload === input.payload)
        } 
        when ((!input.ready) && output_1.fire) {
          fetched(1) := True
          assert (output_1.payload === input.payload)
        }
        assert (fetched(0) === !dut.logic.linkEnable(0))
        assert (fetched(1) === !dut.logic.linkEnable(1)) 

        val async = fetched(0) =/= fetched(1)
        val async_0_fired = async && fetched(0) && past(fetched(0))//for a signle transform. o0 fetched but o1 didn't
        val async_1_fired = async && fetched(1) && past(fetched(1))
        val in_f_xnor_o0_f = (input_fire && output_0.fire) || (!(input_fire || output_0.fire))
        val in_f_xnor_o1_f = (input_fire && output_1.fire) || (!(input_fire || output_1.fire))

        cover(async_0_fired && !output_1.fire)
        cover(async_1_fired && !output_0.fire)
        when(async){
          assert(!past(input.fire))
        }
        when(async_0_fired){          
          assert(in_f_xnor_o1_f)
          assert(!output_0.fire)
        }
        when(async_1_fired){
          assert(in_f_xnor_o0_f)
          assert(!output_1.fire)
        }       
      })
  }
}

