package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib.{StreamFork, History}
import spinal.lib.formal._

class FormalForkTester extends SpinalFormalFunSuite {
  test("fork-verify all") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new StreamFork(UInt(8 bits), 2, true))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inValue = anyseq(UInt(8 bits))
        val inValid = anyseq(Bool())
        val outReady_0 = anyseq(Bool())
        val outReady_1 = anyseq(Bool())

        dut.io.input.payload := inValue
        dut.io.input.valid := inValid

        dut.io.outputs(0).ready := outReady_0
        dut.io.outputs(1).ready := outReady_1


        when(reset || past(reset)) {
          assume(inValid === False)
        }

        assert(dut.io.input.ready === (outReady_0 && outReady_1))
        
        when((!outReady_0 || !outReady_1) && dut.io.outputs(0).valid) {          
          assert(dut.io.outputs(0).payload === past(dut.io.outputs(0).payload))
          assert(dut.io.outputs(0).valid === past(dut.io.outputs(0).valid))
          }
        
        assert(dut.io.outputs(0).payload === dut.io.outputs(1).payload)
        assert(dut.io.outputs(0).valid === dut.io.outputs(1).valid)        

        assert(dut.io.outputs(0).valid === (dut.io.input.valid && dut.io.input.ready))
      })
  }
}

