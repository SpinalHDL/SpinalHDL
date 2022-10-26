package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalForkTester extends SpinalFormalFunSuite {
  def formalfork (synchronous: Boolean = false, back2BackCheck: Boolean = false ) = {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 2
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamFork(dataType, portCount, synchronous))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Stream(dataType))
        val outputs = Vec(master(Stream(dataType)), portCount)

        input >> dut.io.input
        for (i <- 0 until portCount) {
          outputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(input.valid === False)
        }

        cover(input.fire)

        input.formalAssumesSlave()
        if(back2BackCheck) input.formalCovers(3)

        for (i <- 0 until portCount) {
          assert(outputs(i).payload === input.payload)
          outputs(i).formalAssertsMaster()
          if(back2BackCheck) outputs(i).formalCovers(3)          
        }
        
        val fired = if(!synchronous)Vec(RegInit(True), portCount) else null // store fire status, 
        val outputsFireAsync = if(!synchronous){fired(0) =/= fired(1)} else null
        val out0FireFirst = if(!synchronous){fired(1) && !fired(0)} else null
        val out1FireFirst = if(!synchronous){fired(0) && !fired(1)} else null
        val out0FireLast = if(!synchronous){~(input.fire ^ outputs(0).fire)} else null
        val out1FireLast = if(!synchronous){~(input.fire ^ outputs(1).fire)} else null        

        if(synchronous){
          for (i <- 0 until portCount) {
            assert(outputs(i).fire === input.fire)
            
          }
        }
        else{ 
          for(i <- 0 until portCount){
            cover(input.ready)
            when(input.ready){
              fired(i) := True                            
            }
            cover(!input.ready && outputs(i).fire)
            when(!input.ready && outputs(i).fire){
              fired(i) := False              
            }

            assert(fired(i) === dut.logic.linkEnable(i))
          }          
          
          cover(out0FireFirst && !outputs(1).fire)
          cover(out1FireFirst && !outputs(0).fire)

          when(outputsFireAsync) {
            assert(!past(input.fire))
          }

          when(out0FireFirst) {
            assert(out1FireLast) 
            assert(!outputs(0).valid)
          }

          when(out1FireFirst) {
            assert(out0FireLast) 
            assert(!outputs(1).valid)
          }
        }       
      })
  }

  test("fork-verify sync") {
    formalfork(true,false)
  }

  test("fork-verify sync back2Back fail") {
    shouldFail(formalfork(true,true))
  }
  
  test("fork-verify async") {
    formalfork(false,true)
  }
}