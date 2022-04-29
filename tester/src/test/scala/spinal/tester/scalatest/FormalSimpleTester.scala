package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import FormalFifo._

class FormalSimpleTester extends SpinalFormalFunSuite {
  import spinal.core.formal._
  import spinal.core.GenerationFlags._

  def shouldFail(body : => Unit) = assert(try{
    body
    false
  } catch{
    case e => println(e); true
  })


//  for(error <- Error.all;
//      trigger <- Trigger.all){
//    test(s"fifo-prove-$error-$trigger"){
//      shouldFail(fifoProveTest(error, trigger))
//    }
//  }
//  test("fifo-prove ok"){ fifoProveTest(null, Trigger.ANY) }

  for(error <- Error.all;
      trigger <- Trigger.all;
      if !(List(Error.DROP_POP, Error.DROP_PUSH, Error.CORRUPT).contains(error) && trigger == Trigger.D2)){
    test(s"fifo-bmc-$error-$trigger"){
      shouldFail(fifoBmcTest(error, trigger))
    }
  }
  test("fifo-bmc ok"){ fifoBmcTest(null, Trigger.ANY, 10) }

  def startDoneTest(maxDelay : Int): Unit ={
    class StartDoneDut() extends Component {
      val start = in(Bool())
      val done = out(False)
      val delay = in(UInt(4 bits))
      val counter = Reg(UInt(4 bits))
      val busy = RegInit(False)
      when(!busy) {
        when(start){
          busy := True
          counter := 0
        }
      } otherwise {
        counter := counter + 1
        when(counter === delay){
          busy := False
          done := True
        }
      }
    }

    FormalConfig.withProve(25).doVerify(new Component {
      val dut = new StartDoneDut()
      assumeInitial(ClockDomain.current.isResetActive)

      val busy = RegInit(False) setWhen (dut.start) clearWhen (dut.done)
      anyseq(dut.start)
      anyconst(dut.delay)

      val timeout = Timeout(maxDelay)
      when(!busy) {
        when(dut.start) {
          timeout.clear()
        }
      } otherwise {
        assert(!timeout.state)
      }
    })
  }


  test("StartDoneDut pass") {
    startDoneTest(16)
  }
  test("StartDoneDut fail") {
    shouldFail(startDoneTest(15))
  }
}
