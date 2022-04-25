package spinal.tester.scalatest

import spinal.core._
import spinal.lib._

class FormalSimpleTester extends SpinalFormalFunSuite {
  import spinal.core.formal._
  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  test("fifo_order") {
    FormalConfig
      .withProve(10)
      .doVerify(new Component {
        val dut = StreamFifo(UInt(7 bits), 4)
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inValue = anyseq(UInt(7 bits))
        val inValid = anyseq(Bool())
        val outReady = anyseq(Bool())
        dut.io.push.payload := inValue
        dut.io.push.valid := inValid
        dut.io.pop.ready := outReady

        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.formalHold()
        dut.io.pop.formalHold()

        val d1 = anyconst(UInt(7 bits))
        val d1_in = dut.io.push.formalCreateEvent { x => x.fire && x.payload === d1 }
        val d1_out = dut.io.pop.formalCreateEvent { x => x.fire && x.payload === d1 }
        when(d1_out) { assert(d1_in === True) }

        val d2 = anyconst(UInt(7 bits))
        val d2_in = dut.io.push.formalCreateEvent { x => x.fire && x.payload === d2 }
        val d2_out = dut.io.pop.formalCreateEvent { x => x.fire && x.payload === d2 }
        when(d2_out) { assert(d2_in === True) }

        assume(d1 =/= d2)
        when(d2_in) { assume(d1_in === True) }
        when(d2_out && d2_in) { assert(d1_out === True) }
      })
  }

  //Little thing which will provide a done pulse after "delay" cycles following a start pulse. Only manage a single request at the time


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

  def shouldFail(body : => Unit) = assert(try{
    body
    false
  } catch{
    case e => println(e); true
  })

  test("StartDoneDut pass") {
    startDoneTest(16)
  }
  test("StartDoneDut fail") {
    shouldFail(startDoneTest(15))
  }
}
