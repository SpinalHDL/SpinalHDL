package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.lib.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.graphic.Rgb

import scala.util.Random

class SpinalSimPhaseTester extends SpinalSimFunSuite{
  test("test1") {
    //    val compiled = SimConfig.withWave.compile(StreamFifo(UInt(8 bits),16))
    case class Transaction() extends Bundle {
      val flag = Bool()
      val data = Bits(8 bits)
      val color = Rgb(5, 6, 5)

      override def clone = Transaction()
    }

    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifo(
        dataType = Transaction(),
        depth = 32
      )
    )

    compiled.doSim { dut =>
      SimTimeout(100000 * 10)
      Phase.boot() //Initialise phase. Phases are :  setup -> stimulus -> flush -> check -> end
      Phase.flush.retainFor(1000 * 10) //Give 1000 cycle between the end of push stimulus and check phase to flush the hardware

      //When the stimulus phase start
      Phase.stimulus {
        //Scoreboards
        val popScoreboard = ScoreboardInOrder[SimData]()

        //Drivers
        dut.io.flush #= false
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.pop, dut.clockDomain)
        StreamDriver(dut.io.push, dut.clockDomain) { payload =>
          if (Phase.stimulus.isActive) {
            payload.randomize()
            true
          } else {
            false
          }
        }

        //Monitors
        var pushRetainer = Phase.stimulus.retainer(count = 100)
        StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
          popScoreboard.pushRef(payload)
          pushRetainer.release()
        }
        StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
          popScoreboard.pushDut(payload)
        }
      }
    }
  }
}

class SpinalPhaseVerifier extends SpinalFormalFunSuite {
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

  test("StartDoneDut pass") {
    startDoneTest(16)
  }
  test("StartDoneDut fail") {
    assert(try{
      startDoneTest(15)
      false
    } catch{
      case e => println(e); true
    })
  }
}

//        fork{
//          while(true) {
//            dut.io.pop.ready #= Random.nextDouble() < 0.1
//            dut.clockDomain.waitSampling()
//          }
//        }
