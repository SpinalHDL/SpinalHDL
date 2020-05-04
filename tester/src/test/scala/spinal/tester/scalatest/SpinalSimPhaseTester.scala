package spinal.tester.scalatest

import org.scalatest.FunSuite
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
      val flag = Bool
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





//        fork{
//          while(true) {
//            dut.io.pop.ready #= Random.nextDouble() < 0.1
//            dut.clockDomain.waitSampling()
//          }
//        }
