package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.sim._
import spinal.core._
import spinal.lib._

import scala.util.Random

class SpinalSimPhaseTester extends FunSuite{
  test("test1"){
    val compiled = SimConfig.withWave.compile(StreamFifo(UInt(8 bits),16))
    compiled.doSimUntilVoid{dut =>
      Phase.boot()



      Phase.stimulus{
        dut.clockDomain.forkStimulus(10)
        dut.io.flush #= false

        StreamDriver(dut.io.push, dut.clockDomain) { payload =>
          if(Phase.stimulus.isActive) {
            payload.randomize()
            true
          } else {
            false
          }
        }
//        StreamReadyRandomizer(dut.io.pop, dut.clockDomain)
        fork{
          while(true) {
            dut.io.pop.ready #= Random.nextDouble() < 0.1
            dut.clockDomain.waitSampling()
          }
        }

        val scoreboard = ScoreboardInOrder[SimData]()
        Phase.flush{
          Phase.flush.retain()
          fork{
            waitUntil(scoreboard.ref.isEmpty && scoreboard.dut.isEmpty)
            Phase.flush.release()
          }
        }

        Phase.stimulus.retain()
        var pushCounter = 0
        StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
          scoreboard.pushRef(payload)
          pushCounter += 1
          if(pushCounter == 100) {
            Phase.stimulus.release()
          }
        }
        StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
          scoreboard.pushDut(payload)
        }
      }


    }
  }
}
