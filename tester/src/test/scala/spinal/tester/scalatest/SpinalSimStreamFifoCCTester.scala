package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.StreamFifoCC
import spinal.tester

import scala.collection.mutable
import scala.util.Random

class SpinalSimStreamFifoCCTester extends FunSuite {
  test("test1"){
    //Compile the simulator
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = new StreamFifoCC(
        dataType = Bits(32 bits),
        depth = 32,
        pushClock = ClockDomain.external("clkA"),
        popClock = ClockDomain.external("clkB")
      )
    )

    //Run the simulation
    compiled.doSimUntilVoid{dut =>
      val queueModel = mutable.Queue[Long]()

      //Fork a thread to manage the clock domains signals
      val clocksThread = fork{
        //Clear clock domains signals, to be sure the simulation capture their first edge.
        dut.pushClock.fallingEdge()
        dut.popClock.fallingEdge()
        dut.pushClock.disassertReset()
        dut.popClock.disassertReset()
        sleep(0)

        //Do the resets
        dut.pushClock.assertReset()
        dut.popClock.assertReset()
        sleep(10)
        dut.pushClock.disassertReset()
        dut.popClock.disassertReset()
        sleep(1)

        //Forever, randomly toggle one of the clocks (will create asynchronous clocks without fixed frequencies)
        while(true){
          if(Random.nextBoolean()) {
            dut.pushClock.clockToggle()
          } else {
            dut.popClock.clockToggle()
          }
          sleep(1)
        }
      }

      //Push data randomly and fill the queueModel with pushed transactions
      val pushThread = fork{
        while(true){
          dut.io.push.valid.randomize()
          dut.io.push.payload.randomize()
          dut.pushClock.waitSampling()
          if(dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean){
            queueModel.enqueue(dut.io.push.payload.toLong)
          }
        }
      }

      //Pop data randomly and check that it match with the queueModel
      val popThread = fork{
        Suspendable.repeat(1000000){
          dut.io.pop.ready.randomize()
          dut.popClock.waitSampling()
          if(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean){
            assert(dut.io.pop.payload.toLong == queueModel.dequeue())
          }
        }
        simSuccess()
      }
    }
  }
}
