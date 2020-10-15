package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.{StreamFifo, StreamFifoLowLatency}
import spinal.lib.graphic.Rgb
import spinal.lib.sim._
import spinal.tester

import scala.collection.mutable
import scala.util.Random

class SpinalSimStreamFifoTester extends SpinalSimFunSuite {
  test("testBits") {
    //Compile the simulator
    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifo(
        dataType = Bits(32 bits),
        depth = 32
      )
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[Long]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      dut.io.flush #= false

      //Push data randomly and fill the queueModel with pushed transactions
      dut.io.push.valid #= false
      dut.clockDomain.onSamplings {
        if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
          queueModel.enqueue(dut.io.push.payload.toLong)
        }
        dut.io.push.valid.randomize()
        dut.io.push.payload.randomize()
      }

      //Pop data randomly and check that it match with the queueModel
      val popThread = fork {
        dut.io.pop.ready #= true
        for (repeat <- 0 until 10000) {
          dut.io.pop.ready.randomize()
          dut.clockDomain.waitSampling()
          if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
            assert(dut.io.pop.payload.toLong == queueModel.dequeue())
          }
        }
        simSuccess()
      }
    }
  }


  test("testOne") {
    //Compile the simulator
    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifo(
        dataType = Bits(32 bits),
        depth = 1
      )
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[Long]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      dut.io.flush #= false

      //Push data randomly and fill the queueModel with pushed transactions
      dut.io.push.valid #= false
      dut.clockDomain.onSamplings {
        if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
          queueModel.enqueue(dut.io.push.payload.toLong)
        }
        dut.io.push.valid.randomize()
        dut.io.push.payload.randomize()
      }

      //Pop data randomly and check that it match with the queueModel
      val popThread = fork {
        dut.io.pop.ready #= true
        for (repeat <- 0 until 10000) {
          dut.io.pop.ready.randomize()
          dut.clockDomain.waitSampling()
          if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
            assert(dut.io.pop.payload.toLong == queueModel.dequeue())
          }
        }
        simSuccess()
      }
    }
  }


  test("testBundle") {
    //Bundle used as fifo payload
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

    //Run the simulation
    compiled.doSim { dut =>
      //Inits
      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.forkSimSpeedPrinter()
      dut.io.flush #= false

      val scoreboard = ScoreboardInOrder[SimData]()

      //Drivers
      StreamDriver(dut.io.push, dut.clockDomain) { payload =>
        payload.randomize()
        true
      }
      StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

      //Monitors
      StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload)
      }
      StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload)
      }

      waitUntil(scoreboard.matches == 10000)
    }
  }

  test("testTwoDepth") {
    //Bundle used as fifo payload
    case class Transaction() extends Bundle {
      val flag = Bool
      val data = Bits(8 bits)
      val color = Rgb(5, 6, 5)
    }

    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifo(
        dataType = Transaction(),
        depth = 2
      )
    )

    //Run the simulation
    compiled.doSim { dut =>
      //Inits
      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.forkSimSpeedPrinter()
      dut.io.flush #= false

      val scoreboard = ScoreboardInOrder[SimData]()

      //Drivers
      StreamDriver(dut.io.push, dut.clockDomain) { payload =>
        payload.randomize()
        true
      }
      StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

      //Monitors
      StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
        scoreboard.pushRef(payload)
      }
      StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
        scoreboard.pushDut(payload)
      }

      waitUntil(scoreboard.matches == 10000)
    }
  }


  test("lowLatency_0") {
    //Bundle used as fifo payload
    case class Transaction() extends Bundle {
      val flag = Bool
      val data = Bits(8 bits)
      val color = Rgb(5, 6, 5)
    }

    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifoLowLatency(
        dataType = Transaction(),
        depth = 2,
        latency = 0
      )
    )

    //Run the simulation
    compiled.doSim { dut =>
      //Inits
      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.forkSimSpeedPrinter()
      dut.io.flush #= false

      val scoreboard = ScoreboardInOrder[SimData]()

      //Drivers
      StreamDriver(dut.io.push, dut.clockDomain) { payload => payload.randomize(); true }
      StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

      //Monitors
      StreamMonitor(dut.io.push, dut.clockDomain) { payload => scoreboard.pushRef(payload) }
      StreamMonitor(dut.io.pop, dut.clockDomain) { payload => scoreboard.pushDut(payload) }

      waitUntil(scoreboard.matches == 10000)
    }
  }


  test("lowLatency_1") {
    //Bundle used as fifo payload
    case class Transaction() extends Bundle {
      val flag = Bool
      val data = Bits(8 bits)
      val color = Rgb(5, 6, 5)
    }

    val compiled = SimConfig.allOptimisation.compile(
      rtl = new StreamFifoLowLatency(
        dataType = Transaction(),
        depth = 4,
        latency = 1
      )
    )

    //Run the simulation
    compiled.doSim { dut =>
      //Inits
      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.forkSimSpeedPrinter()
      dut.io.flush #= false

      val scoreboard = ScoreboardInOrder[SimData]()

      //Drivers
      StreamDriver(dut.io.push, dut.clockDomain) { payload => payload.randomize(); true }
      StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

      //Monitors
      StreamMonitor(dut.io.push, dut.clockDomain) { payload => scoreboard.pushRef(payload) }
      StreamMonitor(dut.io.pop, dut.clockDomain) { payload => scoreboard.pushDut(payload) }

      waitUntil(scoreboard.matches == 10000)
    }
  }
}
