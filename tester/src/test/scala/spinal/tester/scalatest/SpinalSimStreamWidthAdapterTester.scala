package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.StreamWidthAdapter
import spinal.lib.sim._
import spinal.tester

import scala.collection.mutable
import scala.util.Random

class SpinalSimStreamWidthAdapterTester extends SpinalSimFunSuite {
  test("test2xOut") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2 bits))
      val rtl = StreamWidthAdapter(input, output)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        val value = (high << baseWidth) + low
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xIn") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.withVcdWave.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2 bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        var high: BigInt = 1
        var changed = false
        if(queueModel.nonEmpty){
          high = queueModel.dequeue()
        }
        else{
          changed = true
          assert(dut.output.valid.toBoolean)
          high = dut.output.payload.toBigInt
        }
        val value = (high << baseWidth) + low
        assert(p.toBigInt == value)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        queueModel.enqueue(p.toBigInt)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }
  // test("testOne") {
  //   //Compile the simulator
  //   val compiled = SimConfig.allOptimisation.compile(
  //     rtl = StreamWidthAdapter(
  //       dataType = Bits(32 bits),
  //       depth = 1
  //     )
  //   )

  //   //Run the simulation
  //   compiled.doSimUntilVoid { dut =>
  //     val queueModel = mutable.Queue[Long]()

  //     SimTimeout(1000000 * 8)
  //     dut.clockDomain.forkStimulus(2)

  //     dut.io.flush #= false

  //     //Push data randomly and fill the queueModel with pushed transactions
  //     dut.io.push.valid #= false
  //     dut.clockDomain.onSamplings {
  //       if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
  //         queueModel.enqueue(dut.io.push.payload.toLong)
  //       }
  //       dut.io.push.valid.randomize()
  //       dut.io.push.payload.randomize()
  //     }

  //     //Pop data randomly and check that it match with the queueModel
  //     val popThread = fork {
  //       dut.io.pop.ready #= true
  //       for (repeat <- 0 until 10000) {
  //         dut.io.pop.ready.randomize()
  //         dut.clockDomain.waitSampling()
  //         if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
  //           assert(dut.io.pop.payload.toLong == queueModel.dequeue())
  //         }
  //       }
  //       simSuccess()
  //     }
  //   }
  // }


  // test("testBundle") {
  //   //Bundle used as fifo payload
  //   case class Transaction() extends Bundle {
  //     val flag = Bool()
  //     val data = Bits(8 bits)
  //     val color = Rgb(5, 6, 5)

  //     override def clone = Transaction()
  //   }

  //   val compiled = SimConfig.allOptimisation.compile(
  //     rtl = StreamWidthAdapter(
  //       dataType = Transaction(),
  //       depth = 32
  //     )
  //   )

  //   //Run the simulation
  //   compiled.doSim { dut =>
  //     //Inits
  //     SimTimeout(1000000 * 8)
  //     dut.clockDomain.forkStimulus(2)
  //     dut.clockDomain.forkSimSpeedPrinter()
  //     dut.io.flush #= false

  //     val scoreboard = ScoreboardInOrder[SimData]()

  //     //Drivers
  //     StreamDriver(dut.io.push, dut.clockDomain) { payload =>
  //       payload.randomize()
  //       true
  //     }
  //     StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

  //     //Monitors
  //     StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
  //       scoreboard.pushRef(payload)
  //     }
  //     StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
  //       scoreboard.pushDut(payload)
  //     }

  //     waitUntil(scoreboard.matches == 10000)
  //   }
  // }

  // test("testTwoDepth") {
  //   //Bundle used as fifo payload
  //   case class Transaction() extends Bundle {
  //     val flag = Bool()
  //     val data = Bits(8 bits)
  //     val color = Rgb(5, 6, 5)
  //   }

  //   val compiled = SimConfig.allOptimisation.compile(
  //     rtl = StreamWidthAdapter(
  //       dataType = Transaction(),
  //       depth = 2
  //     )
  //   )

  //   //Run the simulation
  //   compiled.doSim { dut =>
  //     //Inits
  //     SimTimeout(1000000 * 8)
  //     dut.clockDomain.forkStimulus(2)
  //     dut.clockDomain.forkSimSpeedPrinter()
  //     dut.io.flush #= false

  //     val scoreboard = ScoreboardInOrder[SimData]()

  //     //Drivers
  //     StreamDriver(dut.io.push, dut.clockDomain) { payload =>
  //       payload.randomize()
  //       true
  //     }
  //     StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

  //     //Monitors
  //     StreamMonitor(dut.io.push, dut.clockDomain) { payload =>
  //       scoreboard.pushRef(payload)
  //     }
  //     StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
  //       scoreboard.pushDut(payload)
  //     }

  //     waitUntil(scoreboard.matches == 10000)
  //   }
  // }


  // test("lowLatency_0") {
  //   //Bundle used as fifo payload
  //   case class Transaction() extends Bundle {
  //     val flag = Bool()
  //     val data = Bits(8 bits)
  //     val color = Rgb(5, 6, 5)
  //   }

  //   val compiled = SimConfig.allOptimisation.compile(
  //     rtl = StreamFifoLowLatency(
  //       dataType = Transaction(),
  //       depth = 2,
  //       latency = 0
  //     )
  //   )

  //   //Run the simulation
  //   compiled.doSim { dut =>
  //     //Inits
  //     SimTimeout(1000000 * 8)
  //     dut.clockDomain.forkStimulus(2)
  //     dut.clockDomain.forkSimSpeedPrinter()
  //     dut.io.flush #= false

  //     val scoreboard = ScoreboardInOrder[SimData]()

  //     //Drivers
  //     StreamDriver(dut.io.push, dut.clockDomain) { payload => payload.randomize(); true }
  //     StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

  //     //Monitors
  //     StreamMonitor(dut.io.push, dut.clockDomain) { payload => scoreboard.pushRef(payload) }
  //     StreamMonitor(dut.io.pop, dut.clockDomain) { payload => scoreboard.pushDut(payload) }

  //     waitUntil(scoreboard.matches == 10000)
  //   }
  // }


  // test("lowLatency_1") {
  //   //Bundle used as fifo payload
  //   case class Transaction() extends Bundle {
  //     val flag = Bool()
  //     val data = Bits(8 bits)
  //     val color = Rgb(5, 6, 5)
  //   }

  //   val compiled = SimConfig.allOptimisation.compile(
  //     rtl = StreamFifoLowLatency(
  //       dataType = Transaction(),
  //       depth = 4,
  //       latency = 1
  //     )
  //   )

  //   //Run the simulation
  //   compiled.doSim { dut =>
  //     //Inits
  //     SimTimeout(1000000 * 8)
  //     dut.clockDomain.forkStimulus(2)
  //     dut.clockDomain.forkSimSpeedPrinter()
  //     dut.io.flush #= false

  //     val scoreboard = ScoreboardInOrder[SimData]()

  //     //Drivers
  //     StreamDriver(dut.io.push, dut.clockDomain) { payload => payload.randomize(); true }
  //     StreamReadyRandomizer(dut.io.pop, dut.clockDomain)

  //     //Monitors
  //     StreamMonitor(dut.io.push, dut.clockDomain) { payload => scoreboard.pushRef(payload) }
  //     StreamMonitor(dut.io.pop, dut.clockDomain) { payload => scoreboard.pushDut(payload) }

  //     waitUntil(scoreboard.matches == 10000)
  //   }
  // }
}
