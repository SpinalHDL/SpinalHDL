package spinal.core

import spinal.sim._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

import scala.concurrent.{Await, Future}
import scala.util.Random

object SpinalSimPerfTester {

  class SpinalSimPerfTesterDut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }

    io.result := RegNext(io.a + io.b - io.c) init (0)
  }

}

class SpinalSimPerfTester extends SpinalAnyFunSuite {
  SpinalSimTester { env =>
    import env._

    var compiled: SimCompiled[SpinalSimPerfTester.SpinalSimPerfTesterDut] = null

    test(prefix + "compile") {
      compiled = SimConfig
        .allOptimisation
        //      .withGhdl
        .compile(new SpinalSimPerfTester.SpinalSimPerfTesterDut())
    }

    test(prefix + "TestReadSpeedRaw") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.a #= 1
        dut.io.b #= 1
        dut.io.c #= 1
        sleep(10)

        for (repeat <- 0 until 10) {
          var accesses = 0
          var acc = 0l
          val bt = dut.io.a
          val startAt = System.nanoTime
          while(accesses != 1000000){
            acc += bt.toLong
            accesses += 1
          }
          val endAt = System.nanoTime
          val nano = (endAt - startAt) / accesses
          System.out.println(nano + " ns per call acc=" + acc)
        }
      }
    }

    test(prefix + "TestReadSpeedImplicit") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.a #= 1
        dut.io.b #= 1
        dut.io.c #= 1
        sleep(10)

        implicit val manager: SimManager = SimManagerContext.current.manager
        for (repeat <- 0 until 10) {
          var accesses = 0
          var acc = 0l
          val bt = dut.io.a
          val startAt = System.nanoTime
          while(accesses != 1000000){
            acc += bt.toLong
            accesses += 1
          }
          val endAt = System.nanoTime
          val nano = (endAt - startAt) / accesses
          System.out.println(nano + " ns per call acc=" + acc)
        }
      }
    }

    test(prefix + "TestReadSpeedProxy") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.a #= 1
        dut.io.b #= 1
        dut.io.c #= 1
        sleep(10)

        for (repeat <- 0 until 10) {
          var accesses = 0
          var acc = 0l
          val bt = dut.io.a.simProxy()
          val startAt = System.nanoTime
          while(accesses != 1000000){
            acc += bt.toLong
            accesses += 1
          }
          val endAt = System.nanoTime
          val nano = (endAt - startAt) / accesses
          System.out.println(nano + " ns per call acc=" + acc)
        }
      }
    }


    test(prefix + "TestStdSimIntThreadLess") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.forkSimSpeedPrinter(0.2)
        dut.io.a.randomize()
        dut.io.b.randomize()
        dut.io.c.randomize()

        var model = -1
        var times = 0
        dut.clockDomain.onSamplings {
          assert(model == -1 || dut.io.result.toInt == model)
          model = ((dut.io.a.toInt + dut.io.b.toInt - dut.io.c.toInt) & 0xFF)
          dut.io.a #= Random.nextInt(256)
          dut.io.b #= Random.nextInt(256)
          dut.io.c #= Random.nextInt(256)
          times += 1
        }

        for (repeat <- 0 until 4) {
          val startAt = System.nanoTime
          waitUntil(times == (2000000*durationFactor).toInt)
          times = 0
          val endAt = System.nanoTime
          System.out.println((endAt - startAt) * 1e-6 + " ms")
        }
      }
    }


    test(prefix + "TestStdSimInt") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.forkSimSpeedPrinter(0.2)

        var model = 0
        for (repeat <- 0 until 4) {
          val times = (2000000*durationFactor).toInt
          val startAt = System.nanoTime
          for (repeat2 <- 0 until times) {
            dut.io.a #= Random.nextInt(256)
            dut.io.b #= Random.nextInt(256)
            dut.io.c #= Random.nextInt(256)
            dut.clockDomain.waitActiveEdge()
            if (dut.clockDomain.isResetDeasserted) {
              assert(dut.io.result.toInt == model)
              model = ((dut.io.a.toInt + dut.io.b.toInt - dut.io.c.toInt) & 0xFF)
            }
          }
          val endAt = System.nanoTime
          System.out.println((endAt - startAt) * 1e-6 + " ms")
        }
      }
    }


    test(prefix + "TestStdSimIntx2") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for (repeat <- 0 until 4) {
          val times = (80000*durationFactor).toInt
          val startAt = System.nanoTime
          val t1, t2 = fork {
            val rand = new Random(1)
            for (repeat2 <- 0 until times) {
              val a, b, c = rand.nextInt(256)
              dut.io.a #= a
              dut.io.b #= b
              dut.io.c #= c
              dut.clockDomain.waitActiveEdge()
              sleep(0)
              val dummy = if (dut.clockDomain.isResetDeasserted)
                assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
            }
          }
          t1.join(); t2.join()
          val endAt = System.nanoTime
          System.out.println((endAt - startAt) * 1e-6 + " ms")
        }
      }
    }


    test(prefix + "TestStdSimBigInt") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for (repeat <- 0 until 4) {
          val times = (80000*durationFactor).toInt
          val startAt = System.nanoTime
          for (repeat2 <- 0 until times) {
            val a, b, c = BigInt(Random.nextInt(256))
            dut.io.a #= a
            dut.io.b #= b
            dut.io.c #= c
            dut.clockDomain.waitActiveEdge(); sleep(0)
            if (dut.clockDomain.isResetDeasserted) assert(dut.io.result.toBigInt == ((a + b - c) & 0xFF))
          }
          val endAt = System.nanoTime
          System.out.println((endAt - startAt) * 1e-6 + " ms")
        }
      }
    }

    test(prefix + "TestSleep0") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for (repeat <- 0 until 4) {
          val times = (100000*durationFactor).toInt
          val startAt = System.nanoTime
          for (repeat2 <- 0 until times) {
            sleep(0)
          }
          val endAt = System.nanoTime
          System.out.println((endAt - startAt) * 1e-6 + " ms")
        }
      }

    }


    test(prefix + "compilationSpeed") {
      val stages = 100
      val states = (100*designFactor).toInt
      val operands = 5
      SimConfig.withConfig(SpinalConfig(verbose = true)).allOptimisation.doSim(new Component {
        val inputs = Vec(in UInt (8 bits), states)
        val outputs = Vec(out UInt (8 bits), states)
        var ptr = inputs
        for (s <- 0 until stages) {
          val result = Vec(Reg(UInt(8 bits)), states).setName("tmp_" + s)
          for (elementId <- 0 until states) {
            result(elementId) := (0 until operands).map(_ => ptr(Random.nextInt(states))).reduce(_ + _)
          }
          ptr = result
        }
        outputs := ptr
      }) { dut =>

        dut.clockDomain.forkStimulus(10)
        for (r <- 0 until (10000*durationFactor).toInt) {
          for (input <- dut.inputs) {
            input.randomize()
          }
          dut.clockDomain.waitSampling()
        }
      }
    }
  }
}
