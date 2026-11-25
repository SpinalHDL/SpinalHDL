package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

class StreamArbiterLockPolicyTester extends SpinalAnyFunSuite {
  // Two arbiters; arbiter k ends a packet on a beat with payload bit k set. Both factories are
  // configured before either arbiter is elaborated.
  case class Dut(factory: Int => StreamArbiterFactory) extends Component {
    val io = new Bundle {
      val inputs = Vec.fill(2)(Vec.fill(2)(slave(Stream(UInt(8 bits)))))
      val outputs = Vec.fill(2)(master(Stream(UInt(8 bits))))
    }
    val factories = Seq(factory(0), factory(1))
    for (k <- 0 until 2) io.outputs(k) << factories(k).on(io.inputs(k))
  }

  // Payload bit 7 is the input port, so each output beat names its source. Each port's last beat
  // ends a packet for both arbiters, else the lock would starve the other port.
  def checkPackets(factory: Int => StreamArbiterFactory): Unit =
    SimConfig.compile(Dut(factory)).doSim(seed = 42) { dut =>
      val rand = new Random(42)
      val beatsPerPort = 500
      val inputs = Seq.fill(2)(Seq.tabulate(2) { port =>
        Seq.fill(beatsPerPort - 1)(rand.nextInt(128) | port << 7) :+ (3 | port << 7)
      })
      val outputs = Seq.fill(2)(mutable.ArrayBuffer[Int]())

      SimTimeout(100000)
      dut.clockDomain.forkStimulus(10)
      for (k <- 0 until 2) {
        for ((input, data) <- dut.io.inputs(k).zip(inputs(k))) {
          val it = data.iterator
          StreamDriver(input, dut.clockDomain)(p => it.hasNext && { p #= it.next(); true })
        }
        StreamReadyRandomizer(dut.io.outputs(k), dut.clockDomain)
        StreamMonitor(dut.io.outputs(k), dut.clockDomain)(outputs(k) += _.toInt)
      }
      dut.clockDomain.waitSamplingWhere(outputs.forall(_.size == 2 * beatsPerPort))

      for (k <- 0 until 2) {
        for (port <- 0 until 2) assert(outputs(k).filter(_ >> 7 == port) == inputs(k)(port))
        for ((beat, next) <- outputs(k).zip(outputs(k).tail) if beat >> 7 != next >> 7) {
          assert((beat >> k & 1) == 1, f"arbiter $k switched port mid-packet after 0x$beat%02x")
        }
      }
    }

  test("lambdaLock is per factory") {
    checkPackets(k => StreamArbiterFactory().roundRobin.lambdaLock[UInt](_.payload(k)))
  }

  test("setLock is per factory") {
    checkPackets(k => StreamArbiterFactory().roundRobin.setLock { core =>
      new Area {
        import core._
        locked.setWhen(io.output.valid)
        locked.clearWhen(io.output.fire && io.output.payload.asBits(k))
      }
    })
  }
}
