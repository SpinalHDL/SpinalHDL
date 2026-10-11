package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.tester.SpinalSimFunSuite

class SpinalSimStreamFragmentArbiterTester extends SpinalSimFunSuite {
  test("arbiter") {
    SimConfig
      .compile {
        new Component {
          val inputs = Vec(slave(Stream(Fragment(UInt(10 bits)))), 2)
          val output = master(Stream(Fragment(UInt(10 bits))))

          output << StreamFragmentArbiter(UInt(10 bits))(inputs.toList)
        }
      }
      .doSimUntilVoid { dut =>
        SimTimeout(100000)
        dut.clockDomain.forkStimulus(10)

        val scoreboards = Array.fill(2)(ScoreboardInOrder[(Int, Boolean)]())
        dut.inputs.indices.foreach { source =>
          val driver = StreamDriver(dut.inputs(source), dut.clockDomain) { p =>
            val fragment = (source << 8) | simRandom.nextInt(256)
            val last = simRandom.nextInt(5) == 0
            p.fragment #= fragment
            p.last #= last
            scoreboards(source).pushRef((fragment, last))
            true
          }
          driver.setFactor(if (source == 0) 0.5f else 1.0f)
        }

        val completedPackets = Array.fill(2)(0)
        var activeSource = -1
        StreamReadyRandomizer(dut.output, dut.clockDomain)
        StreamMonitor(dut.output, dut.clockDomain) { p =>
          val fragment = p.fragment.toInt
          val last = p.last.toBoolean
          val source = fragment >> 8
          if (activeSource == -1) activeSource = source
          assert(source == activeSource, s"Input $source interrupted a packet from input $activeSource")
          scoreboards(source).pushDut((fragment, last))
          if (last) {
            completedPackets(source) += 1
            activeSource = -1
          }
        }
        dut.clockDomain.onSamplings {
          if (completedPackets.forall(_ >= 50)) simSuccess()
        }
      }
  }

  test("arbiterAndHeaderAdder") {
    SimConfig
      .compile {
        new Component {
          val inputs = Vec(slave(Stream(Fragment(UInt(10 bits)))), 2)
          val output = master(Stream(Fragment(UInt(10 bits))))

          output << StreamFragmentArbiterAndHeaderAdder(UInt(10 bits))(
            Seq(inputs(0) -> U(0x200, 10 bits), inputs(1) -> U(0x201, 10 bits))
          )
        }
      }
      .doSimUntilVoid { dut =>
        SimTimeout(100000)
        dut.clockDomain.forkStimulus(10)

        val scoreboards = Array.fill(2)(ScoreboardInOrder[(Int, Boolean)]())
        dut.inputs.indices.foreach { source =>
          val driver = StreamDriver(dut.inputs(source), dut.clockDomain) { p =>
            val fragment = (source << 8) | simRandom.nextInt(256)
            val last = simRandom.nextInt(5) == 0
            p.fragment #= fragment
            p.last #= last
            scoreboards(source).pushRef((fragment, last))
            true
          }
          driver.setFactor(if (source == 0) 0.5f else 1.0f)
        }

        val completedPackets = Array.fill(2)(0)
        var activeSource = -1
        StreamReadyRandomizer(dut.output, dut.clockDomain)
        StreamMonitor(dut.output, dut.clockDomain) { p =>
          val fragment = p.fragment.toInt
          val last = p.last.toBoolean
          if (activeSource == -1) {
            assert((fragment & 0x3fe) == 0x200, f"Expected a header, got 0x$fragment%03x")
            assert(!last, "A header must not terminate a packet")
            activeSource = fragment & 1
          } else {
            val source = fragment >> 8
            assert(fragment < 0x200, f"Unexpected header 0x$fragment%03x inside a packet")
            assert(source == activeSource, s"Input $source interrupted a packet from input $activeSource")
            scoreboards(source).pushDut((fragment, last))
            if (last) {
              completedPackets(source) += 1
              activeSource = -1
            }
          }
        }
        dut.clockDomain.onSamplings {
          if (completedPackets.forall(_ >= 50)) simSuccess()
        }
      }
  }
}
