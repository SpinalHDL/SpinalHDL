package spinal.lib.bus.avalon

import sim.{AvalonSTDriver, AvalonSTMonitor}

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{ScoreboardInOrder, SimData}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable

case class AvalonSTPipelineFixture(config: AvalonSTConfig, m2s: Boolean, s2m: Boolean) extends Component {

  val inStream = AvalonST(config)
  val outStream = inStream.pipelined(m2s, s2m)

  val io = new Bundle {
    val s = slave(inStream)
    val m = master(outStream)
  }

  clockDomain.readClockWire.dontSimplifyIt()
  clockDomain.readResetWire.dontSimplifyIt()
}

case class AvalonSTDelayAdapterFixture(config: AvalonSTConfig,
                                       lat_m: Int, allow_m: Int,
                                       lat_s: Int, allow_s: Int) extends Component {

  val io = new Bundle {
    val s = slave(AvalonST(config.copy(readyLatency = lat_s, readyAllowance = allow_s)))
    val m = master(AvalonST(config.copy(readyLatency = lat_m, readyAllowance = allow_m)))
  }

  AvalonSTDelayAdapter(io.s, io.m)
}

class AvalonSTTester extends SpinalAnyFunSuite {

  for(s2m <- List(false, true);
      m2s <- List(false, true);
      lat <- List(0, 2, 8);
      allow <- List(0, 2, 8)) {
//    if (s2m || m2s) {
      val text1 = if (s2m) "_s2m" else ""
      val text2 = if (m2s) "_m2s" else ""
      test(s"pipeline${text1}${text2}_lat${lat}_allow${allow}") {
        val config = AvalonSTConfig(4, readyLatency = lat, readyAllowance = allow)
        SimConfig.compile(AvalonSTPipelineFixture(config, m2s = m2s, s2m = s2m)).doSim((dut) => {
          dut.clockDomain.forkStimulus(10)
          SimTimeout(1000000L)

          val queue = new mutable.Queue[SimData]()

          val driver = new AvalonSTDriver(dut.io.s, dut.clockDomain, p => {
            queue.enqueue(SimData.copy(p))
            true
          })

          var beats = 10000

          val monitor = new AvalonSTMonitor(dut.io.m, dut.clockDomain)
          monitor.addCallback(p => {
            assert(queue.dequeue().check(p))
            if (beats == 0)
              simSuccess()
            else
              beats -= 1
          })
        })
      }
//    }
  }

  for(lat_m <- List(0, 2, 8);
      allow_m <- List(0, 2, 8);
      lat_s <- List(0, 2, 8);
      allow_s <- List(0, 2, 8)) {
    test(s"delay_adapt_lat${lat_s}_allow${allow_s}_to_lat${lat_m}_allow${allow_m}") {
      val config = AvalonSTConfig(4)
      SimConfig.compile(AvalonSTDelayAdapterFixture(config,
        lat_s = lat_s, allow_s = allow_s,
        lat_m = lat_m, allow_m = allow_m)).doSim((dut) => {
        dut.clockDomain.forkStimulus(10)
        SimTimeout(1000000L)

        val queue = new mutable.Queue[SimData]()

        val driver = new AvalonSTDriver(dut.io.s, dut.clockDomain, p => {
          queue.enqueue(SimData.copy(p))
          true
        })

        var beats = 10000

        val monitor = new AvalonSTMonitor(dut.io.m, dut.clockDomain)
        monitor.addCallback(p => {
          assert(queue.dequeue().check(p))
          if (beats == 0)
            simSuccess()
          else
            beats -= 1
        })
      })
    }
  }
}
