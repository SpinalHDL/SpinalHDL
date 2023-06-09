package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.util.Random

class SlaveDriver(bus : Bus, cd : ClockDomain) {
  val driver = new Area{
    val a = StreamReadyRandomizer(bus.a, cd)
    val b = bus.p.withBCE generate StreamDriverOoo(bus.b, cd)
    val c = bus.p.withBCE generate StreamReadyRandomizer(bus.c, cd)
    val d = StreamDriverOoo(bus.d, cd)
    val e = bus.p.withBCE generate StreamReadyRandomizer(bus.e, cd)
  }

  def scheduleD(d : TransactionD): Unit ={
    val beats = d.serialize(bus.p.dataBytes)
    driver.d.burst{push =>
      for(beat <- beats) push(beat.write)
    }
  }
}
