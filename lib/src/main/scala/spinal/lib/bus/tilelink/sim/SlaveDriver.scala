package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}


class SlaveDriver(bus : Bus, cd : ClockDomain) {
  val driver = new Area{
    val a = StreamReadyRandomizer(bus.a, cd)
    val b = bus.p.withBCE generate StreamDriverOoo(bus.b, cd)
    val c = bus.p.withBCE generate StreamReadyRandomizer(bus.c, cd)
    val d = StreamDriverOoo(bus.d, cd)
    val e = bus.p.withBCE generate StreamReadyRandomizer(bus.e, cd)

    def noStall(): Unit = {
      a.factor = 1.0f
      if (b != null) b.ctrl.transactionDelay = () => 0
      if (c != null) c.factor = 1.0f
      d.ctrl.transactionDelay = () => 0
      if (e != null) e.factor = 1.0f
    }

    def randomizeStallRate(): Unit = {
      a.setFactor(simRandom.nextFloat())
      if (b != null) b.ctrl.setFactor(simRandom.nextFloat())
      if (c != null) c.setFactor(simRandom.nextFloat())
      d.ctrl.setFactor(simRandom.nextFloat())
      if (e != null) e.setFactor(simRandom.nextFloat())
    }
    
    def setFactor(factor : Float): Unit = {
      a.setFactor(factor)
      if (b != null) b.ctrl.setFactor(factor)
      if (c != null) c.setFactor(factor)
      d.ctrl.setFactor(factor)
      if (e != null) e.setFactor(factor)
    }
  }

  def scheduleD(d : TransactionD): Unit ={
    val beats = d.serialize(bus.p.dataBytes)
    driver.d.burst{push =>
      for(beat <- beats) push(beat.write)
    }
  }

  def scheduleB(b : TransactionB): Unit ={
    val beats = b.serialize(bus.p.dataBytes)
    driver.b.burst{push =>
      for(beat <- beats) push(beat.write)
    }
  }
}
