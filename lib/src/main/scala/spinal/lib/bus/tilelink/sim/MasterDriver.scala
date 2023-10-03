package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.lib.sim._
import spinal.lib.bus.tilelink._
import spinal.core.sim._

class MasterDriver (val bus : Bus, cd : ClockDomain) {
  val driver = new Area {
    val a = StreamDriverOoo(bus.a, cd)
    val b = bus.p.withBCE generate StreamReadyRandomizer(bus.b, cd)
    val c = bus.p.withBCE generate StreamDriverOoo(bus.c, cd)
    val d = StreamReadyRandomizer(bus.d, cd)
    val e = bus.p.withBCE generate StreamDriverOoo(bus.e, cd)

    def noStall(): Unit = {
      a.ctrl.transactionDelay = () => 0
      if(b != null) b.factor = 1.0f
      if(c != null) c.ctrl.transactionDelay = () => 0
      d.factor = 1.0f
      if(e != null) e.ctrl.transactionDelay = () => 0
    }

    def randomizeStallRate() : Unit = {
      a.ctrl.setFactor(simRandom.nextFloat())
      if (b != null) b.setFactor(simRandom.nextFloat())
      if (c != null) c.ctrl.setFactor(simRandom.nextFloat())
      d.setFactor(simRandom.nextFloat())
      if (e != null) e.ctrl.setFactor(simRandom.nextFloat())
    }
  }

  def scheduleA(a : TransactionA): Unit ={
    val beats = a.serialize(bus.p.dataBytes)
    driver.a.burst{push =>
      for(beat <- beats) push(beat.write)
    }
  }

  def scheduleC(c : TransactionC): Unit ={
    val beats = c.serialize(bus.p.dataBytes)
    driver.c.burst{push =>
      for(beat <- beats) push(beat.write)
    }
  }

  def scheduleE(e : TransactionE): Unit ={
    driver.e.single(p => e.write(p))
  }
}