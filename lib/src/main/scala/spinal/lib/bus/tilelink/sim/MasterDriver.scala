package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.lib.sim._
import spinal.lib.bus.tilelink._

class MasterDriver (val bus : Bus, cd : ClockDomain) {
  val driver = new Area {
    val a = StreamDriverOoo(bus.a, cd)
    val b = bus.p.withBCE generate StreamReadyRandomizer(bus.b, cd)
    val c = bus.p.withBCE generate StreamDriverOoo(bus.c, cd)
    val d = StreamReadyRandomizer(bus.d, cd)
    val e = bus.p.withBCE generate StreamDriverOoo(bus.e, cd)
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