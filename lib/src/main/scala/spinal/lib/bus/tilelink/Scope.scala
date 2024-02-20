package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class ScopeFiber extends Area{
  val up = fabric.Node.up()

  def add(pin : Bool, address : Int) = probes += Probe(pin, address)

  case class Probe(pin : Bool, address : Int)
  val probes = ArrayBuffer[Probe]()
  val lock = Lock()
  val fiber = Fiber build new Area{
    up.m2s.supported load bus.tilelink.SlaveFactory.getSupported(
      addressWidth = 12,
      dataWidth = 32,
      allowBurst = true,
      up.m2s.proposed
    )

    up.s2m.none()

    lock.await()
    val factory = new SlaveFactory(up.bus, false)
    for(probe <- probes; pin = probe.pin) new Composite(this, pin.getRtlPath("_")){
      val counter = Reg(UInt(32 bits)) init(0)
      when(pin.pull())(counter := counter + 1)
      factory.read(counter, probe.address)
    }
  }
}
