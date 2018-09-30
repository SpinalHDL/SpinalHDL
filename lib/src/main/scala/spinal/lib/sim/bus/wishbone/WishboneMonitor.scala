package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.mutable._

object WishboneMonitor{
  def apply(bus : Wishbone, clockdomain: ClockDomain)(callback: (Wishbone) => Unit) = new WishboneMonitor(bus,clockdomain).addCallback(callback)
}

class WishboneMonitor(bus: Wishbone, clockdomain: ClockDomain){
  val busStatus = WishboneStatus(bus)
  val callbacks = ArrayBuffer[(Wishbone) => Unit]()

  def addCallback(callback: (Wishbone) => Unit): Unit = callbacks += callback

  fork{
    while(true){
      clockdomain.waitSamplingWhere(busStatus.isAck)
      callbacks.suspendable.foreach{_(bus)}
    }
  }
}