package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.mutable._

object WishboneMonitor{
  def apply(bus : Wishbone, clockdomain: ClockDomain)(callback: (Wishbone) => Unit) = new WishboneMonitor(bus,clockdomain).addCallback(callback)
}

/** This is a helping class for executing code when an acknoledge happend on the bus
  * @param bus the wishbone bus to drive
  * @param clockdomain the clockdomain where the bus reside
  */
class WishboneMonitor(bus: Wishbone, clockdomain: ClockDomain){
  val busStatus = WishboneStatus(bus)
  val callbacks = ArrayBuffer[(Wishbone) => Unit]()

  /** Add a callback, this will be executed on every bus acknoledge
    * @param callback a function or code block that takes a wishbone bus as parameter
    */
  def addCallback(callback: (Wishbone) => Unit): Unit = callbacks += callback

  fork{
    while(true){
      clockdomain.waitSamplingWhere(busStatus.isAck)
      callbacks.foreach{_(bus)}
    }
  }
}