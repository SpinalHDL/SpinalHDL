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
  val requestAckCallbacks = ArrayBuffer[(Wishbone) => Unit]()
  val responseCallbacks = ArrayBuffer[(Wishbone, WishboneTransaction, Boolean) => Unit]()
  val requests = new Queue[(WishboneTransaction, Boolean)]()
  /** Add a callback, this will be executed on every bus acknoledge
    * @param callback a function or code block that takes a wishbone bus as parameter
    */
  def addCallback(callback: (Wishbone) => Unit): Unit = addReqAckCallback(callback)
  def addReqAckCallback(callback: (Wishbone) => Unit): Unit = requestAckCallbacks += callback
  def addResponseCallback(callback: (Wishbone, WishboneTransaction, Boolean) => Unit): Unit = responseCallbacks += callback

  fork{
    while(true){
      clockdomain.waitSamplingWhere(busStatus.isAck || busStatus.isRequestAck)
      if(busStatus.isRequestAck) {
        requestAckCallbacks.foreach{_(bus)}
        requests.enqueue((WishboneTransaction.sampleAsMaster(bus), bus.WE.toBoolean))
      }
      if(busStatus.isAck) {
        assert(requests.nonEmpty, s"${bus} has an ACK with no requests")
        val request = requests.dequeue()
        responseCallbacks.foreach(_(bus, request._1, request._2))
      }
    }
  }
}