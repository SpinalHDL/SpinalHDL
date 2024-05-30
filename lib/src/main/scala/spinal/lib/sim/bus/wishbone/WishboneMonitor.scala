package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.mutable._

trait WishboneMonitorCallback {
  /**
   * Received when a wishbone bus has an acknowledged request
   * @param bus the bus in question
   * @param requestTransaction The request transaction.
   * @param we Whether or not this is a write transaction
   */
  def onRequestAck(bus : Wishbone, requestTransaction: WishboneTransaction, we: Boolean): Unit = {}

  /**
   * Received when a wishbone bus has a request response
   * @param bus the bus in question
   * @param fullTransaction A combination of the request transaction address with the response data in the case of a read
   *                        transaction.
   * @param we Whether or not this was a write transaction
   */
  def onResponse(bus : Wishbone, fullTransaction: WishboneTransaction, we: Boolean): Unit = {}
}

object WishboneMonitor{
  def apply(bus : Wishbone, clockdomain: ClockDomain, callback: WishboneMonitorCallback): WishboneMonitor =
    new WishboneMonitor(bus, clockdomain).addMonitorCallback(callback)

  def apply(bus : Wishbone, clockdomain: ClockDomain, callback: (Wishbone, WishboneTransaction, Boolean) => Unit) =
    new WishboneMonitor(bus, clockdomain).addResponseCallback(callback)

  @deprecated("This form is not super useful for pipelined transactions, prefer passing in WishboneMonitorCallback or just the onResponse callback arguments")
  def apply(bus : Wishbone, clockdomain: ClockDomain)(callback: (Wishbone) => Unit) =
    new WishboneMonitor(bus, clockdomain).addCallback(callback)

}

/** This is a helping class for executing code when an acknoledge happend on the bus
  * @param bus the wishbone bus in question
  * @param clockdomain the clockdomain where the bus reside
  */
class WishboneMonitor(bus: Wishbone, clockdomain: ClockDomain, useByteAddress : Boolean = false){
  val busStatus = WishboneStatus(bus)
  val callbacks = ArrayBuffer[WishboneMonitorCallback]()

  val requests = new Queue[(WishboneTransaction, Boolean)]()

  /** Add a callback, this will be executed on every bus acknowledge
   * @param callback a function or code block that takes a wishbone bus as parameter
   */
  @deprecated("This form is not super useful for pipelined transactions, prefer passing in WishboneMonitorCallback or just the onResponse callback arguments")
  def addCallback(callback : (Wishbone) => Unit) = {
    addMonitorCallback(new WishboneMonitorCallback {
      override def onResponse(bus: Wishbone, fullTransaction: WishboneTransaction, we: Boolean): Unit = callback(bus)
    })
  }

  def addResponseCallback(callback : (Wishbone, WishboneTransaction, Boolean) => Unit) = {
    addMonitorCallback(new WishboneMonitorCallback {
      override def onResponse(bus: Wishbone, fullTransaction: WishboneTransaction, we: Boolean): Unit = callback(bus, fullTransaction, we)
    })
  }

  def addMonitorCallback(callback: WishboneMonitorCallback): WishboneMonitor = {
    callbacks += callback
    this
  }

  fork{
    while(true){
      clockdomain.waitSamplingWhere(busStatus.isResponse || busStatus.isRequestAck)
      if(busStatus.isRequestAck) {
        val transaction = WishboneTransaction.sampleAsMaster(bus, useByteAddress)
        callbacks.foreach{_.onRequestAck(bus, transaction, bus.WE.toBoolean)}
        requests.enqueue((transaction, bus.WE.toBoolean))
      }
      if(busStatus.isResponse) {
        assert(requests.nonEmpty, s"${bus} has an ACK with no requests")
        val (requestTx, write) = requests.dequeue()
        val cbTx = write match {
          case false => requestTx.copy (data = bus.DAT_MISO.toBigInt)
          case true => requestTx
        }

        callbacks.foreach{_.onResponse(bus, cbTx, write) }
      }
    }
  }
}