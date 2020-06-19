package spinal.lib.bus.amba3.ahblite.sim

import spinal.core._
import spinal.lib.bus.amba3.ahblite._
import spinal.core.sim._
import scala.collection.mutable._

object AhbLite3Monitor{
  def apply(bus : AhbLite3, clockdomain: ClockDomain)(callback: (AhbLite3) => Unit) = new AhbLite3Monitor(bus, clockdomain).addCallback(callback)
}

/** This is a helping class for executing code when an acknoledge happend on the bus
  * @param bus the wishbone bus to drive
  * @param clockdomain the clockdomain where the bus reside
  */
class AhbLite3Monitor(bus: AhbLite3, clockdomain: ClockDomain){

  val callbacks = ArrayBuffer[(AhbLite3) => Unit]()

  /** Add a callback, this will be executed on every bus acknoledge
    * @param callback a function or code block that takes a wishbone bus as parameter
    */
  def addCallback(callback: (AhbLite3) => Unit): Unit = callbacks += callback

  fork{
    while(true){
      clockdomain.waitActiveEdgeWhere(bus.HSEL.toBoolean & bus.HTRANS.toInt == 2)
      callbacks.foreach{ _(bus) }
    }
  }
}