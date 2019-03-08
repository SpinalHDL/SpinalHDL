package spinal.lib.sim

import spinal.core.{ClockDomain, Data}
import spinal.core.sim._
import spinal.lib.Flow

import scala.collection.mutable.ArrayBuffer

object FlowMonitor{
  def apply[T <: Data](Flow : Flow[T], clockDomain: ClockDomain)(callback : (T) => Unit) = new FlowMonitor(Flow,clockDomain).addCallback(callback)
}

class FlowMonitor[T <: Data](Flow : Flow[T], clockDomain: ClockDomain){
  val callbacks = ArrayBuffer[(T) => Unit]()

  def addCallback(callback : (T) => Unit): this.type = {
    callbacks += callback
    this
  }

  clockDomain.onSamplings{
    if(Flow.valid.toBoolean) callbacks.foreach(_ (Flow.payload))
  }
}