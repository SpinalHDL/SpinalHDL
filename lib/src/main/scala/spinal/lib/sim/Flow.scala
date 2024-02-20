package spinal.lib.sim

import spinal.core.{ClockDomain, Data}
import spinal.core.sim._
import spinal.lib.Flow

import scala.collection.mutable.{Queue, ArrayBuffer}

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

object FlowDriver {
  def apply[T <: Data](flow: Flow[T], clockDomain: ClockDomain)(driver: (T) => Boolean) =
    new FlowDriver(flow, clockDomain, driver)

  def queue[T <: Data](flow: Flow[T], clockDomain: ClockDomain) = {
    val cmdQueue = Queue[(T) => Unit]()
    val driver = FlowDriver(flow, clockDomain) { p =>
      if (cmdQueue.isEmpty) false
      else {
        cmdQueue.dequeue().apply(p)
        true
      }
    }
    (driver, cmdQueue)
  }
}

class FlowDriver[T <: Data](flow: Flow[T], clockDomain: ClockDomain, var driver: (T) => Boolean) {
  var transactionDelay: () => Int = () => {
    val x = simRandom.nextDouble()
    (x * x * 10).toInt
  }

  var factor = Option.empty[Float]
  def setFactor(value: Float) = factor = Some(value)
  def setFactorPeriodically(period: Long) = periodicaly(period)(setFactor(simRandom.nextFloat()))


  var state = 0
  var delay = transactionDelay()
  flow.valid #= false
  flow.payload.randomize()

  def fsm(): Unit = {
    state match {
      case 0 => {
        factor match {
          case Some(x) => if (simRandom.nextFloat() < x) {
            state += 1
            fsm()
          }
          case None =>
            if (delay == 0) {
              state += 1
              fsm()
            } else {
              delay -= 1
            }
        }
      }
      case 1 => {
        if (driver(flow.payload)) {
          flow.valid #= true
          state += 1
        }
      }
      case 2 => {
        flow.valid #= false
        flow.payload.randomize()
        if (factor.isEmpty) {delay = transactionDelay()}
        state = 0
        fsm()
      }
    }
  }
  clockDomain.onSamplings(fsm)

  def reset() {
    state = 0
    flow.valid #= false
  }
}
