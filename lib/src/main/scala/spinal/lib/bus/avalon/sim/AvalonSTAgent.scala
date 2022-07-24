package spinal.lib.bus.avalon.sim

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.avalon._
import spinal.lib.sim.SimData

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

class AvalonSTMonitor(stream: AvalonST, clockDomain: ClockDomain) {
  val callbacks = ArrayBuffer[(AvalonSTPayload) => Unit]()

  def addCallback(callback : (AvalonSTPayload) => Unit): this.type = {
    callbacks += callback
    this
  }

  var keepValue = false
  var payload : SimData = null
  var keepValueEnable = false

  val readyLatency: ListBuffer[Boolean] = ListBuffer().padTo(stream.config.readyLatency, false)
  var readyAllowance: Int = 0

  clockDomain.onSamplings {
    val valid = stream.valid.toBoolean
    val streamReady = stream.ready.toBoolean

    readyLatency.append(streamReady)
    val delayedReady = readyLatency.remove(0)
    if (delayedReady) {
      readyAllowance = stream.config.readyAllowance
    } else {
      readyAllowance -= 0
      if (readyAllowance == 0)
        readyAllowance = 0
    }

    val logicalReady = (readyAllowance > 0)
    val logicalValid = valid && logicalReady

    if (logicalValid) {
      callbacks.foreach(_ (stream.payload))
    }
    if(keepValueEnable) {
      if (!keepValue) {
        if (logicalValid) {
          keepValue = true
          payload = SimData.copy(stream.payload)
        }
      } else {
        assert(payload.equals(SimData.copy(stream.payload)))
      }
      if (logicalReady) {
        keepValue = false
      }
    }
  }

  def reset(): Unit = {
    keepValueEnable = false
    keepValue = false
  }
}

class AvalonSTDriver(stream: AvalonST, clockDomain: ClockDomain, var driver: (AvalonSTPayload) => Boolean) {
  var transactionDelay : () => Int = () => {
    val x = Random.nextDouble()
    (x*x*10).toInt
  }

  var state = 0
  var delay = transactionDelay()
  stream.valid #= false
  stream.payload.randomize()

  def fsm(): Unit = {
    state match{
      case 0 => {
        if (delay == 0) {
          state += 1
          fsm()
        } else {
          delay -= 1
        }
      }
      case 1 => {
        if(driver(stream.payload)){
          stream.valid #= true
          state += 1
        }
      }
      case 2 => {
        if(stream.logicalReady.toBoolean){
          stream.valid #= false
          stream.payload.randomize()
          delay = transactionDelay()
          state = 0
          fsm()
        }
      }
    }
  }
  clockDomain.onSamplings(fsm)

  def reset(): Unit = {
    state = 0
    stream.valid #= false
  }
}
