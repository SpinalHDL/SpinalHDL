package spinal.lib.sim


import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object StreamMonitor{
  def apply[T <: Data](stream : Stream[T], clockDomain: ClockDomain)(callback : (T) => Unit) = new StreamMonitor(stream,clockDomain).addCallback(callback)
}

class StreamMonitor[T <: Data](stream : Stream[T], clockDomain: ClockDomain){
  val callbacks = ArrayBuffer[(T) => Unit]()

  def addCallback(callback : (T) => Unit): this.type = {
    callbacks += callback
    this
  }

  var keepValue = false
  var payload : SimData = null
  var keepValueEnable = false

  clockDomain.onSamplings{
    val valid = stream.valid.toBoolean
    val ready = stream.ready.toBoolean

    if (valid && ready) {
      callbacks.foreach(_ (stream.payload))
    }
    if(keepValueEnable) {
      if (!keepValue) {
        if (valid) {
          keepValue = true
          payload = SimData.copy(stream.payload)
        }
      } else {
        assert(payload.equals(SimData.copy(stream.payload)))
      }
      if (ready) {
        keepValue = false
      }
    }
  }
}

object StreamDriver{
  def apply[T <: Data](stream : Stream[T], clockDomain: ClockDomain)(driver : (T) => Boolean) = new StreamDriver(stream,clockDomain,driver)
//  def apply[T <: Data](stream : Stream[T], clockDomain: ClockDomain)(driver : (T) => Unit) = new StreamDriver(stream,clockDomain,(x) => {driver(x); true})

  def queue[T <: Data](stream : Stream[T], clockDomain: ClockDomain) = {
    val cmdQueue = mutable.Queue[(T) => Unit]()
    val driver = StreamDriver(stream, clockDomain) { p =>
      if(cmdQueue.isEmpty) false else {
        cmdQueue.dequeue().apply(p)
        true
      }
    }
    (driver, cmdQueue)
  }
}

class StreamDriver[T <: Data](stream : Stream[T], clockDomain: ClockDomain, var driver : (T) => Boolean){
  var transactionDelay : () => Int = () => {
    val x = Random.nextDouble()
    (x*x*10).toInt
  }


  //The  following commented threaded code is the equivalent to the following uncommented thread-less code (nearly)
//  fork{
//    stream.valid #= false
//    while(true) {
//      clockDomain.waitSampling(transactionDelay())
//      clockDomain.waitSamplingWhere(driver(stream.payload))
//      stream.valid #= true
//      clockDomain.waitSamplingWhere(stream.ready.toBoolean)
//      stream.valid #= false
//      stream.payload.randomize()
//    }
//  }

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
        if(stream.ready.toBoolean){
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
}

object StreamReadyRandomizer {
  def apply[T <: Data](stream: Stream[T], clockDomain: ClockDomain) = new StreamReadyRandomizer(stream, clockDomain, () => true)
}

case class StreamReadyRandomizer[T <: Data](stream : Stream[T], clockDomain: ClockDomain, condition: () => Boolean){
  var factor = 0.5f
  clockDomain.onSamplings{
    if (condition()) {
      stream.ready #= Random.nextFloat() < factor
    } else {
      stream.ready #= false
    }
  }
}

//package workshop.simStreamJoinFork
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//
//object SimStreamUtils {
//  //Fork a thread to constantly randomize the valid/payload signals of the given stream
//  def streamMasterRandomizer[T <: Data](stream : Stream[T], clockDomain: ClockDomain): Unit = fork{
//    stream.valid #= false
//    while(true){
//      stream.valid.randomize()
//      stream.payload.randomize()
//      clockDomain.waitSampling()
//    }
//  }
//
//  //Fork a thread to constantly randomize the ready signal of the given stream
//  def streamSlaveRandomizer[T <: Data](stream : Stream[T], clockDomain: ClockDomain): Unit = fork{
//    while(true){
//      stream.ready.randomize()
//      clockDomain.waitSampling()
//    }
//  }
//
//  //Fork a thread which will call the body function each time a transaction is consumed on the given stream
//  def onStreamFire[T <: Data](stream : Stream[T], clockDomain: ClockDomain)(body : => Unit): Unit = fork{
//    while(true) {
//      clockDomain.waitSampling()
//      var dummy = if (stream.valid.toBoolean && stream.ready.toBoolean) {
//        body
//      }
//    }
//  }
//}


class SimStreamAssert[T <: Data](s : Stream[T], cd : ClockDomain){
  var valid = false
  var payload : SimData = null
  import spinal.core.sim._
  cd.onSamplings{
    if(!valid){
      if(s.valid.toBoolean) {
        valid = true
        payload = SimData.copy(s.payload)
      }
    }else {
      assert(payload.equals(SimData.copy(s.payload)))
    }
    if(s.ready.toBoolean) {
      valid = false
    }
  }
}
