package spinal.lib.sim


import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  val validProxy = stream.valid.simProxy()
  val readyProxy = stream.ready.simProxy()
  clockDomain.onSamplings{
    val valid = validProxy.toBoolean
    val ready = readyProxy.toBoolean

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

  def reset() {
    keepValueEnable = false
    keepValue = false
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
  implicit val _ = sm
  var transactionDelay : () => Int = () => {
    val x = simRandom.nextDouble()
    (x*x*10).toInt
  }

  var factor = Option.empty[Float]
  def setFactor(value : Float) = factor = Some(value)
  def setFactorPeriodically(period : Long) = periodicaly(period)(setFactor(simRandom.nextFloat()))

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
  val validProxy = stream.valid.simProxy()
  validProxy #= false
  stream.payload.randomize()

  val readyProxy = stream.ready.simProxy()

  def fsm(): Unit = {
    state match{
      case 0 => {
        factor match {
          case Some(x) => if(simRandom.nextFloat() < x) {
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
        if(driver(stream.payload)){
          validProxy #= true
          state += 1
        }
      }
      case 2 => {
        if(readyProxy.toBoolean){
          validProxy #= false
          stream.payload.randomize()
          if(factor.isEmpty){delay = transactionDelay()}
          state = 0
          fsm()
        }
      }
    }
  }
  clockDomain.onSamplings (fsm)

  def reset() {
    state = 0
    stream.valid #= false
  }
}

object StreamReadyRandomizer {
  def apply[T <: Data](stream: Stream[T], clockDomain: ClockDomain) = new StreamReadyRandomizer(stream, clockDomain, () => true)
}

case class StreamReadyRandomizer[T <: Data](stream : Stream[T], clockDomain: ClockDomain,var condition: () => Boolean){
  var factor = 0.5f
  def setFactor(value : Float) = factor = value
  def setFactorPeriodically(period : Long) = periodicaly(period)(setFactor(simRandom.nextFloat()))
  val readyProxy = stream.ready.simProxy()
  clockDomain.onSamplings{
    if (condition()) {
      readyProxy #= simRandom(readyProxy.manager).nextFloat() < factor
    } else {
      readyProxy #= false
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

/**
 * Allows to specify bursts of stream transactions, but those bursts will be scheduled out of order
 * The order inside each burst is preserved, once a burst began, nothing else goes until it is done.
 *
 * Usage :
 * myStreamDriverOoo.burst{ push =>
 *   push{ payload
 *      payload.mySignal #= beat0
 *   }
 *   push{ payload
 *      payload.mySignal #= beat1
 *   }
 * }
 */
class StreamDriverOoo[T <: Data](stream : Stream[T], cd: ClockDomain){
  implicit val _ = sm
  val storage = ArrayBuffer[mutable.Queue[(T) => Unit] => Unit]()
  val queue = mutable.Queue[(T) => Unit]()
  val ctrl = StreamDriver(stream, cd) { p =>
    while(queue.isEmpty && !storage.isEmpty){
      val index = simRandom.nextInt(storage.length)
      storage(index).apply(queue)
      storage.remove(index)
    }
    if(queue.isEmpty) false else {
      queue.dequeue().apply(p)
      true
    }
  }

  def single(body : T => Unit) : Unit = {
    storage += (q => q.enqueue(body))
  }
  def burst(body : ((T => Unit) => Unit) => Unit) = {
    storage += (q => {
      body.apply{e =>
        q.enqueue(e)
      }
    })
  }
}

object StreamDriverOoo{
  def apply[T <: Data](stream : Stream[T], cd: ClockDomain) = new StreamDriverOoo(stream, cd)
}