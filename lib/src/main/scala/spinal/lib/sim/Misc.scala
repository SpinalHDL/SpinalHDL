package spinal.lib.sim
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class Phase(next : Phase){
  var isActive : Boolean = false
  var activeListeners = ArrayBuffer[() => Unit]()
  var endListeners = ArrayBuffer[() => Unit]()
  def activate(): Unit ={
    isActive = true
    activeListeners.foreach(_())
    release()
  }
  private var retains = 1
  def retain() : Unit = retains += 1
  def release() : Unit = {
    retains -= 1
    if(retains == 0){
      isActive = false
      endListeners.foreach(_())
      next.activate()
    }
  }
  def onActivate(listener :  => Unit) : Unit = activeListeners += (() => listener)
  def onEnd(listener :  => Unit) : Unit = endListeners += (() => listener)
  def apply(listener :  => Unit) : Unit = onActivate(listener)
}

class PhaseContext{
  val end = new Phase(null){
    override def activate(): Unit = simSuccess()
  }
  val check = new Phase(end)
  val flush = new Phase(check)
  val stimulus = new Phase(flush)
  val setup = new Phase(stimulus)
  fork{
    setup.activate()
  }
}

object Phase{
  val threadLocal = new ThreadLocal[PhaseContext]
  def context = threadLocal.get()
  def boot() : Unit = {
    threadLocal.set(new PhaseContext)
  }
  def setup: Phase = context.setup
  def stimulus: Phase = context.stimulus
  def flush: Phase = context.flush
  def check:  Phase = context.check
}