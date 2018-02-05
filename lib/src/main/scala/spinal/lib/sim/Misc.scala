package spinal.lib.sim
import spinal.core.sim._
import spinal.sim.SimManagerContext

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

  def retainer(count : Int) = new {
    var counter = 0
    if(count != 0) retain()
    def release(): Unit ={
      counter += 1
      if(counter == count){
        Phase.this.release()
      }
    }
  }
  def onActivate(listener :  => Unit) : Unit = activeListeners += (() => listener)
  def onEnd(listener :  => Unit) : Unit = endListeners += (() => listener)
  def apply(listener :  => Unit) : Unit = onActivate(listener)
  def retainFor(time : Long): Unit ={
    def doit: Unit ={
      fork{
        sleep(time)
        release()
      }
    }

    retain()
    if(isActive){
      doit
    }else{
      onActivate(doit)
    }
  }

}

class PhaseContext{
  val end = new Phase(null){
    retain()
    override def activate(): Unit = {
      super.activate()
      simSuccess()
    }
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
    SimManagerContext.current.manager.retain()
    threadLocal.set(new PhaseContext)
    SimManagerContext.current.manager.onEnd(threadLocal.remove())
  }
  def setup: Phase = context.setup
  def stimulus: Phase = context.stimulus
  def flush: Phase = context.flush
  def check:  Phase = context.check
  private def end:  Phase = context.check
  def isUsed = threadLocal.get() != null
}