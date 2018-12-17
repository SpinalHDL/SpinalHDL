package spinal.sim

import java.util.concurrent.locks.LockSupport

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations._




class SimThread(body: => Unit@suspendable) {
  private val manager = SimManagerContext.current.manager
  var waitingThreads = ArrayBuffer[() => Unit]()

  val masterContext = SimManagerContext.current
  var exception : Throwable = null
  def join(): Unit@suspendable = {
    val thread = SimManagerContext.current.thread
    assert(thread != this)
    if (!this.isDone) {
      waitingThreads += thread.resume
      thread.suspend()
    }
  }

  def sleep(cycles: Long): Unit@suspendable = {
    manager.schedule(cycles, this)
    suspend()
  }

  def waitUntil(cond: => Boolean): Unit@suspendable = {
    if (!cond) {
      manager.sensitivities += new SimManagerSensitive {
        override def update() = {
          if (cond) {
            manager.schedule(0, SimThread.this)
            false
          } else {
            true
          }
        }
      }
      suspend()
    }
  }

  def isDone: Boolean  = done
  def nonDone: Boolean = !done

  def suspend(): Unit@suspendable = {
    manager.context.thread = null
    LockSupport.unpark(masterContext.masterThread)
    LockSupport.park()
    manager.context.thread = SimThread.this
  }


  def resume() = {
    LockSupport.unpark(jvmThread)
    LockSupport.park()
    if (isDone) {
      if(exception != null) throw exception
      waitingThreads.foreach(thread => {
        SimManagerContext.current.manager.schedule(0)(thread())
      })
    }
  }

  var done = false
  val jvmThread = manager.newJvmThread {
    manager.setupJvmThread(Thread.currentThread())
    SimManagerContext.threadLocal.set(masterContext)
    manager.context.thread = SimThread.this
    try {
      body
    } catch {
      case e : Throwable => exception = e
    }
    manager.context.thread = null
    done = true
  }






//  private val manager = SimManagerContext.current.manager
//  private var nextStep: Unit => Unit = null
//  var waitingThreads = ArrayBuffer[() => Unit]()
//
//
//
//  def join(): Unit@suspendable = {
//    val thread = SimManagerContext.current.thread
//    assert(thread != this)
//    if (!this.isDone) {
//      waitingThreads += thread.resume
//      thread.suspend()
//    }
//  }
//
//  def sleep(cycles: Long): Unit@suspendable = {
//    manager.schedule(cycles, this)
//    suspend()
//  }
//
//  def waitUntil(cond: => Boolean): Unit@suspendable = {
//    if (!cond) {
//      manager.sensitivities += new SimManagerSensitive {
//        override def update() = {
//          if (cond) {
//            manager.schedule(0, SimThread.this)
//            false
//          } else {
//            true
//          }
//        }
//      }
//      suspend()
//    }
//  }
//
//  def isDone: Boolean = nextStep == null
//  def nonDone: Boolean = nextStep != null
//
//  def suspend(): Unit@suspendable = {
//    shift { k: (Unit => Unit) =>
//      nextStep = k
//    }
//  }
//
//  def unschedule(): Unit ={
//    nextStep = null
//  }
//
//  def resume() = {
//    manager.context.thread = this
//    if (nextStep != null) {
//      val back = nextStep
//      nextStep = null
//      if (back != null) back()
//    }
//    manager.context.thread = null
//    if (isDone) {
//      waitingThreads.foreach(thread => {
//        SimManagerContext.current.manager.schedule(0)(thread())
//      })
//    }
//  }
//
//
//  reset {
//    suspend()
//    body
//  }

}
