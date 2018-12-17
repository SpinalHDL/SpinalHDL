package spinal.sim

import java.util.concurrent.locks.LockSupport

import scala.collection.mutable.ArrayBuffer

class SimThread(body: => Unit) {
  private val manager = SimManagerContext.current.manager
  var waitingThreads = ArrayBuffer[() => Unit]()

  val mainContext = SimManagerContext.current
  var exception : Throwable = null
  def join(): Unit = {
    val thread = SimManagerContext.current.thread
    assert(thread != this)
    if (!this.isDone) {
      waitingThreads += thread.resume
      thread.suspend()
    }
  }

  def sleep(cycles: Long): Unit = {
    manager.schedule(cycles, this)
    suspend()
  }

  def waitUntil(cond: => Boolean): Unit = {
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

  def suspend(): Unit = {
    manager.context.thread = null
    LockSupport.unpark(mainContext.masterThread)
    jvmThread.park()
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
    SimManagerContext.threadLocal.set(mainContext)
    manager.context.thread = SimThread.this
    try {
      body
    } catch {
      case e : JvmThreadUnschedule =>
        throw e
      case e : Throwable =>
        exception = e
    }
    manager.context.thread = null
    done = true
  }
}
