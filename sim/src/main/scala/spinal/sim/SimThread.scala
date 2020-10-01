package spinal.sim


import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class SimThreadUnschedule() extends Exception
class SimThread(body: => Unit) {
  private val manager = SimManagerContext.current.manager
  var waitingThreads = ArrayBuffer[() => Unit]()

  val mainContext = SimManagerContext.current
  var exception : Throwable = null
  def join(): Unit = {
    val thread = SimManagerContext.current.thread
    assert(thread != this)
    if (!this.isDone) {
      waitingThreads += thread.managerResume
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
          Thread.currentThread()
          if (cond || isDone) {
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
    jvmThread.barrier.await()
    jvmThread.park()
    manager.context.thread = SimThread.this
    if(isDone) {
      throw SimThreadUnschedule()
    }
  }

  def resume(): Unit = {
    SimManagerContext.current.manager.schedule(0)(this.managerResume())
  }

  //Should only be used from the sim manager itself, not from a simulation thread
  def managerResume() = {
    jvmThread.unpark()
    jvmThread.barrier.await()
    if (isDone) {
      if(exception != null) throw exception
      waitingThreads.foreach(thread => {
        SimManagerContext.current.manager.schedule(0)(thread())
      })
    }
  }

  var done = false
  def terminate(): Unit ={
    if(manager.context.thread == this) {
      throw SimThreadUnschedule()
    } else {
      done = true
    }
  }

  val seed = Random.nextLong()
  val jvmThread = manager.newJvmThread {
    manager.setupJvmThread(Thread.currentThread())
    SimManagerContext.threadLocal.set(mainContext)
    manager.context.thread = SimThread.this
    try {
      Random.setSeed(seed)
      body
    } catch {
      case e : JvmThreadUnschedule =>
        manager.context.thread = null
        done = true
        throw e
      case e : SimThreadUnschedule =>
      case e : Throwable =>
        exception = e
    }
    manager.context.thread = null
    done = true
  }
}
