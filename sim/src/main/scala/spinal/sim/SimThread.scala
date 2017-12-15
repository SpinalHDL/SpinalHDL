package spinal.sim

import scala.collection.mutable.ArrayBuffer
import scala.util.continuations._


class SimThread(body: => Unit@suspendable, var time: Long) {
  private val manager = SimManagerContext.current.manager
  private var nextStep: Unit => Unit = null
  var waitingThreads = ArrayBuffer[SimThread]()



  def join(): Unit@suspendable = {
    val thread = SimManagerContext.current.thread
    assert(thread != this)
    if (!this.isDone) {
      waitingThreads += thread
      thread.suspend()
    }
  }

  def sleep(cycles: Long): Unit@suspendable = {
    time += cycles
    manager.scheduleThread(this)
    suspend()
  }

  def waitUntil(cond: => Boolean): Unit@suspendable = {
    if (!cond) {
      manager.sensitivities += new SimManagerSensitive {
        override def update() = {
          if (cond) {
            manager.scheduleThread(SimThread.this)
            false
          } else {
            true
          }
        }
      }
      suspend()
    }
  }

  def isDone: Boolean = nextStep == null
  def nonDone: Boolean = nextStep != null

  def suspend(): Unit@suspendable = {
    shift { k: (Unit => Unit) =>
      nextStep = k
    }
  }

  def resume() = {
    if (nextStep != null) {
      val back = nextStep
      nextStep = null
      if (back != null) back()
    }
    if (isDone) {
      waitingThreads.foreach(thread => {
        thread.time = time
        SimManagerContext.current.manager.scheduleThread(thread)
      })
    }
  }


  reset {
    suspend()
    body
  }

}
