package spinal.core.fiber

import net.openhft.affinity.Affinity
import spinal.core.{GlobalData, ScopeProperty, SpinalError}
import spinal.sim.{JvmThread, SimManager}

import scala.collection.mutable

class EngineContext {
  val pending = mutable.Queue[AsyncThread]()
  val waiting = mutable.LinkedHashSet[AsyncThread]()
  var currentAsyncThread : AsyncThread = null
  val cpuAffinity = SimManager.newCpuAffinity()
  Affinity.setAffinity(cpuAffinity) //Boost context switching by 2 on host OS, by 10 on VM

  //Manage the JvmThread poll
  val jvmBusyThreads = mutable.ArrayBuffer[JvmThread]()
  val jvmIdleThreads = mutable.Stack[JvmThread]()
  def newJvmThread(body : => Unit) : JvmThread = {
    if(jvmIdleThreads.isEmpty){
      val newJvmThread = new JvmThread(cpuAffinity){
        override def bodyDone(): Unit = {
          jvmBusyThreads.remove(jvmBusyThreads.indexOf(this))
          jvmIdleThreads.push(this)
        }
      }
      jvmIdleThreads.push(newJvmThread)
      newJvmThread.start()
      newJvmThread.barrier.await()
    }

    val gb = GlobalData.get
    val jvmThread = jvmIdleThreads.pop()
    jvmThread.body = {() =>
      GlobalData.set(gb)
      body
    }

    jvmBusyThreads += jvmThread
    jvmThread
  }


  def schedule(body : => Unit) : AsyncThread = {
    val t = new AsyncThread(currentAsyncThread, this, body)
    pending += t
    t
  }

  def start(): Unit ={
    val initialContext = ScopeProperty.capture()
    try {
      while (pending.nonEmpty) {
        val t = pending.dequeue()
        t.context.restore()
        t.managerResume()
        t.context = ScopeProperty.capture()
      }

      if (waiting.nonEmpty) {
        println("\n! SpinalHDL async engine is stuck !")
        for (t <- waiting) {
          println(s"$t wait on ${t.waitOn}")
        }
        throw new Exception("SpinalHDL async engine is stuck")
      }
    } finally {
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unscheduleAsked = true)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unschedule())
      for (t <- (jvmIdleThreads ++ jvmBusyThreads)) {
        while (t.isAlive()) {
          Thread.sleep(0)
        }
      }
      initialContext.restore()
    }
  }

  def sleep(asyncThread: AsyncThread): Unit ={
    waiting += asyncThread
    asyncThread.suspend()
  }

  def wakeup(asyncThread: AsyncThread): Unit ={
    waiting -= asyncThread
    pending += asyncThread
  }
}

object Engine extends ScopeProperty[EngineContext]{
  override protected var _default: EngineContext = null

  def create[T](body : => T, name : String = "root") = {
    val e = new EngineContext
    Engine.push(e)
    var ret : T = null.asInstanceOf[T]
    e.schedule{ret = body}.setName(name)
    e.start
    ret
  }
}


