package spinal.core.fiber

import net.openhft.affinity.Affinity
import spinal.core.{GlobalData, ScopeProperty, SpinalError}
import spinal.sim.{JvmThread, SimManager}

import scala.collection.mutable

class EngineContext {
  val pending = mutable.Queue[AsyncThread]()
  val waiting = mutable.LinkedHashSet[AsyncThread]()
  val onCompletion = mutable.ArrayBuffer[() => Unit]()
  var currentAsyncThread : AsyncThread = null
  val cpuAffinity = SimManager.newCpuAffinity()

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
    var hadException = true
    val initialAffinity = Affinity.getAffinity
    try {
      Affinity.setAffinity(cpuAffinity) //Boost context switching by 2 on host OS, by 10 on VM
      while (pending.nonEmpty) {
        val t = pending.dequeue()
        t.context.restore()
        t.managerResume()
        if(t.isDone) println(s"Done   $t") else println(s"Resume $t")
        t.context = ScopeProperty.capture()
      }
      hadException = false
      onCompletion.foreach(_.apply())
    } finally {
      Affinity.setAffinity(initialAffinity)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unscheduleAsked = true)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unschedule())
      for (t <- (jvmIdleThreads ++ jvmBusyThreads)) {
        while (t.isAlive()) {
          Thread.sleep(0)
        }
      }
      initialContext.restore()

      if (waiting.nonEmpty) {
        println("\n! SpinalHDL async engine is stuck !")
        val incomingHandles = waiting.map(_.willLoad).filter(_ != null)
        for (t <- waiting; if !incomingHandles.contains(t.waitOn)) {
          println(s"$t wait on ${t.waitOn.getName("???")}")
        }
        if(!hadException) throw new Exception("SpinalHDL async engine is stuck")
      }
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


