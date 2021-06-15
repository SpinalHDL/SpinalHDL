package spinal.core.fiber

import net.openhft.affinity.Affinity
import spinal.core
import spinal.core.{Component, GlobalData, ScalaLocated, ScopeProperty, SpinalError}
import spinal.sim.{JvmThread, SimManager}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class EngineContext {
  val pending = mutable.Queue[AsyncThread]()
  val waiting = mutable.LinkedHashSet[AsyncThread]()
  var mainThread : AsyncThread = null
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
//      val c = Component.current
//      if(c != null && c.prePopTasks.nonEmpty) hardFork(c.rework(c.prePop()))
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
//        println(s"Resume $t")
        t.managerResume()
//        if(t.isDone) println(s"Done   $t")
        t.context = ScopeProperty.capture()
      }
      //Ensure there is no prepop tasks remaining, as things can be quite aggresively context switched since the fiber update
      var hadPrePop = true
      while(hadPrePop) {
        hadPrePop = false
        GlobalData.get.toplevel.walkComponents { c =>
//          assert(c.prePopTasks.isEmpty)
          if (c.prePopTasks.nonEmpty) {
            c.rework(
              c.prePop()
            )
            hadPrePop = true
          }
        }
      }

      if(waiting.isEmpty) onCompletion.foreach(_.apply())
      hadException = false
    } finally {
      if(!mainThread.isDone) {
        println("The main thread is stuck at :\n")
        println(ScalaLocated.long2(mainThread.jvmThread.getStackTrace))
        println("\n")
      }

      Affinity.setAffinity(initialAffinity)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unscheduleAsked = true)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unschedule())
      for (t <- (jvmIdleThreads ++ jvmBusyThreads)) {
        while (t.isAlive()) {
          Thread.sleep(0)
        }
      }
      initialContext.restore()
    }
    if (waiting.nonEmpty) {
      println("\n! SpinalHDL async engine is stuck !")
      val incomingHandles = waiting.flatMap(_.willLoadHandles).filter(_ != null)
      val handleToWaiters = mutable.LinkedHashMap[Handle[_], ArrayBuffer[AsyncThread]]()
      var count = 0
      for (t <- waiting; if !incomingHandles.contains(t.waitOn)) {
//        println(s"$t wait on ${t.waitOn.getName("???")}")
        handleToWaiters.getOrElseUpdate(t.waitOn, ArrayBuffer[AsyncThread]()) += t
        count += 1
      }

      for((handle, threads) <- handleToWaiters){
        println(s"Waiting on $handle defined at ${handle.getScalaLocationShort}:")
        threads.zipWithIndex.foreach{case(t, i) => println(s"${i+1}) $t")}
      }

//      println(count)
      println("\n")
      if(!hadException) throw new Exception("SpinalHDL async engine is stuck")
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
    e.mainThread = e.schedule{
      ret = body
      val dummy = 1
    }.setName(name)
    e.start
    ret
  }
}


