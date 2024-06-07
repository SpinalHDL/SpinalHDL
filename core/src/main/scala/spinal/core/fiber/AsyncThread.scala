package spinal.core.fiber

import spinal.core.ScalaLocated.filterStackTrace
import spinal.core.{Nameable, ScalaLocated, ScopeProperty}
import spinal.sim.{JvmThread, JvmThreadUnschedule, SimThread, SimThreadUnschedule}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object AsyncThread{
  def current = Engine.get.currentAsyncThread
}

class AsyncThread(parent : AsyncThread, engine: EngineContext, body : => Unit) extends Nameable { // TODO
  var exception : Throwable = null
  var done = false
  def isDone = done
  val willLoadHandles = ArrayBuffer[Handle[_]]()
  var context = ScopeProperty.capture()

  def addSoonHandle(that : Handle[_]): Unit ={
    willLoadHandles += that
    that.willBeLoadedBy = this
  }

  def managerResume() = {
    waitOn = null
    jvmThread.unpark()
    jvmThread.barrier.await()
    if (isDone) {
      if(exception != null) throw exception
    }
  }

  var allowSuspend = true
  def suspend(): Unit = {
    assert(allowSuspend)
    engine.currentAsyncThread = null
    jvmThread.barrier.await()
    jvmThread.park()
    engine.currentAsyncThread = this
    if(isDone) {
      throw SimThreadUnschedule()
    }
  }

  var waitOn : Handle[_] = null


  var terminatedStackTrace : Array[StackTraceElement] = null

  val jvmThread : JvmThread = Engine.get.newJvmThread {
//    manager.setupJvmThread(Thread.currentThread())
//    SimManagerContext.threadLocal.set(mainContext)
//    manager.context.thread = SimThread.this
    try {
      context.restore()
      engine.currentAsyncThread = this
      body
    } catch {
      case e : JvmThreadUnschedule =>
        engine.currentAsyncThread = null
        terminatedStackTrace = e.getStackTrace
        done = true
        throw e
//      case e : SimThreadUnschedule =>
      case e : Throwable =>
        exception = e
    }
    engine.currentAsyncThread = null
    done = true
  }

  override def toString: String = {
    if(isNamed) return getName()
    if(willLoadHandles.nonEmpty) s"${willLoadHandles.filter(!_.isLoaded).map(_.getName("???")).mkString(", ")} loader" else super.toString
  }

  def getLocationShort() : String = {
    done match {
      case true => {
        val filtred = filterStackTrace(terminatedStackTrace)
        if(filtred.isEmpty) return terminatedStackTrace(0).toString
        filtred(0).toString
      }
    }
  }
}
