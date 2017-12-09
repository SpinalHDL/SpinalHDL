package spinal.sim

import scala.collection.mutable
import scala.util.continuations._



class SimManager(val raw : SimRaw) {
  val threads = mutable.ArrayBuffer[SimThread]()
  var schedulingOffset = 0
  var time = 0l
  var userData : Any = null
  val context = new SimManagerContext()
  context.manager = this
  SimManagerContext.threadLocal.set(context)


  def getLong(bt : Signal) : Long = raw.getLong(bt)
  def setLong(bt : Signal, value : Long)= raw.setLong(bt, value)
//  def getClock(clockDomain: ClockDomain) = raw.getClock(clockDomain)
  def scheduleThread(thread : SimThread): Unit ={
    threads.indexWhere(thread.time < _.time, schedulingOffset) match {
      case -1 => threads += thread
      case idx => threads.insert(idx, thread)
    }
  }

  def newThread(body : => Unit@suspendable): SimThread@suspendable ={
    val thread = new SimThread(body, time)
    scheduleThread(thread)
    thread
  }

  def run(body : => Unit@suspendable): Unit ={
    scheduleThread(new SimThread(body, time))
    run()
  }
  def run(): Unit ={
    var continue = true
    while(continue){
      val nextTime = threads.head.time
      val delta = nextTime - time
      time = nextTime

      var threadsToRunCount = 1
      val threadsCount = threads.length
      while(threadsToRunCount < threadsCount && threads(threadsToRunCount).time == nextTime){
        threadsToRunCount += 1
      }
      if(delta != 0) {
//        println("TIME=" + time)
        raw.sleep(delta)
      }
      schedulingOffset = threadsToRunCount
      var threadId = 0
      while(threadId != threadsToRunCount){
        val thread = threads(threadId)
        context.thread = thread
        thread.resume()
        threadId += 1
      }
      raw.eval()
      threads.remove(0, threadsToRunCount)
      schedulingOffset = 0

      if(threads.isEmpty){
        continue = false
      }
    }

    raw.end()
  }
}
