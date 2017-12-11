package spinal.sim

import scala.collection.convert.WrapAsJava.asJavaIterator
import scala.collection.mutable
import scala.util.continuations._


trait SimThreadBlocker{
  def check() : Boolean
}

trait SimManagerSensitive{
  def update() : Boolean
}

class SimManager(val raw : SimRaw) {
  val threads = mutable.ArrayBuffer[SimThread]()
  val sensitivities = mutable.ArrayBuffer[SimManagerSensitive]()
  val commandBuffer = mutable.ArrayBuffer[() => Unit]()
  var schedulingOffset = 0
  var time = 0l
  var userData : Any = null
  val context = new SimManagerContext()
  context.manager = this
  SimManagerContext.threadLocal.set(context)

  def getInt(bt : Signal) : Int = raw.getInt(bt)
  def getLong(bt : Signal) : Long = raw.getLong(bt)
  def getBigInt(bt : Signal) : BigInt = raw.getBigInt(bt)
  def setLong(bt : Signal, value : Long): Unit = {
    commandBuffer += (() => raw.setLong(bt, value))
  }
  def setBigInt(bt : Signal, value : BigInt): Unit = {
    commandBuffer += (() => raw.setBigInt(bt, value))
  }
  def scheduleThread(thread : SimThread): Unit = {
    if(thread.time < time) thread.time = time
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
    val startAt = System.nanoTime()
    val tRoot = new SimThread(body, time)
    scheduleThread(tRoot)
    runWhile(tRoot.nonDone)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Progress] Simulation done in ${duration*1e3}%1.3f ms""")
  }

  def runWhile(continueWhile : => Boolean = true): Unit ={
    try {
      while (continueWhile && threads.nonEmpty) {
        val nextTime = threads.head.time
        val delta = nextTime - time
        time = nextTime

        var threadsToRunCount = 1
        val threadsCount = threads.length
        while (threadsToRunCount < threadsCount && threads(threadsToRunCount).time == nextTime) {
          threadsToRunCount += 1
        }
        if (delta != 0) {
          //        println("TIME=" + time)
          raw.sleep(delta)
        }
        schedulingOffset = threadsToRunCount
        var threadId = 0
        while (threadId != threadsToRunCount) {
          val thread = threads(threadId)
          context.thread = thread
          thread.resume()
          threadId += 1
        }
        commandBuffer.foreach(_ ())
        commandBuffer.clear()
        raw.eval()

        var sensitivitiesCount = sensitivities.length
        var sensitivitiesId = 0
        while (sensitivitiesId < sensitivitiesCount) {
          if (!sensitivities(sensitivitiesId).update()) {
            sensitivitiesCount -= 1
            sensitivities(sensitivitiesId) = sensitivities(sensitivitiesCount)
            sensitivities.remove(sensitivitiesCount)
          } else {
            sensitivitiesId += 1
          }
        }


        threads.remove(0, threadsToRunCount)
        schedulingOffset = 0
      }
    } catch {
      case e : Throwable => {
        raw.sleep(1)
        raw.end()
        println("ERROR")
        throw e
      }
    }
    raw.end()
  }
}
