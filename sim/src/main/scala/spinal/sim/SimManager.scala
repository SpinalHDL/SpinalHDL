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

class SimCallSchedule(val time: Long, val call : ()  => Unit)

class SimSuccess extends Exception
class SimFailure(message : String) extends Exception (message)

class SimManager(val raw : SimRaw) {
  val threads = mutable.ArrayBuffer[SimCallSchedule]()
  val sensitivities = mutable.ArrayBuffer[SimManagerSensitive]()
  val commandBuffer = mutable.ArrayBuffer[() => Unit]()
  val onEndListeners = mutable.ArrayBuffer[() => Unit]()
  var schedulingOffset = 0
  var time = 0l
  private var retains = 0
  var userData : Any = null
  val context = new SimManagerContext()
//  var simContinue = false
  context.manager = this
  SimManagerContext.threadLocal.set(context)

  def onEnd(callback : => Unit) : Unit = onEndListeners += (() => callback)
  def getInt(bt : Signal) : Int = raw.getInt(bt)
  def getLong(bt : Signal) : Long = raw.getLong(bt)
  def getBigInt(bt : Signal) : BigInt = raw.getBigInt(bt)
  def setLong(bt : Signal, value : Long): Unit = {
    bt.dataType.checkLongRange(value, bt)
    commandBuffer += (() => raw.setLong(bt, value))
  }
  def setBigInt(bt : Signal, value : BigInt): Unit = {
    bt.dataType.checkBigIntRange(value, bt)
    commandBuffer += (() => raw.setBigInt(bt, value))
  }

  def schedule(thread : SimCallSchedule): Unit = {
    assert(thread.time >= time)
    threads.indexWhere(thread.time < _.time, schedulingOffset) match {
      case -1 => threads += thread
      case idx => threads.insert(idx, thread)
    }
  }

  def schedule(delay : Long)(thread : => Unit): Unit = {
    schedule( new SimCallSchedule(time + delay, () => thread))
  }

  def schedule(delay : Long, thread : SimThread): Unit = {
    schedule( new SimCallSchedule(time + delay, thread.resume))
  }


  def retain() = retains += 1
  def release() = retains -= 1

  def newThread(body : => Unit@suspendable): SimThread ={
    val thread = new SimThread(body)
    schedule(delay=0, thread)
    thread
  }



//  def exitSim(): Unit@suspendable = {
//    simContinue = false
//    SimManagerContext.current.thread.suspend()
//  }

  def run(body : => Unit@suspendable): Unit ={
    val startAt = System.nanoTime()
    val tRoot = new SimThread(body)
    schedule(delay=0, tRoot)
    runWhile(tRoot.nonDone)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }


  def runAll(body : => Unit@suspendable): Unit ={
    val startAt = System.nanoTime()
    val tRoot = new SimThread(body)
    schedule(delay=0, tRoot)
    runWhile(true)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }

  def runWhile(continueWhile : => Boolean = true): Unit ={
    try {
//      simContinue = true
      var forceDeltaCycle = false
      var evalNanoTime = 0l
      var evalNanoTimeRef = System.nanoTime()
      while (((continueWhile || retains != 0) && threads.nonEmpty/* && simContinue*/) || forceDeltaCycle) {
        //Process sensitivities

//        evalNanoTime -= System.nanoTime()
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
//        evalNanoTime += System.nanoTime()

        //Sleep until the next activity
        val nextTime = if(forceDeltaCycle) time else threads.head.time
        val delta = nextTime - time
        time = nextTime
        if (delta != 0) {
          raw.sleep(delta)
        }

        //Execute pending threads
        var threadsToRunCount = 0
        val threadsCount = threads.length
        while (threadsToRunCount < threadsCount && threads(threadsToRunCount).time == nextTime) {
          threadsToRunCount += 1
        }
        schedulingOffset = threadsToRunCount
        var threadId = 0
        while (threadId != threadsToRunCount) {
          val thread = threads(threadId)
          thread.call()
          threadId += 1
        }


        //Evaluate the hardware outputs
        if(forceDeltaCycle){
//          if(false) {
            raw.eval()
//          } else {
////            evalNanoTime -= System.nanoTime()
//            raw.eval()
//            val nano =  System.nanoTime()
////            evalNanoTime += nano
//            val elabsedTime = nano - evalNanoTimeRef
//            if(elabsedTime > 1000000000){
//              println(s"[Info] Simulator evaluation use ${100*evalNanoTime/elabsedTime}% of the processing")
//              evalNanoTimeRef = nano
//              evalNanoTime = 0
//            }
//          }
        }



        //Execute the threads commands
        forceDeltaCycle = commandBuffer.nonEmpty
        if(forceDeltaCycle){
          commandBuffer.foreach(_ ())
          commandBuffer.clear()
        }

        threads.remove(0, threadsToRunCount)
        schedulingOffset = 0
      }
      if(retains != 0){
        throw new SimFailure("Simulation ended while there was still some retains")
      }
    } catch {
      case e : SimSuccess =>
      case e : Throwable => {
        raw.sleep(1)
        raw.end()
        println(f"""[Error] Simulation failed at time=$time""")
        throw e
      }
    }
    onEndListeners.foreach(_())
    raw.end()
    SimManagerContext.threadLocal.set(null)
  }
}
