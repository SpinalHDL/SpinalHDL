package spinal.sim

import java.util.concurrent.CyclicBarrier
import net.openhft.affinity.Affinity

import scala.collection.mutable
import scala.util.Random


trait SimThreadBlocker{
  def check() : Boolean
}

trait SimManagerSensitive{
  def update() : Boolean
}

class SimCallSchedule(val time: Long, val call : ()  => Unit){
  var next : SimCallSchedule = null
}

class JvmThreadUnschedule extends Exception

//Reusable thread
abstract class JvmThread(cpuAffinity : Int) extends Thread{
  var body : () => Unit = null
  var unscheduleAsked = false
  val barrier = new CyclicBarrier(2)
  def park() : Unit = {
    barrier.await()
    if(unscheduleAsked)
      throw new JvmThreadUnschedule()
  }

  def unpark() : Unit = {
    barrier.await()
  }

  def unschedule(): Unit ={
    unscheduleAsked = true
    unpark()
  }


  override def run(): Unit = {
    spinal.affinity.Affinity(cpuAffinity)
    barrier.await()
    try {
      while (true) {
        park()
        body()
        body = null
        bodyDone()
        barrier.await()
      }
    } catch{
      case _ : JvmThreadUnschedule =>
    }
  }

  def bodyDone() : Unit
}

class SimSuccess extends Exception
class SimFailureBackend() extends Exception ()
class SimFailure(message : String) extends Exception (message)

object SimManager{
  var cpuAffinity = Random.nextInt(cpuCount)
  lazy val cpuCount = {
    try {
      val systemInfo = new oshi.SystemInfo
      systemInfo.getHardware.getProcessor.getLogicalProcessorCount
    } catch {
      // fallback when oshi can't work on Apple M1
      // see https://github.com/oshi/oshi/issues/1462
      // remove this workaround when the issue is fixed
      //
      // DO NOT REMOVE `_ : IllegalStateException` until net.java.dev.jna >= 5.8
      // see java-native-access/jna#1324, also SpinalHDL/SpinalHDL#711
      case e : Throwable => {
        Runtime.getRuntime().availableProcessors()
      }
    }
  }
  def newCpuAffinity() : Int = synchronized {
    val ret = cpuAffinity
    cpuAffinity = (cpuAffinity + 1) % cpuCount
    ret
  }
}

class SimManager(val raw : SimRaw, val random: Random = Random, val testName : String = "unnamed") {
  val cpuAffinity = SimManager.newCpuAffinity()
  val mainThread = Thread.currentThread()
  var threads : SimCallSchedule = null
  var printEvalTime = false

  val sensitivities = mutable.ArrayBuffer[SimManagerSensitive]()
  var commandBuffer = mutable.ArrayBuffer[() => Unit]()
  val onEndListeners = mutable.ArrayBuffer[() => Unit]()
  var time, deltaCycle = 0l
  private var retains = 0
  var userData : Any = null
  val context = new SimManagerContext()
  context.manager = this
  SimManagerContext.threadLocal.set(context)

  val timePrecision: BigDecimal = BigDecimal(10).pow(raw.getTimePrecision())

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

    val jvmThread = jvmIdleThreads.pop()
    jvmThread.body = () => body

    jvmBusyThreads += jvmThread
    jvmThread
  }

  def newSpawnTask() : SimThreadSpawnTask = new SimThreadSpawnTask {
    override def setup() = {} //Dummy
  }

  val readBypass = if(raw.isBufferedWrite) mutable.HashMap[Signal, BigInt]() else null
  def setupJvmThread(thread: Thread): Unit = {}
  def onEnd(callback : => Unit) : Unit = onEndListeners += (() => callback)
  def getInt(bt : Signal) : Int = {
    if(readBypass == null) return raw.getInt(bt)


    readBypass.get(bt) match {
      case Some(x) =>
        val v = x.toInt
        bt.dataType.checkIntRange(v, bt)
        v
      case _ => raw.getInt(bt)
    }
  }
  def getLong(bt : Signal) : Long =   {
    if(readBypass == null) return raw.getLong(bt)
    readBypass.get(bt) match {
      case Some(x) =>
        val v = x.toLong
        bt.dataType.checkLongRange(v, bt)
        v
      case _ => raw.getLong(bt)
    }
  }
  def getBigInt(bt : Signal) : BigInt =  {
    if(readBypass == null) return raw.getBigInt(bt)
    readBypass.get(bt) match {
      case Some(x) =>
        bt.dataType.checkBigIntRange(x, bt)
        x
      case _ => raw.getBigInt(bt)
    }
  }

  def getBigInt(bt : Signal, address : Long) : BigInt =  {
    raw.getBigIntMem(bt, address)
  }

  def setLong(bt : Signal, value : Long): Unit = {
    bt.dataType.checkLongRange(value, bt)
    commandBuffer += {() => raw.setLong(bt, value); if(readBypass != null)(readBypass += (bt -> BigInt(value)))}
  }
  def setBigInt(bt : Signal, value : BigInt): Unit = {
    bt.dataType.checkBigIntRange(value, bt)
    commandBuffer += {() => raw.setBigInt(bt, value); if(readBypass != null) readBypass += (bt -> value)}
  }
  def setBigInt(mem : Signal,address : Long, value : BigInt): Unit = {
    mem.dataType.checkBigIntRange(value, mem)
    commandBuffer += {() => raw.setBigIntMem(mem, value, address) /*;if(readBypass != null) readBypass += (bt -> value)*/}
  }

  def schedule(thread : SimCallSchedule): Unit = {
    assert(thread.time >= time)
    var ptr = threads
    ptr match {
      case null => threads = thread
      case _ if ptr.time > thread.time => thread.next = threads; threads = thread
      case _ => {
        while(ptr.next != null && ptr.next.time <= thread.time){
          ptr = ptr.next
        }
        thread.next = ptr.next
        ptr.next = thread
      }
    }
  }

  def schedule(delay : Long)(thread : => Unit): Unit = {
    schedule( new SimCallSchedule(time + delay, () => thread))
  }

  def schedule(delay : Long, thread : SimThread): Unit = {
    val s = new SimCallSchedule(time + delay, thread.managerResume)
    schedule(s)
  }


  def retain() = retains += 1
  def release() = retains -= 1

  def newThread(body : => Unit): SimThread ={
    val thread = new SimThread(body)
    schedule(delay=0, thread)
    thread
  }



  def run(body : => Unit): Unit ={
    val startAt = System.nanoTime()
    def threadBody(body : => Unit) : Unit = {
      body
      throw new SimSuccess
    }
    val tRoot = new SimThread(threadBody(body))
    schedule(delay=0, tRoot)
    runWhile(true)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }


  def runAll(body : => Unit): Unit ={
    val startAt = System.nanoTime()
    val tRoot = new SimThread(body)
    schedule(delay=0, tRoot)
    runWhile(true)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }

  def runWhile(continueWhile : => Boolean = true): Unit ={
    val initialAffinity = Affinity.getAffinity
    spinal.affinity.Affinity(cpuAffinity) //Boost context switching by 2 on host OS, by 10 on VM
    try {
//      simContinue = true
      var forceDeltaCycle = false
      var evalNanoTime = 0l
      var evalNanoTimeRef = System.nanoTime()
      deltaCycle = 0

      if(raw.eval()){
        throw new SimFailure("RTL assertion failure")
      }

      var nanoCounter = 0l
      var evalCalls = 0l
      var nanoStart = System.nanoTime()
      while (((continueWhile || retains != 0) && threads != null/* && simContinue*/) || forceDeltaCycle) {
        //Sleep until the next activity
        val nextTime = if(forceDeltaCycle) time else threads.time
        val delta = nextTime - time
        time = nextTime
        if (delta != 0) {
          raw.sleep(delta)
          deltaCycle = 0
        }

        //Execute pending threads
        var tPtr = threads
        while (tPtr != null && tPtr.time == nextTime) {
          tPtr = tPtr.next
        }
        var tPtr2 = threads
        threads = tPtr
        while (tPtr2 != tPtr) {
          tPtr2.call()
          tPtr2 = tPtr2.next
        }

        //Evaluate the hardware outputs
        if(forceDeltaCycle){
          printEvalTime match {
            case false => {
              if (raw.eval()) throw new SimFailure("HDL assertion failure")
            }
            case true => {
              val t1 = System.nanoTime()
              if (raw.eval()) throw new SimFailure("HDL assertion failure")
              val t2 = System.nanoTime()
              evalCalls += 1
              nanoCounter += t2 - t1
              if (evalCalls % 100000 == 0) {
                println(f"evalAVG=${nanoCounter / evalCalls} ns, eff=${nanoCounter * 100 / (t2 - nanoStart)}%%")
              }
            }
          }
        }

        //Execute the threads commands
        if(commandBuffer.nonEmpty){
          commandBuffer.foreach(_())
          commandBuffer.clear()
          forceDeltaCycle = true
        } else {
          forceDeltaCycle = false
        }

        //Process sensitivities
        deltaCycle += 1
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

        forceDeltaCycle |= commandBuffer.nonEmpty
      }
      if(retains != 0){
        throw new SimFailure("Simulation ended while there was still some retains")
      } else {
        throw new SimFailure("Simulation ended in a freeze state, there is nothing to squedule which can make time advance.")
      }
    } catch {
      case e : SimSuccess =>
      case e : Throwable => {
        println(f"""[Error] Simulation failed at time=$time""")
        raw.sleep(1)
        val str = e.getStackTrace.head.toString
        if(str.contains("spinal.core.") && !str.contains("sim")){
          System.err.println("It seems like you used some SpinalHDL hardware elaboration API in the simulation. If you did, you shouldn't.")
        }
        throw e
      }
    } finally {
      spinal.affinity.Affinity(initialAffinity)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unscheduleAsked = true)
      (jvmIdleThreads ++ jvmBusyThreads).foreach(_.unschedule())
      for(t <- (jvmIdleThreads ++ jvmBusyThreads)){
        while(t.isAlive()){Thread.sleep(0)}
      }
      onEndListeners.foreach(_())
      raw.end()
      SimManagerContext.threadLocal.set(null)
    }
  }
}
