package spinal.core.fiber

import spinal.core.{Area, GlobalData, OnCreateStack}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

object ElabOrderId{
  val INIT  = -1000000
  val SETUP = 0
  val BUILD = 1000000
  val PATCH = 1500000
  val CHECK = 2000000

  def getName(that : Int) = that match {
    case INIT  => "init"
    case SETUP => "setup"
    case BUILD => "build"
    case PATCH => "patch"
    case CHECK => "check"
  }
}

object Fiber {
  def apply[T: ClassTag](orderId : Int)(body : => T) : Handle[T] = {
    GlobalData.get.elab.addTask(orderId)(body)
  }
  def setup[T: ClassTag](body : => T) : Handle[T] = apply(ElabOrderId.SETUP)(body)
  def build[T: ClassTag](body : => T) : Handle[T] = apply(ElabOrderId.BUILD)(body)
  def check[T: ClassTag](body : => T) : Handle[T] = apply(ElabOrderId.CHECK)(body)

  def callback(orderId: Int)(body: => Unit): Unit = {
    GlobalData.get.elab.addCallback(orderId)(body)
  }

  def setupCallback(body: => Unit): Unit = callback(ElabOrderId.SETUP)(body)
  def buildCallback(body: => Unit): Unit = callback(ElabOrderId.BUILD)(body)

  def await(id : Int): Unit = {
    GlobalData.get.elab.await(id)
  }

  def awaitSetup() = await(ElabOrderId.SETUP)
  def awaitBuild() = await(ElabOrderId.BUILD)
  def awaitPatch() = await(ElabOrderId.PATCH)
  def awaitCheck() = await(ElabOrderId.CHECK)
}

class Fiber extends Area{
  var currentOrderId = ElabOrderId.INIT

  val spawnLock = Lock()
  val startLock = mutable.LinkedHashMap[Int, Lock]()
  val doneRetainer = mutable.LinkedHashMap[Int, Retainer]()
  val taskRetainer = mutable.LinkedHashMap[AsyncThread, RetainerHold]()
  val callbacks = new mutable.LinkedHashMap[Int, mutable.Queue[() => Unit]]()

  var syncThread : AsyncThread = null

  def addTask[T: ClassTag](body: => T): Handle[T] = {
    addTask(ElabOrderId.INIT)(body)
  }
  def addTask[T: ClassTag](orderId : Int)(body : => T) : Handle[T] = {
    spawnLock.retain()
    val (h, t) = hardForkRawHandle(withDep = true) { h : Handle[T] =>
      spawnLock.release()
      Fiber.await(orderId)
      if(classOf[Area].isAssignableFrom(classTag[T].runtimeClass)) {
        OnCreateStack.set(a => a.setCompositeName(h))
      }
      val ret = body
      taskRetainer.get(AsyncThread.current).foreach(_.release())
      ret
    }
    t.setCompositeName(h)
    h
  }

  def await(id : Int) = {
    taskRetainer.get(AsyncThread.current).foreach(_.release())
    val newRetainer = doneRetainer.getOrElseUpdate(id,new Retainer().setCompositeName(this, s"${ElabOrderId.getName(id)}_doneRetainer"))
    taskRetainer(AsyncThread.current) = newRetainer().setCompositeName(AsyncThread.current, "fiber_completion")

    startLock.getOrElseUpdate(id, {
      val r = new Lock().setCompositeName(this, s"${ElabOrderId.getName(id)}_start").retain()
      if(syncThread != null) {
        r.willBeLoadedBy = syncThread
        syncThread.addSoonHandle(r)
      }
      r
    }).await()
  }

  def addCallback(orderId : Int)(body : => Unit): Unit = {
    callbacks.getOrElseUpdate(orderId, mutable.Queue[() => Unit]()).enqueue(() => body)
  }


  def runSync(): Unit ={
    val e = Engine.get
    syncThread = AsyncThread.current
    spawnLock.await()

    for(sl <- startLock.values){
      sl.willBeLoadedBy = AsyncThread.current
      AsyncThread.current.addSoonHandle(sl)
    }

    while(startLock.nonEmpty){
      val phaseId = startLock.keys.min
      callbacks.get(phaseId).foreach(_.foreach(_.apply()))
      callbacks.remove(phaseId)
      startLock(phaseId).release()
      startLock.remove(phaseId)
      doneRetainer(phaseId).await()
    }
  }
}

object ElabDemo extends App {
  import spinal.core._

  SpinalVerilog{new Component{
    //Fork a thread which will start in build phase
    val b1 = Fiber build new Area{
      println("Build 1")
      println("b2.miaou = " + b2.miaou) //Will wait until the b2 thread completed
      println("Build 1 Done")
    }
    val b2 = Fiber build new Area{
      println("Build 2")
      val miaou = 42
      println("Build 2 Done")
    }
    val s1 = Fiber setup new Area{
      println("Setup 1")
      println("Setup 1  Done")
    }
    val s2 = Fiber setup new Area{
      println("Setup 2")
      println("Setup 2 Done")
    }
  }}

  SpinalVerilog{new Component{
    val miaou = Handle[Int]
    val b1 = hardFork on new Area{
      println("Build 1")
      println("miaou = " + miaou.get) //Will wait until the miaou handle is loaded
      println("Build 1 Done")
    }
    val b2 = hardFork on new Area{
      println("Build 2")
      miaou.load(42)
      println("Build 2 Done")
    }
  }}


  SpinalVerilog{new Component{
    val lock = Lock()
    val b1 = Fiber build new Area{
      println("Build 1")
      lock.await() //Will be blocked until b2 release the lock (see val s1)
      println("Build 1 Done")
    }
    val b2 = Fiber build new Area{
      println("Build 2")
      lock.release()
      println("Build 2 Done")
    }
    val s1 = Fiber setup new Area{
      println("Setup 1")
      lock.retain() //Let's lock the lock to allow b2 running stuff before b1
      println("Setup 1  Done")
    }
  }}
}