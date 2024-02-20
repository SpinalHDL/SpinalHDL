package spinal.core.fiber

import spinal.core.{Area, GlobalData}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

object ElabOrderId{
  val INIT  = -1000000
  val SETUP = 0
  val BUILD = 1000000
  val CHECK = 2000000
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
  def awaitCheck() = await(ElabOrderId.CHECK)
}

class Fiber extends Area{
  var currentOrderId = ElabOrderId.INIT


  class Fence(id : Int) extends Area{
    val lock = Lock().retain()
    val pendings = mutable.ArrayBuffer[AsyncThread]()
    def await(): Unit = {
      pendings += AsyncThread.current
      lock.await()
    }
    setCompositeName(Fiber.this, s"lock_${id.toString}")
  }

  val inflightLock = Lock()
  val inflight = mutable.LinkedHashSet[AsyncThread]()
  val fences = mutable.LinkedHashMap[Int, Fence]()
  val callbacks = new mutable.LinkedHashMap[Int, mutable.Queue[() => Unit]]()

  private def active(t : AsyncThread): Unit = {
    inflight += t
    inflightLock.retain()
  }

  private def idle(): Unit = {
    inflight -= AsyncThread.current
    inflightLock.release()
  }

  def addTask[T: ClassTag](body: => T): Handle[T] = {
    addTask(ElabOrderId.INIT)(body)
  }
  def addTask[T: ClassTag](orderId : Int)(body : => T) : Handle[T] = {
    val (h, t) = hardForkRawHandle(withDep = true) { h : Handle[T] =>
      Fiber.await(orderId)
      if(classTag[T].runtimeClass == classOf[Area]) {
        GlobalData.get.onAreaInit = Some(a => a.setCompositeName(h))
      }
      val ret = body
      idle()
      ret
    }
    t.setCompositeName(h)
    active(t)
    h
  }

  def await(id : Int) = {
    idle()
    fences.getOrElseUpdate(id, new Fence(id)).await()
  }

  def addCallback(orderId : Int)(body : => Unit): Unit = {
    callbacks.getOrElseUpdate(orderId, mutable.Queue[() => Unit]()).enqueue(() => body)
  }


  def runSync(): Unit ={
    val e = Engine.get
    //Link locks to current thread
    fences.values.foreach{ e =>
      e.lock.willBeLoadedBy = AsyncThread.current
      AsyncThread.current.addSoonHandle(e.lock)
    }
    while(fences.nonEmpty || callbacks.nonEmpty || inflight.nonEmpty){
      inflightLock.await()
      assert(inflight.size == 0)

      val orderId = (fences.keys ++ callbacks.keys).min
      currentOrderId = orderId

      callbacks.get(orderId).foreach{ l =>
        l.foreach(_.apply())
        callbacks.remove(orderId)
      }

      fences.get(orderId) match {
        case Some(lock) => {
          lock.pendings.foreach(active)
          lock.lock.release()
          fences.remove(orderId)
        }
        case None =>
      }

      inflightLock.await()
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