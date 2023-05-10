package spinal.core.fiber

import spinal.core.GlobalData

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ElabOrderId{
  val SETUP = 0
  val BUILD = 1000000
}

object Elab {
  def apply[T](orderId : Int)(body : => T) : Handle[T] = {
    GlobalData.get.elab.addTask(orderId)(body)
  }
  def setup[T](body : => T) : Handle[T] = apply(ElabOrderId.SETUP)(body)
  def build[T](body : => T) : Handle[T] = apply(ElabOrderId.BUILD)(body)
}

class Elab {
  def addTask[T](orderId : Int)(body : => T) : Handle[T] = {
    val lock = Lock().retain()
    val (h, t) = hardForkRaw(withDep = false){
      lock.await()
      body
    }
    lock.setCompositeName(h, "lock")
    tasks.getOrElseUpdate(orderId, mutable.Queue[(Lock, Handle[_], AsyncThread)]()) += Tuple3(lock, h, t)
    h
  }
  val tasks = new mutable.LinkedHashMap[Int, mutable.Queue[(Lock, Handle[_], AsyncThread)]]()

  def runSync(): Unit ={
    val e = Engine.get

    //Link locks to current thread
    tasks.foreach(_._2.foreach{ e =>
      e._1.willBeLoadedBy = AsyncThread.current
      AsyncThread.current.addSoonHandle(e._1)
    })
    while(tasks.nonEmpty){
      val orderId = tasks.keys.min
      val queue = tasks(orderId)
      val locks = ArrayBuffer[Handle[_]]()
      while(queue.nonEmpty){
        val (l, h, t) = queue.dequeue()
        h.willBeLoadedBy = t
        t.addSoonHandle(h)
        l.release()
        locks += h
      }
      locks.foreach(_.await())
      tasks.remove(orderId)
    }
  }
}

object ElabDemo extends App {
  import spinal.core._
  SpinalVerilog{new Component{
    val b1 = Elab build new Area{
      println("Build 1")
      b2.get
      println("Build 1 Done")
    }
    val b2 = Elab build new Area{
      println("Build 2")
      println("Build 2 Done")
    }
    val s1 = Elab setup new Area{
      println("Setup 1")
      b1.get
      println("Setup 1  Done")
    }
    val s2 = Elab setup new Area{
      println("Setup 2")
      println("Setup 2 Done")
    }
  }}
}