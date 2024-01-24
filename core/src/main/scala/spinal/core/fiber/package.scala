package spinal.core

import spinal.sim.{JvmThread, JvmThreadUnschedule}
import scala.collection.Seq

package object fiber {
  class HardForkApi {
    def on[T](body : => T) : Handle[T] = hardFork(body)
  }
  def hardFork : HardForkApi = new HardForkApi
  def hardFork[T](body : => T) : Handle[T] = hardForkRaw(true)(body)._1
  def hardForkRaw[T](withDep : Boolean = true)(body : => T) : (Handle[T], AsyncThread) = {
    val ret = Handle[T]
    val e = Engine.get
    val t = e.schedule{
      val v = body
      ret.load(v)
    }
    if(withDep) {
      ret.willBeLoadedBy = t
      t.addSoonHandle(ret)
    }
    (ret, t)
  }

  def hardForkRawHandle[T](withDep: Boolean = true)(body: Handle[T] => T): (Handle[T], AsyncThread) = {
    val ret = Handle[T]
    val e = Engine.get
    val t = e.schedule {
      val v = body(ret)
      ret.load(v)
    }
    if (withDep) {
      ret.willBeLoadedBy = t
      t.addSoonHandle(ret)
    }
    (ret, t)
  }

  def soon(that : Seq[Handle[_]])  : Unit = {
    val t = Engine.get.currentAsyncThread
    that.foreach(t.addSoonHandle(_))
  }

  def soon(that : Handle[_], others : Handle[_]*)  : Unit = {
    val t = Engine.get.currentAsyncThread
    t.addSoonHandle(that)
    others.foreach(t.addSoonHandle(_))
  }
}
