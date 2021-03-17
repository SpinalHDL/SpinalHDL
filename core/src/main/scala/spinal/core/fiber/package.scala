package spinal.core

import spinal.sim.{JvmThread, JvmThreadUnschedule}

package object fiber {
  def hardFork[T](body : => T) : Handle[T] = {
    val ret = Handle[T]
    val e = Engine.get
    val t = e.schedule{
      val v = body
      ret.load(v)
    }
    ret.willBeLoadedBy = t
    t.addSoonHandle(ret)
    ret
  }

  def soon(that : Handle[_]*)  : Unit = {
    val t = Engine.get.currentAsyncThread
    that.foreach(t.addSoonHandle(_))
  }
}
