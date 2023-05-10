package spinal.core

import spinal.sim.{JvmThread, JvmThreadUnschedule}

package object fiber {
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

  def soon(that : Handle[_]*)  : Unit = {
    val t = Engine.get.currentAsyncThread
    that.foreach(t.addSoonHandle(_))
  }
}
