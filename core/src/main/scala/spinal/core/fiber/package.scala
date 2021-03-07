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
    t.willLoadHandles += ret
    ret
  }

  def soon(that : Handle[_]*)  : Unit = {
    Engine.get.currentAsyncThread.willLoadHandles ++= that
  }
}
