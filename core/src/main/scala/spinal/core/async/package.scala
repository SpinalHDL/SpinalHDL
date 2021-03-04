package spinal.core

import spinal.sim.{JvmThread, JvmThreadUnschedule}

package object async {
  def async[T](body : => T) : Handle[T] = {
    val ret = Handle[T]
    val e = Engine.get
    val t = e.schedule{ret.load(body)}
    t.willLoad = ret
    ret
  }
}
