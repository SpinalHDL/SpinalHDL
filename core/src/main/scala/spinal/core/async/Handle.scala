package spinal.core.async

import spinal.core._

import scala.collection.mutable.ArrayBuffer

object Handle{
  def apply[T] = new Handle[T]
}

class Handle[T] extends Nameable {
  private var loaded = false
  private var value : T = null.asInstanceOf[T]
  private var wakeups = ArrayBuffer[() => Unit]()

  def get : T = {
    if(loaded) return value
    val t = AsyncThread.current
    val e = Engine.get
    t.waitOn = this
    wakeups += {() => e.wakeup(t)}
    e.sleep(t)
    value
  }

  def load(value : T) = {
    loaded = true
    this.value = value
    wakeups.foreach(_.apply())
    wakeups.clear()
  }
}