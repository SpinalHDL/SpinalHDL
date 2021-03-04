package spinal.core.fiber

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
    applyName(value)
    loaded = true
    this.value = value
    wakeups.foreach(_.apply())
    wakeups.clear()
  }

  def applyName(value : Any) = value match {
    case value : Nameable => value.setCompositeName(this, Nameable.DATAMODEL_WEAK)
    case l : Seq[_] if l.nonEmpty && l.head.isInstanceOf[Nameable] => for((e,i) <- l.zipWithIndex) e match {
      case e : Nameable => e.setCompositeName(this, i.toString, Nameable.DATAMODEL_WEAK)
      case _ =>
    }
    case _ =>
  }
}