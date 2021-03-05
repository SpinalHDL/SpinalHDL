package spinal.core.fiber

import spinal.core._

import scala.collection.mutable.ArrayBuffer

class Unset
object Unset extends Unset

object Handle{
//  def apply[T] = new Handle[T]
  def apply[T]() = new Handle[T]
  def apply[T](value : T) = new Handle[T].load(value)


  implicit def keyImplicit[T](key : Handle[T]): T = key.get
  implicit def keyImplicit[T](key : Seq[Handle[T]]): Seq[T] = key.map(_.get)
  implicit def initImplicit[T](value : T) : Handle[T] = Handle(value) //TODO might need to remove that dangerous one ?
  implicit def initImplicit[T](value : Unset) : Handle[T] = Handle[T]
  implicit def initImplicit[T](value : Int) : Handle[BigInt] = Handle(value)
  implicit def initImplicit[T](value : Long) : Handle[BigInt] = Handle(value)
}

class Handle[T] extends Nameable {
  private var loaded = false
  private var value : T = null.asInstanceOf[T]
  private var wakeups = ArrayBuffer[() => Unit]()

  def isLoaded = loaded
  def get : T = {
    if(loaded) return value
    val t = AsyncThread.current
    val e = Engine.get
    t.waitOn = this
    wakeups += {() => e.wakeup(t)}
    e.sleep(t)
    value
  }
  def waitLoad : Unit = get

  def load(value : T) = {
    applyName(value)
    loaded = true
    this.value = value
    wakeups.foreach(_.apply())
    wakeups.clear()
    this
  }

  def load(value : Handle[T]): Unit ={
    val e = Engine.get
    val t = e.schedule{
      this.load(value.get)
    }
    t.willLoad = this
  }
  def unload(): Unit ={
    loaded = false
    value = null.asInstanceOf[T]
  }

  def applyName(value : Any) = value match {
    case value : Nameable => value.setCompositeName(this, Nameable.DATAMODEL_WEAK)
    case l : Seq[_] if l.nonEmpty && l.head.isInstanceOf[Nameable] => for((e,i) <- l.zipWithIndex) e match {
      case e : Nameable => e.setCompositeName(this, i.toString, Nameable.DATAMODEL_WEAK)
      case _ =>
    }
    case _ =>
  }



  //TODO legacy API ?
  def produce[T](body : => T) = hardFork(body)

  def merge(that : Handle[T]): Unit ={
    var loaded = false
    hardFork{
      that.get
      if(!loaded) this.load(that.get)
      loaded = true
    }
    hardFork{
      this.get
      if(!loaded) that.load(this.get)
      loaded = true
    }
  }

  def derivatedFrom[T2](that : Handle[T2])(body : T2 => T) = hardFork(body)
  def derivate[T2](body : (T) => T2) = hardFork(body(value))
}