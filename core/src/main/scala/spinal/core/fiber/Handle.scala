package spinal.core.fiber

import spinal.core._

import scala.collection.mutable.ArrayBuffer

class Unset
object Unset extends Unset



object Handle {
  def apply[T]() = new Handle[T]
  def apply[T](value : => T) = hardFork(value)
  def sync[T](value : T) = {
    val h = new Handle[T]
    h.loaded = true
    h.value = value
    h
  }


  implicit def keyImplicit[T](key : Handle[T]): T = key.get
  implicit def keyImplicit[T](key : Seq[Handle[T]]): Seq[T] = key.map(_.get)
  implicit def initImplicit[T](value : T) : Handle[T] = Handle.sync(value) //TODO might need to remove that dangerous one ?
//  implicit def initImplicit[T](value : => T) : Handle[T] = hardFork(value)
  implicit def initImplicit[T](value : Unset) : Handle[T] = Handle[T]
  implicit def initImplicit[T](value : Int) : Handle[BigInt] = Handle(value)
  implicit def initImplicit[T](value : Long) : Handle[BigInt] = Handle(value)
  implicit def handleDataPimped[T <: Data](key : Handle[T]): DataPimper[T] = new DataPimper(key.get)

  var loadHandleAsync = false
}

/** Fiber synchronization primitive that can be used later to store a value of type `T`.
  *
  * It can be created with:
  *   - a type: `val a = Handle[Int]`) 
  *   - a lambda executed in a new task: `val a = Handle{otherHandle.get + 1}`
  * 
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/fiber.html#handle-t Fiber documentation]] 
  */
class Handle[T] extends Nameable with OverridedEqualsHashCode {
  @dontName private var loaded = false
  @dontName private var value : T = null.asInstanceOf[T]
  @dontName private var wakeups : ArrayBuffer[() => Unit] = null // ArrayBuffer[() => Unit]()
  @dontName var willBeLoadedBy : AsyncThread = null


  def await() = this.get

/** Specify in advance that the task creating this [[Handle]]'s value will use the provided handle(s)
  *
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/fiber.html#soon-handle Fiber documentation]]
  */
  def soon(that : Handle[_]*) : Unit = {
    if(willBeLoadedBy != null) {
      that.foreach(willBeLoadedBy.addSoonHandle)
    } else {
      Handle {  //TODO maybe can avoid that fork
        that.foreach(spinal.core.fiber.soon(_))
        this.await()
      }.setCompositeName(this, "soon")
    }
  }

  def isLoaded = loaded

  /** Return the [[Handle]]'s value (will block the task if that handle isn't loaded yet) */
  def get : T = {
    if(loaded) return value
    val t = AsyncThread.current
    val e = Engine.get
    t.waitOn = this
    if(wakeups == null) wakeups = ArrayBuffer[() => Unit]()
    wakeups += {() => e.wakeup(t)}
    e.sleep(t)
    value
  }
  def waitLoad : Unit = get

  /** Set the value of the [[Handle]] (will reschedule all tasks waiting on it) */
  def load(value : T) = {
    applyName(value)
    loaded = true
    this.value = value
    if(wakeups != null) {
      wakeups.foreach(_.apply())
      wakeups.clear()
    }
    this
  }

  /** Set the value of the [[Handle]] (will reschedule all tasks waiting on it) */
  def load(value : Handle[T]): Unit = { // TODO optimize if value is loaded already
    Handle.loadHandleAsync match{
      case false =>  load(value.get)
      case true => loadAsync(value.get)
    }
  }

  def loadAsync(body : => T) : Unit = {
    val t = Engine.get.schedule{
      this.load(body)
    }
    t.addSoonHandle(this)
  }

  def loadNothing(): Unit ={
    load(null.asInstanceOf[T])
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


  override def toString: String = getName("???") + (if(!loaded) "" else "=" + value.toString)

  def map[T2](body : (T) => T2) = hardFork(body(get))

  //TODO legacy API ?
  /** Generate a new [[Handle]] when this one is loaded
   * @see [[derivate()]]
   */
  def produce[T](body : => T) = hardFork(body)

//  def merge(that : Handle[T]): Unit ={
//    var loadDone = false
//    hardFork{
//      that.get
//      if(!loadDone) this.load(that.get)
//      loadDone = true
//    }.setCompositeName(this, "merge")
//    hardFork{
//      this.get
//      if(!loadDone) that.load(this.get)
//      loadDone = true
//    }.setCompositeName(that, "merge")
//  }

  def derivatedFrom[T2](that : Handle[T2])(body : T2 => T) : Unit = hardFork{soon(this); load(body(that.get))}

  /** Generate a new [[Handle]] when this one is loaded
   * 
   * The provided lambda take the loaded value as argument and return the derivate
   * [[Handle]] value.
   */
  def derivate[T2](body : (T) => T2) = hardFork{body(get)}
}