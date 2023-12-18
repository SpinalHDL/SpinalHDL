package spinal.core.fiber

import spinal.core.{Area, Nameable, assert}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Lock() extends Handle[Int]{
  load(0)
  private var retains = 0
  def retain() : this.type = {
    retains += 1
    this.unload()
    this
  }
  def release() : Unit = {
    assert(retains > 0)
    retains -= 1
    if(retains == 0) {
      this.load(0)
    }
  }
}


trait Lockable extends Area{
  val lock = spinal.core.fiber.Lock()
  def retain() = lock.retain()
  def release() = lock.release()
  def await() = lock.await()
}

class RetainerHold extends Handle[Unit] {
  def release() = load()
  def done() = load()
}


case class Retainer() {
  val retainers = mutable.Queue[RetainerHold]()
  def apply() : RetainerHold = {
    val rh = new RetainerHold()
    retainers += rh
    rh
  }


  def await(): Unit = {
    while (retainers.nonEmpty) {
      retainers.dequeue().await()
    }
  }
}

class RetainerGroup(args : Seq[Nameable]) extends Area{
  val things = args.map{
    case r : Retainer => r()
    case r : Lock => r.retain()
  }
  def release() = {
    things.foreach{
      case r : RetainerHold => r.release()
      case r : Lock => r.release()
    }
  }
}