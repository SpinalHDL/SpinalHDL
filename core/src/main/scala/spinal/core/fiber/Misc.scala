package spinal.core.fiber

import spinal.core.{Area, assert}

import scala.collection.mutable

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

case class Retainer() {
  val retainers = mutable.Queue[Retainer]()
  def apply() : Retainer = new Retainer()

  class Retainer extends Handle[Unit] {
    retainers += this

    def release() = load()
    def done() = load()
  }

  def await(): Unit = {
    while (retainers.nonEmpty) {
      retainers.dequeue().await()
    }
  }
}