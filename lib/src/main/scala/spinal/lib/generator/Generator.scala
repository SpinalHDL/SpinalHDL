package spinal.lib.generator

import spinal.core._
import spinal.core.fiber._
import spinal.core.internals.classNameOf
import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}

class Generator extends Area with TagContainer{ //TODO TagContainer

  //TODO old API
  val add = new {
    def task[T](body : => T) = produce(body)
  }
  def produce[T](body : => T) = hardFork(body)
//  def createDependency[T](that : Handle[T]) = {}
  def createDependency[T]() = Handle[T]

  def export[T](h : Handle[T]) = {
    h.produce(this.tags += new Export(h.getName, h.get))
    h
  }
  def dts[T <: Nameable](node : Handle[T])(value : => String) = add task {
    node.produce(this.tags += new Dts(node, value))
    node
  }

  val dependencies = new {
    def += [T](that : Handle[T]) = {}
    def ++= (that : Seq[Handle[_]]) = {}
  }

  val products = new {
    def += [T](that : Handle[T]) = {}
    def ++= (that : Seq[Handle[_]]) = {}
  }

  def produceIo[T <: Data](body : => T) : Handle[T] = {
    val h = Handle[T]
    products += h
//    Generator.stack.head.add {
      val p = new Generator()
      p.dependencies += this
      p.add task {h.load{
          val subIo = body
          val topIo = cloneOf(subIo).setPartialName(h, "", true)
          topIo.copyDirectionOf(subIo)
          for((s,t) <- (subIo.flatten, topIo.flatten).zipped if s.isAnalog) t.setAsAnalog()
          topIo <> subIo
          topIo
        }}
      p
//    }
    h
  }

  val _context = ScopeProperty.capture() //TODO not as heavy
  def apply[T](body : => T): T = {
    val oldContext = ScopeProperty.capture() //TODO not as heavy
    _context.restore()
    val b = body
    oldContext.restore()
    b
  }

}

case class Lock() extends Handle{
  val wake = Handle[Int](0)
  var retains = 0
  def retain() : Unit = {
    retains += 1
    assert(!this.isLoaded)
  }
  def release() : Unit = {
    assert(retains > 0)
    retains -= 1
    if(retains == 0) wake.load(0)
  }

  def start() {
    hardFork {
      wake.waitLoad
      if (retain == 0) this.load(null) else {
        wake.unload()
        start()
      }
    }
  }
  start()
}
