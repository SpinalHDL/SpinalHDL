package spinal.lib.generator

import spinal.core._
import spinal.core.fiber._
import spinal.core.internals.classNameOf
import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}


//TODO old API
object Dependable{
  def apply[T](d : Handle[_]*)(body : => T) : Handle[T] = {
    Handle{
      d.foreach(_.get)
      body
    }
  }
}



class Generator extends Area { //TODO TagContainer
  //TODO old API

  val initialClockDomain = ClockDomain.currentHandle

  val generatorLock = new Lock
  val generatorDone = new Lock

  val add = new {
    def task[T](body : => T) = {
      generatorDone.retain()
      hardFork{
        soon(generatorDone)
        generatorLock.get;
        val v = body
        generatorDone.release()
        v
      }
    }
  }
  def produce[T](body : => T) = hardFork{generatorLock.get; body}
//  def createDependency[T](that : Handle[T]) = {}
  def createDependency[T]() = {
    val h = Handle[T]
    dependencies += h
    h
  }


  def dts[T <: Nameable](node : Handle[T])(value : => String) = add task {
    node.produce(Component.current.addTag(new Dts(node, value)))
    node
  }

  val dependencies = new {
    def += [T <: Generator](that : T) : Unit = {
      +=(that.generatorDone)
    }

    def += [T](that : Handle[T]) : Unit = {
      generatorLock.retain()
      val t = hardFork {
        soon(generatorLock)
        that.get
        generatorLock.release()
      }
      t.setCompositeName(Generator.this, "unlock")
    }
    def ++= (that : Seq[Handle[_]]) : Unit = that.foreach(+=(_))
  }

//  val products = ArrayBuffer[Handle[_]]()
  val products = new {
    def += (that : Handle[_]) = {}
    def ++= (that : Seq[Handle[_]]) = {}
  }

  val tags = new {
    def += (that : SpinalTag) : Unit = hardFork(Component.current.addTag(that))
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

  def apply[T](body : => T): T = this.rework(body)

//  def toComponent(name : String = null) = new GeneratorComponent()

  def product[T] = {
    val h = Handle[T]
    this.generatorLock.soon(h)
    h
  }
}

object GeneratorComponent{
  implicit def toGenerator[T <: Generator](g : GeneratorComponent[T]) = g.body

  def apply[T <: Generator](generatorLamda : => T, name : String = null) = new GeneratorComponent(generatorLamda, name)
}


class GeneratorComponent[T](gen : => T) extends Component {
  val body : T = gen
  this.setDefinitionName(if(name == null) classNameOf(this) else name)
}

case class Lock() extends Handle[Int]{
  load(0)
  private var retains = 0
  def retain() : Unit = {
    retains += 1
    this.unload()
  }
  def release() : Unit = {
    assert(retains > 0)
    retains -= 1
    if(retains == 0) {
      this.load(0)
    }
  }
}
