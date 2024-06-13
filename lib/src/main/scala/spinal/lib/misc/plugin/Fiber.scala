package spinal.lib.misc.plugin

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

class FiberPlugin extends Area with Hostable {
  this.setName(ClassName(this))
  def withPrefix(prefix: String) = setName(prefix + "_" + ClassName(this))

  def retains(that: Seq[Any]) = RetainerGroup(that)
  def retains(head: Any, tail: Any*) = RetainerGroup(head +: tail)


  var pluginEnabled = true
  var host : PluginHost = null
  val hostLock = Lock().retain()

  val subservices = ArrayBuffer[Any]()
  def addService[T](that : T) : T = {
    subservices += that
    that
  }

  def awaitBuild() = Fiber.awaitBuild()

  val lockables = mutable.LinkedHashSet[() => Lock]()
  def buildBefore(l : => Lock): Unit = {
    if (lockables.isEmpty) {
      spinal.core.fiber.Fiber.setupCallback {
        val things = lockables.map(_())
        things.foreach(_.retain())
        if (buildCount == 0) {
          during build{}
        }
      }
    }
    lockables += (() => l)
  }

  def setupRetain(l: => Lock): Unit = {
    spinal.core.fiber.Fiber.setupCallback {
      l.retain()
    }
  }

  var buildCount = 0


  override def setHost(h: PluginHost): Unit = {
    h.addService(this)
    subservices.foreach(h.addService)
    host = h
    if(!isNamed){
      this.setName(ClassName(this))
    }
    hostLock.release()
  }

  def during = new {
    def setup[T: ClassTag](body: => T): Handle[T] = spinal.core.fiber.Fiber setup {
      pluginEnabled generate {
        hostLock.await()
        val onCreate = OnCreateStack.getOrElse(null)
        host.rework {
          OnCreateStack.set(onCreate)
          body
        }
      }
    }

    def build[T: ClassTag](body: => T): Handle[T] = {
      buildCount += 1
      spinal.core.fiber.Fiber build {
        pluginEnabled generate {
          hostLock.await()
          val onCreate = OnCreateStack.getOrElse(null)
          val ret = host.rework{
            OnCreateStack.set(onCreate)
            body
          }
          buildCount -= 1
          if (buildCount == 0) {
            lockables.foreach(_().release())
          }
          ret
        }
      }
    }
  }
}