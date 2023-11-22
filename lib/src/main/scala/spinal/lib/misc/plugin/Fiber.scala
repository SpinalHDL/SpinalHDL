package spinal.lib.misc.plugin

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FiberPlugin extends Area with Lockable with Hostable {
  this.setName(ClassName(this))

  def withPrefix(prefix: String) = setName(prefix + "_" + getName())

  var pluginEnabled = true
  var host : PluginHost = null

  val subservices = ArrayBuffer[Any]()


  val lockables = mutable.LinkedHashSet[() => Lockable]()
  def addLockable(l : => Lockable): Unit = {
    if (lockables.isEmpty) {
      spinal.core.fiber.Fiber.setupCallback {
        val things = lockables.map(_())
        things.foreach(_.retain())
      }
      spinal.core.fiber.Fiber.buildCallback {
        if (buildCount == 0) {
          val things = lockables.map(_())
          things.foreach(_.release())
        }
      }
    }
    lockables += (() => l)
  }

  def addRetain(l: => Lockable): Unit = {
    spinal.core.fiber.Fiber.setupCallback {
      l.retain()
    }
  }

  var buildCount = 0


  override def setHost(h: PluginHost): Unit = {
    h.addService(this)
    subservices.foreach(h.addService)
    host = h
  }

  def during = new {
    def setup[T](body: => T): Handle[T] = spinal.core.fiber.Fiber setup {
      pluginEnabled generate {
        host.rework(body)
      }
    }

    def build[T](body: => T): Handle[T] = {
      buildCount += 1
      spinal.core.fiber.Fiber build {
        pluginEnabled generate {
          lock.await()
          val ret = host.rework(body)
          buildCount -= 1
          if (buildCount == 0) {
            lockables.foreach(_().release())
          }
          ret
        }
      }
    }
  }

  override def valCallbackRec(obj: Any, name: String) = {
    obj match {
//      case obj : NamedType[_] => obj.setName(name)
      case _ => super.valCallbackRec(obj, name)
    }
  }
}