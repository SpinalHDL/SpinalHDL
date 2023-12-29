package spinal.lib.misc.plugin

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

class FiberPlugin extends Area with Hostable {
  this.setName(ClassName(this))

  def withPrefix(prefix: String) = setName(prefix + "_" + getName())

  def retains(that: Seq[Any]) = RetainerGroup(that)
  def retains(head: Any, tail: Any*) = RetainerGroup(head +: tail)


  var pluginEnabled = true
  var host : PluginHost = null

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
  }

  def during = new {
    def setup[T: ClassTag](body: => T): Handle[T] = spinal.core.fiber.Fiber setup {
      pluginEnabled generate {
        host.rework(body)
      }
    }

    def build[T: ClassTag](body: => T): Handle[T] = {
      buildCount += 1
      spinal.core.fiber.Fiber build {
        pluginEnabled generate {
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
}