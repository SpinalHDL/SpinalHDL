package spinal.lib.misc.service

import spinal.core._
import spinal.core.fiber._

import scala.collection.mutable.ArrayBuffer

class FiberPlugin extends Area with Lockable with Hostable {
  this.setName(ClassName(this))

  def withPrefix(prefix: String) = setName(prefix + "_" + getName())

  var pluginEnabled = true
  var host : ServiceHost = null

  val subservices = ArrayBuffer[Any]()

  override def setHost(h: ServiceHost): Unit = {
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

    def build[T](body: => T): Handle[T] = spinal.core.fiber.Fiber build {
      pluginEnabled generate {
        lock.await()
        host.rework(body)
      }
    }
  }

  override def valCallbackRec(obj: Any, name: String) = {
    obj match {
      case obj : NamedType[_] => obj.setName(name)
      case _ => super.valCallbackRec(obj, name)
    }
  }
}