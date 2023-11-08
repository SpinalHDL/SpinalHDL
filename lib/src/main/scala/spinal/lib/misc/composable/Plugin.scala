package spinal.lib.misc.composable

import spinal.core._
import spinal.core.fiber._
import spinal.lib.Stageable

import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable.ArrayBuffer

trait Lockable extends Area {
  val lock = spinal.core.fiber.Lock()
  def retain() = lock.retain()
  def release() = lock.release()
}

class Plugin extends Area with Lockable {
  this.setName(ClassName(this))

  def withPrefix(prefix: String) = setName(prefix + "_" + getName())

  var pluginEnabled = true
  var host : ServiceHost = null

  def setHost(h: ServiceHost): Unit = {
    h.add(this)
    host = h
  }

  def during = new {
    def setup[T](body: => T): Handle[T] = spinal.core.fiber.Fiber setup {
      pluginEnabled match {
        case false => null.asInstanceOf[T]
        case true => host.rework(body)
      }
    }

    def build[T](body: => T): Handle[T] = spinal.core.fiber.Fiber build {
      pluginEnabled match {
        case false => null.asInstanceOf[T]
        case true => {
          lock.await()
          host.rework(body)
        }
      }
    }
  }

  override def valCallbackRec(obj: Any, name: String) = {
    obj match {
      case obj : Stageable[_] => obj.setName(name)
      case _ => super.valCallbackRec(obj, name)
    }
  }
}