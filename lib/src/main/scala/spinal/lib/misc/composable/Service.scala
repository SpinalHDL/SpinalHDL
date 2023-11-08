package spinal.lib.misc.composable

import spinal.core._
import spinal.core.fiber._
import scala.reflect.{ClassTag, classTag}

import scala.collection.mutable.ArrayBuffer



object Service {
  def list[T: ClassTag]: Seq[T] = ServiceHost.get.list[T]
  def apply[T: ClassTag]: T = ServiceHost.get.apply[T]
}

object ServiceHost extends ScopeProperty[ServiceHost] {
  def on[T](body: => T) = this (new ServiceHost).on(body)
}

class ServiceHost {
  val services = ArrayBuffer[Any]()

  val _context = ScopeProperty.capture()

  def rework[T](body: => T): T = {
    val oldContext = ScopeProperty.captureNoClone()
    _context.restoreCloned()
    val b = ServiceHost(this) {
      body
    }
    oldContext.restore()
    b
  }

  def add(that: Any): Unit = services += that

  def list[T: ClassTag]: Seq[T] = {
    val clazz = classTag[T].runtimeClass
    val filtered = ArrayBuffer[Any]()
    services.collect { case t: T => t }
  }

  def apply[T: ClassTag]: T = {
    val filtered = list[T]
    filtered.length match {
      case 0 => throw new Exception(s"Can't find the service ${classTag[T].runtimeClass.getName}")
      case 1 => filtered.head
      case _ => throw new Exception(s"Found multiple instances of ${classTag[T].runtimeClass.getName}")
    }
  }
}


