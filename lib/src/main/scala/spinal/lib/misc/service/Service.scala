package spinal.lib.misc.service

import spinal.core._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}

object Service {
  def list[T: ClassTag]: Seq[T] = ServiceHost.get.list[T]
  def apply[T: ClassTag]: T = ServiceHost.get.apply[T]
}

object ServiceHost extends ScopeProperty[ServiceHost] {
  def on[T](body: => T) = this (new ServiceHost).on(body)
}

trait Hostable{
  def setHost(h: ServiceHost) : Unit
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

  def addService(that: Any): Unit = services += that
  def asHostOf(hostables: Seq[Hostable]) : Unit = hostables.foreach(_.setHost(this))
  def asHostOf(head : Hostable, tail: Hostable*) : Unit = asHostOf(head +: tail)

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


