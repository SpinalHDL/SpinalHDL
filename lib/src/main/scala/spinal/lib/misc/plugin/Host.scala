package spinal.lib.misc.plugin

import spinal.core._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.collection.Seq

object Plugin {
  def list[T: ClassTag]: Seq[T] = PluginHost.get.list[T]
  def apply[T: ClassTag]: T = PluginHost.get.apply[T]
}

object PluginHost extends ScopeProperty[PluginHost] {
  def on[T](body: => T) = this (new PluginHost).on(body)
}

trait Hostable{
  def setHost(h: PluginHost) : Unit
}

class PluginHost {
  val services = ArrayBuffer[Any]()
  val _context = ScopeProperty.capture()

  def rework[T](body: => T): T = {
    val oldContext = ScopeProperty.captureNoClone()
    _context.restoreCloned()
    val b = PluginHost(this) {
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

  def get[T: ClassTag]: Option[T] = {
    val filtered = list[T]
    filtered.length match {
      case 0 => None
      case 1 => Some(filtered.head)
      case _ => throw new Exception(s"Found multiple instances of ${classTag[T].runtimeClass.getName}")
    }
  }

  def find[T: ClassTag](filter : T => Boolean) = list[T].find(filter).get
}


