package spinal.sim

import scala.collection.mutable


object SimManagerContext{
  val threadLocal = new ThreadLocal[SimManagerContext]
  def current = threadLocal.get()
  def reset() = threadLocal.set(new SimManagerContext)
}

class SimManagerContext{
  var thread : SimThread = null
  var manager : SimManager = null
  val masterThread = Thread.currentThread()
  private val keyValue = mutable.HashMap[Any,Any]()
  def get[T](key : Any) : T = keyValue.get(key).get.asInstanceOf[T]
  def set[T](key : Any, value : T) : Unit = keyValue.put(key, value)
  def contains(key : Any) = keyValue.contains(key)
}
