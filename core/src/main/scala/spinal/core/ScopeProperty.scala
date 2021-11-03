package spinal.core

import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}

object ScopeProperty {
  def apply[T] = new ScopeProperty[T]()
  def apply[T](default : T) = {
    val sp = new ScopeProperty[T]()
    sp.set(default)
    sp
  }
  val it = new ThreadLocal[mutable.LinkedHashMap[ScopeProperty[Any], Any]]
  def get : mutable.LinkedHashMap[ScopeProperty[Any], Any] = {
    val v = it.get()
    if(v != null) return v
    it.set(mutable.LinkedHashMap[ScopeProperty[Any], Any]())
    it.get()
  }

  case class Capture(context : mutable.LinkedHashMap[ScopeProperty[Any], Any]){
    def restore(): Unit ={
      it.set(context)
    }
  }

  def capture(): Capture ={
    Capture(context = get.clone())
  }
  def captureNoClone(): Capture ={
    Capture(context = get)
  }

  def sandbox[T](body : => T) = {
    val spc = ScopeProperty.capture()
    try{ body } finally { spc.restore() }
  }

  implicit def toValue[T](scopeProperty: ScopeProperty[T]) = scopeProperty.get
  implicit def toBits(scopeProperty: ScopeProperty[Int]) = new {
    def bits = BitCount(scopeProperty.get)
  }
}


class ScopeProperty[T]  {
  def get : T = ScopeProperty.get.get(this.asInstanceOf[ScopeProperty[Any]]) match {
    case Some(x) => {
      x.asInstanceOf[T]
    }
    case _ => {
      val v = default
      this.set(v)
      v
    }
  }

  def set(v : T) = new {
    val property = ScopeProperty.get
    val previous = property.get(ScopeProperty.this.asInstanceOf[ScopeProperty[Any]])
    property.update(ScopeProperty.this.asInstanceOf[ScopeProperty[Any]], v)
    def restore() = {
      previous match {
        case None =>  ScopeProperty.get.remove(ScopeProperty.this.asInstanceOf[ScopeProperty[Any]])
        case Some(x) => ScopeProperty.get.update(ScopeProperty.this.asInstanceOf[ScopeProperty[Any]], x)
      }
    }
  }
//  def push(v : T) = stack.push(v)
//  def pop() = {
//    stack.pop()
//    if(stack.isEmpty){
//      ScopeProperty.get -= this.asInstanceOf[ScopeProperty[Any]]
//    }
//  }

//  def headOption = if(stack.isEmpty) None else Some(get)
  def isEmpty = ScopeProperty.get.get(this.asInstanceOf[ScopeProperty[Any]]) match {
    case Some(x) => false
    case _ => true
  }
//  def nonEmpty = stack.nonEmpty

  def default : T = {
    this match {
      case n : Nameable => println("On $n")
      case _ =>
    }
    throw new Exception(s"ScopeProperty ${this} isn't set")
  }
//  def setDefault(x: T): Unit = _default = x

  final def _default = ??? //I changed a bit the API, now instead of var _default, you can override def default. Also instead of setDefault, you can directly use "set"

  def apply(value : T) = new {
    def apply[B](body : => B): B ={
      val previous = ScopeProperty.this.get
      set(value)
      val b = body
      set(previous.asInstanceOf[T])
      b
    }
    def on[B](body : => B) = {
      val previous = ScopeProperty.this.get
      set(value)
      val b = body
      set(previous.asInstanceOf[T])
      b
    }
  }
}

class ScopePropertyValue(val dady : ScopeProperty[_ <: Any]){
  def on[B](body : => B) = {
    val previous = dady.get
    dady.asInstanceOf[ScopeProperty[Any]].set(this)
    val b = body
    dady.asInstanceOf[ScopeProperty[Any]].set(previous)
    b
  }
}

//trait ScopePropertyApplicator extends PostInitCallback{
//  println("PUSH")
//
//  override def postInitCallback(): ScopePropertyApplicator.this.type = {
//    println("POP")
//    this
//  }
//}
//
//trait ScopePropertyApplicator2 extends PostInitCallback{
//  println("PUSH2")
//
//  override def postInitCallback(): ScopePropertyApplicator2.this.type = {
//    println("POP2")
//    this
//  }
//}