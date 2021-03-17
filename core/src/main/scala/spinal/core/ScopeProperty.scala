package spinal.core

import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}

object ScopeProperty {
  val it = new ThreadLocal[mutable.LinkedHashMap[ScopeProperty[Any], Stack[Any]]]
  def get : mutable.LinkedHashMap[ScopeProperty[Any], Stack[Any]] = {
    val v = it.get()
    if(v != null) return v
    it.set(mutable.LinkedHashMap[ScopeProperty[Any], Stack[Any]]())
    it.get()
  }

  case class Capture(context : Seq[(ScopeProperty[Any], Seq[Any])]){
    def restore(): Unit ={
      get.clear()
      for(e <- context){
        get.update(e._1, Stack.concat(e._2))
      }
    }
  }

  def capture(): Capture ={
    Capture(context = get.toSeq.map(e => e._1 -> Seq.concat(e._2)))
  }

  def sandbox[T](body : => T) = {
    val spc = ScopeProperty.capture()
    try{ body } finally { spc.restore() }
  }
}


trait ScopeProperty[T]{
  private def stack = ScopeProperty.get.getOrElseUpdate(this.asInstanceOf[ScopeProperty[Any]],new Stack[Any]()).asInstanceOf[Stack[T]]
  def get = if(!ScopeProperty.get.contains(this.asInstanceOf[ScopeProperty[Any]]) || stack.isEmpty) default else stack.head
  protected[core] def push(v : T) = stack.push(v)
  protected[core] def pop() = {
    stack.pop()
    if(stack.isEmpty){
      ScopeProperty.get -= this.asInstanceOf[ScopeProperty[Any]]
    }
  }

  def headOption = if(stack.isEmpty) None else Some(get)
  def isEmpty = stack.isEmpty
  def nonEmpty = stack.nonEmpty

  protected var _default: T
  def default : T = _default
  def setDefault(x: T): Unit = _default = x

  def apply(value : T) = new {
    def apply[B](body : => B): B ={
      push(value)
      val b = body
      pop()
      b
    }
    def on[B](body : => B) = {
      push(value)
      val b = body
      pop()
      b
    }
  }
}

class ScopePropertyValue(val dady : ScopeProperty[_ <: Any]){
  def on[B](body : => B) = {
    dady.asInstanceOf[ScopeProperty[Any]].push(this)
    val b = body
    dady.pop()
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