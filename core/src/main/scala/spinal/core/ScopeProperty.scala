package spinal.core

import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.Stack

trait ScopeProperty[T]{
  def stack = GlobalData.get.scopeProperties.getOrElseUpdate(this.asInstanceOf[ScopeProperty[Any]],new Stack[Any]()).asInstanceOf[Stack[T]]
  def get = if(stack.isEmpty) default else stack.head
  def default : T

  def apply(value : T) = new {
    def apply[B](body : => B): B ={
      stack.push(value)
      val b = body
      stack.pop()
      b
    }
    def on[B](body : => B) = {
      stack.push(value)
      val b = body
      stack.pop()
      b
    }
  }
}

class ScopePropertyValue(dady : ScopeProperty[_ <: Any]){
  def on[B](body : => B) = {
    dady.stack.asInstanceOf[mutable.Stack[Any]].push(this)
    val b = body
    dady.stack.pop()
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