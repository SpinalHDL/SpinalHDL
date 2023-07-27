package spinal.core

import spinal.idslplugin.PostInitCallback

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}

class ScopePropertyContext{
  val mutableMap = mutable.HashMap[ScopeProperty[Any], Any]() //For stuff changing often
  var immutableMap = scala.collection.immutable.HashMap[ScopeProperty[Any], Any]() //For things mostly static

  override def clone()  : ScopePropertyContext = {
    val c = new ScopePropertyContext
    c.mutableMap ++= mutableMap
    c.immutableMap = immutableMap
    c
  }

  def get(that : ScopeProperty[Any]) = if(that.storeAsMutable)
    mutableMap.get(that)
  else
    immutableMap.get(that)

  def remove(that : ScopeProperty[Any]) =  if(that.storeAsMutable)
    mutableMap.remove(that)
  else
    immutableMap = immutableMap - that

  def update(that : ScopeProperty[Any], value : Any) = if(that.storeAsMutable)
    mutableMap.update(that, value)
  else
    immutableMap = immutableMap + (that -> value)
}

object ScopeProperty {
  def apply[T] = new ScopeProperty[T]()
  def apply[T](defaultValue : T) : ScopeProperty[T] = {
    val sp = new ScopeProperty[T](){
      override def default = defaultValue
    }
    sp
  }
  val it = new ThreadLocal[ScopePropertyContext]
  def get : ScopePropertyContext = {
    val v = it.get()
    if(v != null) return v
    it.set(new ScopePropertyContext)
    it.get()
  }

  case class Capture(context : ScopePropertyContext){
    def restore(): Unit ={
      it.set(context)
    }
    def restoreCloned(): Unit ={
      it.set(context.clone())
    }

    def get[T](sp: ScopeProperty[T]) : T = context.get(sp.asInstanceOf[ScopeProperty[Any]]).asInstanceOf[T]
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

  implicit def toValue[T](scopeProperty: ScopeProperty[T]): T = scopeProperty.get
  implicit def toBits(scopeProperty: ScopeProperty[Int]) : ToBitsPimper= new ToBitsPimper(scopeProperty)
}

class ToBitsPimper(scopeProperty: ScopeProperty[Int]) {
  def bits = BitCount(scopeProperty.get)
}

class ScopeProperty[T]  {
  var storeAsMutable = false
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

  class SetReturn(v : T){
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
  def set(v : T) = new SetReturn(v)
  def clear() = {
    ScopeProperty.get.remove(ScopeProperty.this.asInstanceOf[ScopeProperty[Any]])
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

  def apply(value : T) = new ApplyClass(value)
  class ApplyClass(value : T)  {
    def apply[B](body : => B): B = {
      val wasSet = !ScopeProperty.this.isEmpty
      val previous = if(wasSet) ScopeProperty.this.get
      set(value)
      val b = body
      if(wasSet) set(previous.asInstanceOf[T]) else clear()
      b
    }
    def on[B](body : => B) = apply(body)
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