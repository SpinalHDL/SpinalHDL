package spinal.core


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class DslContext(clockDomain: ClockDomain, component: Component, scope: ScopeStatement)


trait Expression{
  def foreachExpression(func : (Expression) => Unit) : Unit
  def walkExpression(func : (Expression) => Unit) : Unit = {
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }
}

class RefExpression(val source : Nameable) extends Expression{
  def foreachExpression(func : (Expression) => Unit) : Unit = {

  }

  def foreachLeafExpression(func : (Expression) => Unit) : Unit = {
    func(this)
  }
}

trait Statement{
//  var previous, next : Statement = null
  
  def foreachStatements(func : (Statement) => Unit)
  def walkStatements(func : (Statement) => Unit): Unit ={
    foreachStatements(s => {
      func(s)
      s.walkStatements(func)
    })
  }
  def foreachExpression(func : (Expression) => Unit) : Unit
}
trait AssignementStatementTarget {
  private [core] def nameable : Nameable
}
class AssignementStatement(val target : AssignementStatementTarget ,val  source : Expression) extends Statement{
  def foreachStatements(func : (Statement) => Unit) = Unit
  def foreachExpression(func : (Expression) => Unit) : Unit = func(source)
}
class WhenStatement(val cond : Expression) extends Statement{
  val whenTrue, whenFalse = new ScopeStatement

  def foreachStatements(func : (Statement) => Unit) = {
    whenTrue.foreachStatements(func)
    whenFalse.foreachStatements(func)
  }

  def foreachExpression(func : (Expression) => Unit) = {
    func(cond)
  }
}

class ScopeStatement(){
  val content = mutable.ListBuffer[Statement]() //TODO IR ! linkedlist  hard
  def append(that : Statement) : this.type = {
    content += that
    this
  }

  def prepend(that : Statement) : this.type = {
    content.prepend(that)
    this
  }

  def foreachStatements(func : (Statement) => Unit) = {
    content.foreach(func)
  }
}


//class ScopeStatement(){
//  var head, last : Statement = null
//
//
//  def prepend(that : Statement) : this.type = {
//    if(head != null){
//      head.previous = that
//    }
//    that.next = head
//    that.previous = null
//
//    head = that
//
//    this
//  }
//
//  def append(that : Statement) : this.type = {
//    that.next = null
//    if(last != null){
//      last.next = that
//      that.previous = last
//    }else{
//      that.previous = null
//    }
//
//    last = that
//    this
//  }
//
//  def foreachStatements(func : (Statement) => Unit) = {
//    var ptr = head
//    while(ptr != null){
//      func(ptr)
//      ptr = ptr.next
//    }
//  }
//}