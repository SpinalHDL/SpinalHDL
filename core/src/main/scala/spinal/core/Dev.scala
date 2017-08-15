package spinal.core


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
  def foreachStatements(func : (Statement) => Unit)
  def walkStatements(func : (Statement) => Unit): Unit ={
    foreachStatements(s => {
      func(s)
      s.walkStatements(func)
    })
  }
  def foreachExpression(func : (Expression) => Unit) : Unit
}
class AssignementStatement(val target : Nameable,val  source : Expression) extends Statement{
  def foreachStatements(func : (Statement) => Unit) = Unit
  def foreachExpression(func : (Expression) => Unit) : Unit = func(source)
}
class WhenStatement(val cond : Expression) extends Statement{
  val whenTrue, whenFalse = new ScopeStatement

  def foreachStatements(func : (Statement) => Unit) = {
    if(whenTrue != null) func(whenTrue)
    if(whenFalse != null) func(whenFalse)
  }

  def foreachExpression(func : (Expression) => Unit) = {
    func(cond)
  }
}

class ScopeStatement() extends Statement{
  val content = ArrayBuffer[Statement]()

  def foreachStatements(func : (Statement) => Unit) = {
    content.foreach(func)
  }
  def foreachExpression(func : (Expression) => Unit) : Unit = Unit

  def add(that : Statement) : Unit = content += that
}
