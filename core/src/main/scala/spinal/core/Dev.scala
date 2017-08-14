package spinal.core


import scala.collection.mutable.ArrayBuffer


case class DslContext(clockDomain: ClockDomain, component: Component, conditionalContext: ConditionalContext)


trait Expression{
  def foreachExpression(func : (Expression) => Unit) : Unit
  def walkExpression(func : (Expression) => Unit) : Unit = {
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }


//  def walkLeafExpression(func : (Expression) => Unit) : Unit = {
//    foreachExpression(e => {
//      e.walkLeafExpression(func)
//    })
//  }


}

case class RefExpression(source : Nameable) extends Expression{
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
class WhenStatement(val cond : Expression,var  whenTrue : Statement,var  whenFalse : Statement) extends Statement{
  def foreachStatements(func : (Statement) => Unit) = {
    if(whenTrue != null) func(whenTrue)
    if(whenFalse != null) func(whenFalse)
  }

  def foreachExpression(func : (Expression) => Unit) = {
    func(cond)
  }

  def addTrue(that : Statement) : Unit = whenTrue match {
    case null => whenTrue = that
    case block : BlockStatement => block.add(that)
    case _ =>
      val block = new BlockStatement
      block.add(whenTrue)
      block.add(that)
      whenTrue = block
  }

  def addFalse(that : Statement) : Unit = whenFalse match {
    case null => whenFalse = that
    case block : BlockStatement => block.add(that)
    case _ =>
      val block = new BlockStatement
      block.add(whenFalse)
      block.add(that)
      whenTrue = block
  }
}

class BlockStatement() extends Statement{
  val content = ArrayBuffer[Statement]()

  def foreachStatements(func : (Statement) => Unit) = {
    content.foreach(func)
  }
  def foreachExpression(func : (Expression) => Unit) : Unit = Unit

  def add(that : Statement) : Unit = content += that
}

//trait StatementLocation{
//  def add(s : Statement)
//}
//
//class BlockStatementLocation(block : BlockStatement) extends StatementLocation{
//  override def add(s: Statement): Unit = block.content += s
//}
//
//class WhenTrueStatementLocation(w : WhenStatement) extends StatementLocation{
//  override def add(s: Statement): Unit = w.whenTrue match {
//    case null => w.whenTrue = s
//    case block : BlockStatement => block.
//  }
//    if(w.whenTrue == null){
//    w.whenTrue = s
//  } else if()
//}