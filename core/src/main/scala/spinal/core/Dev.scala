package spinal.core


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class DslContext(clockDomain: ClockDomain, component: Component, scope: ScopeStatement)


trait BaseNode {
    private[core] var algoId = 0
}

trait NameableNode extends BaseNode with Nameable{
  def rootScopeStatement : ScopeStatement = dslContext.scope
  val statements = mutable.ListBuffer[AssignementStatement]() //TODO IR ! linkedlist  hard

  def hasOnlyOneStatement = statements.length == 1 //TODO faster
  def headStatement = statements.head
  def append(that : AssignementStatement) : this.type = {
    statements += that
    this
  }

  def prepend(that : AssignementStatement) : this.type = {
    statements.prepend(that)
    this
  }

  def foreachStatements(func : (AssignementStatement) => Unit) = {
    statements.foreach(func)
  }

}

trait Expression extends BaseNode{
  def opName : String
  def foreachExpression(func : (Expression) => Unit) : Unit
  def walkExpression(func : (Expression) => Unit) : Unit = {
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }
}

case class RefExpression(source : NameableNode) extends Expression{
  def opName : String ="(x)"
  def foreachExpression(func : (Expression) => Unit) : Unit = {

  }

  def foreachLeafExpression(func : (Expression) => Unit) : Unit = {
    func(this)
  }
}



trait Statement{
  var parentScope : ScopeStatement = null
  def rootScopeStatement : ScopeStatement
//  def isConditionalStatement : Boolean
//  var previous, next : Statement = null

  def foreachStatements(func : (Statement) => Unit)
  def walkStatements(func : (Statement) => Unit): Unit ={
    foreachStatements(s => {
      func(s)
      s.walkStatements(func)
    })
  }
  def foreachExpression(func : (Expression) => Unit) : Unit
  def walkExpression(func : (Expression) => Unit): Unit ={
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }

  def walkLeafStatements(func : (LeafStatement) => Unit): Unit ={
    foreachStatements(s => {
      s match {
        case s : LeafStatement => func(s)
        case _ =>  s.walkLeafStatements(func)
      }
    })
  }

  def walkParentTreeStatements(func : (TreeStatement) => Unit) : Unit = {
    if(parentScope.parentStatement != null){
      func(parentScope.parentStatement)
      parentScope.parentStatement.walkParentTreeStatements(func)
    }
  }
}
trait LeafStatement extends Statement
trait TreeStatement extends Statement

//trait AssignementStatementTarget {
//  private [core] def nameableNode : NameableNode
//}

trait AssignementKind
object AssignementKind{
  object DATA extends AssignementKind
  object INIT extends AssignementKind
}

case class AssignementStatement(val target : NameableNode ,val  source : Expression, val kind : AssignementKind) extends LeafStatement{
  if(target != null) target.append(this)
  override def rootScopeStatement = target.rootScopeStatement
//  override def isConditionalStatement: Boolean = false
  def foreachStatements(func : (Statement) => Unit) = Unit
  def foreachExpression(func : (Expression) => Unit) : Unit = func(source)
}
class WhenStatement(val cond : Expression) extends TreeStatement{
  val whenTrue, whenFalse = new ScopeStatement(this)
  override def rootScopeStatement: ScopeStatement = ??? //doesn't make sense
//  override def isConditionalStatement: Boolean = true

  def foreachStatements(func : (Statement) => Unit) = {
    whenTrue.foreachStatements(func)
    whenFalse.foreachStatements(func)
  }

  def foreachExpression(func : (Expression) => Unit) = {
    func(cond)
  }
}

class ScopeStatement(var parentStatement : TreeStatement){
  val content = mutable.ListBuffer[Statement]() //TODO IR ! linkedlist  hard

  def sizeIsOne = content.length == 1 //TODO faster
  def head = content.head
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

  def walkStatements(func : (Statement) => Unit): Unit ={
    foreachStatements(s => {
      func(s)
      s.walkStatements(func)
    })
  }

  def walkLeafStatements(func : (LeafStatement) => Unit): Unit ={
    foreachStatements(s => {
      s match {
        case s : LeafStatement => func(s)
        case _ =>  s.walkLeafStatements(func)
      }
    })
  }

  def walkExpression(func : (Expression) => Unit): Unit ={
    foreachStatements(s => {
      s.walkExpression(func)
    })
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


object GraphUtils{
  def walkAllComponents(root : Component, func : Component => Unit) : Unit = {
    func(root)
    root.children.foreach(walkAllComponents(_, func))
  }
}