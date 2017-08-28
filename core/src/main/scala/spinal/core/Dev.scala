package spinal.core


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class DslContext(clockDomain: ClockDomain, component: Component, scope: ScopeStatement)


trait BaseNode {
    private[core] var algoId = 0
}

trait NameableNode extends BaseNode with Nameable

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

class RefExpression(val source : Nameable) extends Expression{
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

trait AssignementStatementTarget {
  private [core] def nameableNode : NameableNode
}
class AssignementStatement(val target : AssignementStatementTarget ,val  source : Expression) extends LeafStatement{
  override def rootScopeStatement = target.nameableNode.dslContext.scope
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
  def splitByScope(nameables : TraversableOnce[Nameable]): mutable.HashMap[ScopeStatement,mutable.HashSet[Nameable]] = {
    val dic = mutable.HashMap[ScopeStatement,mutable.HashSet[Nameable]]()
    for(n <- nameables){
      dic.getOrElseUpdate(n.dslContext.scope, new mutable.HashSet[Nameable]) += n
    }
    dic
  }
}