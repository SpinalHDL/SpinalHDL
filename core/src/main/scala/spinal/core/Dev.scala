package spinal.core


import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class DslContext(clockDomain: ClockDomain, component: Component, scope: ScopeStatement)


trait BaseNode {
  private[core] var algoInt, algoIncrementale = 0
  private[core] def getClassIdentifier: String = this.getClass.getName.split('.').last.replace("$","")
}

//object NameableNode{
//  def unapply(that : NameableNode) : Option[Unit] = Some()
//}

trait NameableExpression extends Expression with Nameable with ContextUser{
  override def foreachExpression(func: (Expression) => Unit): Unit = {}
  override def remapExpressions(func: (Expression) => Expression): Unit = {}

  var nextNameable, previousNameable : NameableExpression = null
  def removeNameable() : Unit = {
    if(previousNameable != null){
      previousNameable.nextNameable = nextNameable
    } else {
      dslContext.component.headNameable = nextNameable
    }
    if(nextNameable != null){
      nextNameable.previousNameable = previousNameable
    } else {
      dslContext.component.lastNameable = previousNameable
    }
    previousNameable = null
    nextNameable = null

    //TODO IR remove statement from Nameable
  }
  
  
  def rootScopeStatement : ScopeStatement = dslContext.scope



  var headStatement, lastStatement : AssignementStatement = null
  def hasOnlyOneStatement = headStatement == lastStatement && headStatement != null
  def isEmpty = headStatement == null
  //  def sizeIsOne = head != null && head == last
  def prepend(that : AssignementStatement) : this.type = {
    if(headStatement != null){
      headStatement.previousNameableStatement = that
    } else {
      lastStatement = that
    }
    that.nextNameableStatement = headStatement
    that.previousNameableStatement = null

    headStatement = that

    this
  }

  def append(that : AssignementStatement) : this.type = {
    that.nextNameableStatement = null
    that.previousNameableStatement = lastStatement
    if(lastStatement != null){
      lastStatement.nextNameableStatement = that
    } else {
      headStatement = that
    }

    lastStatement = that
    this
  }

  def statementIterable = new Iterable[AssignementStatement] {
    override def iterator: Iterator[AssignementStatement] = statementIterator
  }

  def statementIterator = new Iterator[AssignementStatement] {
    var ptr = headStatement
    override def hasNext: Boolean = ptr != null

    override def next(): AssignementStatement = {
      val ret = ptr
      ptr = ret.nextNameableStatement
      ret
    }
  }


  def foreachStatements(func : (AssignementStatement) => Unit) = {
    var ptr = headStatement
    while(ptr != null){
      val current = ptr
      ptr = ptr.nextNameableStatement
      func(current)
    }
  }
}


trait ExpressionContainer{
  def normalizeInputs: Unit = {}
  def remapExpressions(func : (Expression) => Expression) : Unit
  def remapDrivingExpressions(func : (Expression) => Expression) : Unit = remapExpressions(func)
  def foreachExpression(func : (Expression) => Unit) : Unit
  def foreachDrivingExpression(func : (Expression) => Unit) : Unit = foreachExpression(func)
  def walkExpression(func : (Expression) => Unit) : Unit = {
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }

  def walkNameableExpression(func : (NameableExpression) => Unit) = walkExpression(e => e match {
    case e : NameableExpression => func(e)
    case _ =>
  })

  def walkDrivingExpressions(func : (Expression) => Unit) : Unit = {
    foreachDrivingExpression(e => {
      func(e)
      e.walkDrivingExpressions(func)
    })
  }
  def walkRemapExpressions(func : (Expression) => Expression) : Unit = {
    remapExpressions(func)
    foreachExpression(e => {
      e.walkRemapExpressions(func)
    })
  }
  def walkRemapDrivingExpressions(func : (Expression) => Expression) : Unit = {
    remapDrivingExpressions(func)
    foreachDrivingExpression(e => {
      e.walkRemapDrivingExpressions(func)
    })
  }
}

trait Expression extends BaseNode with ExpressionContainer{
  def opName : String
  def simplifyNode: Expression = this
}


//TODO IR check same scope
object Statement{
  def isFullToFullStatement(s : Statement) = s match {
    case  AssignementStatement(a : NameableExpression,b : NameableExpression) => true
    case _ => false
  }
  def isSomethingToFullStatement(s : Statement) = s match {
    case  AssignementStatement(a : NameableExpression,_) => true
    case _ => false
  }
}

trait Statement extends ExpressionContainer with ScalaLocated with BaseNode{
  var parentScope : ScopeStatement = null
  var previousScopeStatement, nextScopeStatement : Statement = null
  def rootScopeStatement: ScopeStatement = if(parentScope.parentStatement != null) parentScope.parentStatement.rootScopeStatement else parentScope
//  def isConditionalStatement : Boolean

//  def removeStatement() : Unit = parentScope.content -= this
  def removeStatement() : Unit = {
    if(previousScopeStatement != null){
      previousScopeStatement.nextScopeStatement = nextScopeStatement
    } else {
      parentScope.head = nextScopeStatement
    }
    if(nextScopeStatement != null){
      nextScopeStatement.previousScopeStatement = previousScopeStatement
    } else {
      parentScope.last = previousScopeStatement
    }
    previousScopeStatement = null
    nextScopeStatement = null
    parentScope = null

  //TODO IR remove statement from Nameable
  }

  def foreachStatements(func : (Statement) => Unit)
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

  def walkParentTreeStatements(func : (TreeStatement) => Unit) : Unit = {
    if(parentScope.parentStatement != null){
      func(parentScope.parentStatement)
      parentScope.parentStatement.walkParentTreeStatements(func)
    }
  }
}
trait LeafStatement extends Statement{
  final override def foreachStatements(func: (Statement) => Unit): Unit = {}

}
trait TreeStatement extends Statement

//trait AssignementStatementTarget {
//  private [core] def nameableNode : NameableNode
//}


object AssignementStatement{
  def unapply(x : AssignementStatement) : Option[(Expression, Expression)] = Some(x.target, x.source)
}

abstract class AssignementStatement extends LeafStatement{
  var target, source : Expression = null
  var previousNameableStatement, nextNameableStatement : AssignementStatement = null
  override def rootScopeStatement = finalTarget.rootScopeStatement
  def finalTarget : BaseType = target match{
    case n : BaseType => n
    case a : AssignementExpression => a.finalTarget
  }

  def foreachExpression(func : (Expression) => Unit) : Unit = {
    func(target)
    func(source)
  }

  override def foreachDrivingExpression(func : (Expression) => Unit) : Unit = {
    target match {
      case ref : NameableExpression =>
      case a : AssignementExpression => a.foreachDrivingExpression(func)
    }
    func(source)
  }


  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    target match {
      case ref : NameableExpression =>
      case a : AssignementExpression => a.remapDrivingExpressions(func)
    }
    source = func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    target = func(target)
    source = func(source)
  }

  override def removeStatement(): Unit = {
    super.removeStatement()

    //Remove from BaseType
    if(previousNameableStatement != null){
      previousNameableStatement.nextNameableStatement = nextNameableStatement
    } else {
      finalTarget.headStatement = nextNameableStatement
    }
    if(nextNameableStatement != null){
      nextNameableStatement.previousNameableStatement = previousNameableStatement
    } else {
      finalTarget.lastStatement = previousNameableStatement
    }
    previousNameableStatement = null
    nextNameableStatement = null
  }
}

object DataAssignementStatement{
  def apply(target : Expression, source : Expression) = {
    val ret = new DataAssignementStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.append(ret)
    ret
  }
}

class DataAssignementStatement extends AssignementStatement{

}

object InitAssignementStatement{
  def apply(target : Expression, source : Expression) = {
    val ret = new InitAssignementStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.append(ret)
    ret
  }
}

class InitAssignementStatement extends AssignementStatement{

}

class WhenStatement(var cond : Expression) extends TreeStatement{
  val whenTrue, whenFalse = new ScopeStatement(this)

//  override def isConditionalStatement: Boolean = true

  override def normalizeInputs: Unit = {}

  def foreachStatements(func : (Statement) => Unit) = {
    whenTrue.foreachStatements(func)
    whenFalse.foreachStatements(func)
  }

  def foreachExpression(func : (Expression) => Unit) = {
    func(cond)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    cond = func(cond)
  }
}


class SwitchStatementElement(var keys : ArrayBuffer[Expression],var scopeStatement: ScopeStatement)
class SwitchStatement(var value : Expression) extends TreeStatement{
  val elements = ArrayBuffer[SwitchStatementElement]()
  var defaultScope : ScopeStatement = null

  override def foreachStatements(func: (Statement) => Unit): Unit = {
    elements.foreach(x => x.scopeStatement.foreachStatements(func))
    defaultScope.foreachStatements(func)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    value = func(value)
    elements.foreach(x => {
      for(i <- 0 until x.keys.length){
        x.keys(i) = func(x.keys(i))
      }
    })
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(value)
    elements.foreach(x => x.keys.foreach(func))
  }
}

class ScopeStatement(var parentStatement : TreeStatement)/* extends ExpressionContainer*/{
//  val content = mutable.ListBuffer[Statement]() //TODO IR ! linkedlist  hard
//
//  def sizeIsOne = content.length == 1 //TODO faster
//  def head = content.head
//  def append(that : Statement) : this.type = {
//    content += that
//    this
//  }
//
//  def prepend(that : Statement) : this.type = {
//    content.prepend(that)
//    this
//  }
//
//  def foreachStatements(func : (Statement) => Unit) = {
//    content.foreach(func)
//  }
  var head, last : Statement = null
  def isEmpty = head == null
  def nonEmpty = head != null
//  def sizeIsOne = head != null && head == last
  def prepend(that : Statement) : this.type = {
    if(head != null){
      head.previousScopeStatement = that
    } else {
      last = that
    }
    that.nextScopeStatement = head
    that.previousScopeStatement = null

    head = that

    this
  }

  def append(that : Statement) : this.type = {
    that.nextScopeStatement = null
    that.previousScopeStatement = last
    if(last != null){
      last.nextScopeStatement = that
    } else {
      head = that
    }

    last = that
    this
  }

  def statementIterable = new Iterable[Statement] {
    override def iterator: Iterator[Statement] = statementIterator
  }

  def statementIterator = new Iterator[Statement] {
    var ptr = head
    override def hasNext: Boolean = ptr != null

    override def next(): Statement = {
      val ret = ptr
      ptr = ret.nextScopeStatement
      ret
    }
  }


  def foreachStatements(func : (Statement) => Unit) = {
    var ptr = head
    while(ptr != null){
      val current = ptr
      ptr = ptr.nextScopeStatement
      func(current)
    }
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

//  def walkExpression(func : (Expression) => Unit): Unit ={
//    walkStatements(s => {
//      s.walkExpression(func)
//    })
//  }
//  override def remapOwnedExpression(func: (Expression) => Expression): Unit = ???
//  override def foreachExpression(func: (Expression) => Unit): Unit = ???
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