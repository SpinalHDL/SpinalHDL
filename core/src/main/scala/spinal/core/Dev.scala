package spinal.core


import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//TODO IR
// Add assert node into the clock pulling phase
//Clock pulling phase could be more systematic ?
case class DslContext(clockDomain: ClockDomain, component: Component, scope: ScopeStatement)


trait BaseNode {
  private[core] var algoInt, algoIncrementale = 0
  private[core] def getClassIdentifier: String = this.getClass.getName.split('.').last.replace("$","")
}

//object NameableNode{
//  def unapply(that : NameableNode) : Option[Unit] = Some()
//}


trait DoubleLinkedContainerElement[SC  <: DoubleLinkedContainer[SC, SE], SE <: DoubleLinkedContainerElement[SC, SE]]{
  def dlcParent : SC
  var dlceLast, dlceNext : SE = null.asInstanceOf[SE]
  def dlcRemove(): Unit = {
    //super.removeStatement()

    //Remove from BaseType
    if(dlceLast != null){
      dlceLast.dlceNext = dlceNext
    } else {
      dlcParent.dlcHead = dlceNext
    }
    if(dlceNext != null){
      dlceNext.dlceLast = dlceLast
    } else {
      dlcParent.dlcLast = dlceLast
    }
    dlceLast = null.asInstanceOf[SE]
    dlceNext = null.asInstanceOf[SE]
  }
}

trait DoubleLinkedContainer[SC <: DoubleLinkedContainer[SC, SE], SE <: DoubleLinkedContainerElement[SC, SE]]{
  var dlcHead, dlcLast : SE = null.asInstanceOf[SE]
  def dlcHasOnlyOne = dlcHead == dlcLast && dlcHead != null
  def dlcIsEmpty = dlcHead == null
  //  def sizeIsOne = head != null && head == last
  def dlcPrepend(that : SE) : this.type = {
    if(dlcHead != null){
      dlcHead.dlceLast = dlcHead
    } else {
      dlcLast = that
    }
    that.dlceNext = dlcHead
    that.dlceLast = null.asInstanceOf[SE]

    dlcHead = that

    this
  }

  def dlcAppend(that : SE) : this.type = {
    that.dlceNext = null.asInstanceOf[SE]
    that.dlceLast = dlcLast
    if(dlcLast != null){
      dlcLast.dlceNext = that
    } else {
      dlcHead = that
    }

    dlcLast = that
    this
  }
//
//  def iterable = new Iterable[Any] {
//    override def iterator: Iterator[SE] = iterator
//  }
//
//  def iterator = new Iterator[Any] {
//    var ptr = dlcHead
//    override def hasNext: Boolean = ptr != null
//
//    override def next(): SE = {
//      val ret = ptr
//      ptr = ret.nextNameableStatement
//      ret.asInstanceOf[SE]
//    }
//  }


  def dlcForeach[T >: SE](func : T => Unit) : Unit = {
    var ptr = dlcHead
    while(ptr != null){
      val current = ptr
      ptr = ptr.dlceNext
      func(current)
    }
  }
}

trait StatementDoubleLinkedContainer[SC <: Statement with DoubleLinkedContainer[SC, SE], SE <: Statement with DoubleLinkedContainerElement[SC, SE]] extends Statement with DoubleLinkedContainer[SC,SE]{
  def foreachStatements(func : SE => Unit) = dlcForeach(func)
  def hasOnlyOneStatement = dlcHasOnlyOne
  def head = dlcHead
}

trait StatementDoubleLinkedContainerElement[SC <: DoubleLinkedContainer[SC, SE], SE <: DoubleLinkedContainerElement[SC, SE]] extends Statement with DoubleLinkedContainerElement[SC,SE]{
  override def removeStatement(): Unit = {
    super.removeStatement()
    dlcRemove()
  }
}


trait DeclarationStatement extends LeafStatement with Nameable {
  override def foreachExpression(func: (Expression) => Unit): Unit = {}
  override def remapExpressions(func: (Expression) => Expression): Unit = {}
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

object TypeBool
object TypeBits
object TypeUInt
object TypeSInt
object TypeEnum

trait Expression extends BaseNode with ExpressionContainer{
  def opName : String
  def simplifyNode: Expression = this
  def getTypeObject : Any
}


//TODO IR check same scope
object Statement{
  def isFullToFullStatement(s : Statement) = s match {
    case  AssignementStatement(a : DeclarationStatement,b : DeclarationStatement) => true
    case _ => false
  }
  def isSomethingToFullStatement(s : Statement) = s match {
    case  AssignementStatement(a : DeclarationStatement,_) => true
    case _ => false
  }
}

trait Statement extends ExpressionContainer with ScalaLocated with BaseNode{
  var parentScope : ScopeStatement = null
  var lastScopeStatement, nextScopeStatement : Statement = null
  def rootScopeStatement: ScopeStatement = if(parentScope.parentStatement != null) parentScope.parentStatement.rootScopeStatement else parentScope

  def removeStatement() : Unit = {
    removeStatementFromScope()
  }

  def removeStatementFromScope() : Unit = {
    if(lastScopeStatement != null){
      lastScopeStatement.nextScopeStatement = nextScopeStatement
    } else {
      parentScope.head = nextScopeStatement
    }
    if(nextScopeStatement != null){
      nextScopeStatement.lastScopeStatement = lastScopeStatement
    } else {
      parentScope.last = lastScopeStatement
    }
    lastScopeStatement = null
    nextScopeStatement = null
    parentScope = null
  }


  def walkParentTreeStatements(func : (TreeStatement) => Unit) : Unit = {
    if(parentScope.parentStatement != null){
      func(parentScope.parentStatement)
      parentScope.parentStatement.walkParentTreeStatements(func)
    }
  }
}

trait LeafStatement extends Statement{

}

trait TreeStatement extends Statement{
  def foreachStatements(func : Statement => Unit)
  def walkStatements(func : Statement => Unit): Unit ={
    foreachStatements{
      case s : LeafStatement => func(s)
      case s : TreeStatement => func(s); s.walkStatements(func)
    }
  }

  def walkLeafStatements(func : LeafStatement => Unit): Unit ={
    foreachStatements {
      case s : LeafStatement => func(s)
      case s : TreeStatement => s.walkLeafStatements(func)
    }
  }

  def foreachDeclarations(func : DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s : DeclarationStatement => func(s)
      case _ =>
    }
  }

  def walkDeclarations(func : DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s : DeclarationStatement => func(s)
      case s : TreeStatement => s.walkDeclarations(func)
      case s =>
    }
  }
}

//trait AssignementStatementTarget {
//  private [core] def nameableNode : NameableNode
//}


object AssignementStatement{
  def unapply(x : AssignementStatement) : Option[(Expression, Expression)] = Some(x.target, x.source)
}

abstract class AssignementStatement extends LeafStatement with StatementDoubleLinkedContainerElement[BaseType,AssignementStatement]{
  var target, source : Expression = null
  override def rootScopeStatement = finalTarget.rootScopeStatement

  override def dlcParent = finalTarget
  override def normalizeInputs: Unit = {
    target match {
      case t : WidthProvider =>
        val finalTarget = this.finalTarget
        source = InputNormalize.resizedOrUnfixedLit(source.asInstanceOf[Expression with WidthProvider], t.getWidth, finalTarget.asInstanceOf[BitVector].resizeFactory, finalTarget, this)
      case _ =>
    }
  }

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
      case ref : BaseType =>
      case a : AssignementExpression => a.foreachDrivingExpression(func)
    }
    func(source)
  }


  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    target match {
      case ref : BaseType =>
      case a : AssignementExpression => a.remapDrivingExpressions(func)
    }
    source = func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    target = func(target)
    source = func(source)
  }


}

object DataAssignementStatement{
  def apply(target : Expression, source : Expression) = {
    val ret = new DataAssignementStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
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
    ret.finalTarget.dlcAppend(ret)
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


class SwitchStatementElement(var keys : ArrayBuffer[Expression],var scopeStatement: ScopeStatement) extends ScalaLocated
class SwitchStatement(var value : Expression) extends TreeStatement{
  val elements = ArrayBuffer[SwitchStatementElement]()
  var defaultScope : ScopeStatement = null

  override def foreachStatements(func: (Statement) => Unit): Unit = {
    elements.foreach(x => x.scopeStatement.foreachStatements(func))
    if(defaultScope != null) defaultScope.foreachStatements(func)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    value = func(value)
    remapElementsExpressions(func)
  }

  def remapElementsExpressions(func: (Expression) => Expression): Unit = {
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

  override def normalizeInputs: Unit = {
    def bitVectorNormalize(factory : => Resize) : Unit =  {
      val targetWidth = value.asInstanceOf[WidthProvider].getWidth

      for(e <- elements; k <- e.keys){
        for(i <- 0 until e.keys.length) {
          val k = e.keys(i)
          e.keys(i) = k match {
            case k: Expression with WidthProvider => InputNormalize.resizedOrUnfixedLit(k, targetWidth, factory, value, e)
          }
        }
      }
    }

    //TODO IR enum encoding stuff
    value.getTypeObject match {
      case `TypeBits` => bitVectorNormalize(new ResizeBits)
      case `TypeUInt` => bitVectorNormalize(new ResizeUInt)
      case `TypeSInt` => bitVectorNormalize(new ResizeSInt)
      case _ =>
    }
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
      head.lastScopeStatement = that
    } else {
      last = that
    }
    that.nextScopeStatement = head
    that.lastScopeStatement = null
    that.parentScope = this

    head = that

    this
  }

  def append(that : Statement) : this.type = {
    that.parentScope = this
    that.nextScopeStatement = null
    that.lastScopeStatement = last
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


  def walkStatements(func : Statement => Unit): Unit ={
    foreachStatements{
      case s : LeafStatement => func(s)
      case s : TreeStatement => func(s); s.walkStatements(func)
    }
  }

  def walkLeafStatements(func : LeafStatement => Unit): Unit ={
    foreachStatements {
      case s : LeafStatement => func(s)
      case s : TreeStatement => s.walkLeafStatements(func)
    }
  }

  def foreachDeclarations(func : DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s : DeclarationStatement => func(s)
      case s =>
    }
  }

  def walkDeclarations(func : DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s : DeclarationStatement => func(s)
      case s : TreeStatement => s.walkDeclarations(func)
      case s =>
    }
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


class DefaultTag(val that : BaseType) extends SpinalTag