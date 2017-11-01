package spinal.core


import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//TODO IR :
// Mem blackboxify


trait BaseNode {
  var algoInt, algoIncrementale = 0
  private[core] def getClassIdentifier: String = this.getClass.getName.split('.').last.replace("$","")
  def toStringMultiLine() : String = this.toString
}



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
//  def foreachDrivingExpressionWithDelay(func : (Expression, Int) => Unit) : Unit = foreachExpression(func(_,0))

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
  private[core] def foreachDrivingExpression(outHi : Int, outLo : Int)(f : (Expression, Int,Int) => Unit) : Unit = foreachDrivingExpression{
    case input : Expression with WidthProvider => f(input, input.getWidth-1,0)
    case input => f(input, 0,0)
  }

  override def toString = opName
}



object Statement{
  def isFullToFullStatement(bt : BaseType) : Boolean = bt.hasOnlyOneStatement && bt.head.parentScope == bt.rootScopeStatement && (bt.head match {
    case  AssignmentStatement(a : DeclarationStatement,b : DeclarationStatement) => true
    case _ => false
  })
  def isSomethingToFullStatement(bt : BaseType) : Boolean = bt.hasOnlyOneStatement && bt.head.parentScope == bt.rootScopeStatement && (bt.head match {
    case  AssignmentStatement(a : DeclarationStatement,_) => true
    case _ => false
  })
}

trait Statement extends ExpressionContainer with ContextUser with ScalaLocated with BaseNode{
  var lastScopeStatement, nextScopeStatement : Statement = null
  def rootScopeStatement: ScopeStatement = if(parentScope.parentStatement != null) parentScope.parentStatement.rootScopeStatement else parentScope
  def removeStatement() : Unit = {
    removeStatementFromScope()
  }
  def foreachClockDomain(func : ClockDomain => Unit) : Unit = {}

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

  def walkParentTreeStatementsUntilRootScope(func : (TreeStatement) => Unit) : Unit = {
    val root = rootScopeStatement
    if(root == null) return //Input of top level
    var ptr = parentScope
    while(ptr != root){
      if(ptr.parentStatement == null){
        print("asd")
      }
      func(ptr.parentStatement)
      ptr = ptr.parentStatement.parentScope
    }
  }

  def insertNext(s : Statement): Unit = {
    if(nextScopeStatement == null)
      parentScope.last = s
    else
      this.nextScopeStatement.lastScopeStatement = s

    s.nextScopeStatement = this.nextScopeStatement
    this.nextScopeStatement = s

    s.lastScopeStatement = this
    s.parentScope = this.parentScope
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

//trait AssignmentStatementTarget {
//  private [core] def nameableNode : NameableNode
//}


object AssignmentStatement{
  def unapply(x : AssignmentStatement) : Option[(Expression, Expression)] = Some(x.target, x.source)
}

abstract class AssignmentStatement extends LeafStatement with StatementDoubleLinkedContainerElement[BaseType,AssignmentStatement]{
  var target, source : Expression = null
  override def rootScopeStatement = finalTarget.rootScopeStatement

  override def dlcParent = finalTarget
  override def normalizeInputs: Unit = {
    target match {
      case t : WidthProvider =>
        val finalTarget = this.finalTarget
        source = InputNormalize.assignementResizedOrUnfixedLit(this)
      case _ =>
    }
  }

  def finalTarget : BaseType = target match{
    case n : BaseType => n
    case a : AssignmentExpression => a.finalTarget
  }

  def foreachExpression(func : (Expression) => Unit) : Unit = {
    func(target)
    func(source)
  }

  override def foreachDrivingExpression(func : (Expression) => Unit) : Unit = {
    target match {
      case ref : BaseType =>
      case a : AssignmentExpression => a.foreachDrivingExpression(func)
    }
    func(source)
  }


  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    target match {
      case ref : BaseType =>
      case a : AssignmentExpression => a.remapDrivingExpressions(func)
    }
    source = func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    target = func(target)
    source = func(source)
  }

  override def toStringMultiLine() = {
    s"$target := $source"
  }
}

object DataAssignmentStatement{
  def apply(target : Expression, source : Expression) = {
    val ret = new DataAssignmentStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
    ret
  }
}

class DataAssignmentStatement extends AssignmentStatement{

}

object InitAssignmentStatement{
  def apply(target : Expression, source : Expression) = {
    val ret = new InitAssignmentStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
    ret
  }
}

class InitAssignmentStatement extends AssignmentStatement{

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

object SwitchStatementKeyBool{
  def apply(cond : Expression): SwitchStatementKeyBool ={
    val ret = new SwitchStatementKeyBool
    ret.cond = cond
    ret
  }
}
class SwitchStatementKeyBool extends Expression{
  var cond : Expression = null

  override def opName: String = "is(b)"
  override def getTypeObject: Any = TypeBool
  override def remapExpressions(func: (Expression) => Expression): Unit = cond = func(cond)
  override def foreachExpression(func: (Expression) => Unit): Unit = func(cond)
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
            case k : SwitchStatementKeyBool => k
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
      case `TypeEnum` => InputNormalize.switchEnumImpl(this)
      case _ =>
    }
  }


  def isFullyCoveredWithoutDefault: Boolean ={
    object Exclusion{
      def apply(size : BigInt) : Exclusion = {
        if(size < 4096) new ExclusionByArray((size))
        else new ExclusionByNothing(size)
      }
    }

    abstract class Exclusion(val size : BigInt){
      var remaining = size
      def allocate(id : BigInt): Boolean
    }

    class ExclusionByNothing(size : BigInt) extends Exclusion(size){ //TODO better than nothing
      override def allocate(id: BigInt): Boolean = true
    }

    class ExclusionByArray(size : BigInt) extends Exclusion(size){
      val occupancy = new Array[Boolean](size.toInt)

      def allocate(id_ : BigInt): Boolean ={
        val id = id_.toInt
        if(id_ >= size){
          println("???")
        }
        if(occupancy(id)) return false
        occupancy(id) = true
        remaining -= 1
        return true
      }
    }


    val coverage = Exclusion(value.getTypeObject match {
      case TypeBool => BigInt(2)
      case TypeBits => BigInt(1) << value.asInstanceOf[WidthProvider].getWidth
      case TypeUInt => BigInt(1) << value.asInstanceOf[WidthProvider].getWidth
      case TypeSInt => BigInt(1) << value.asInstanceOf[WidthProvider].getWidth
      case TypeEnum => BigInt(value.asInstanceOf[EnumEncoded].getDefinition.elements.length)
    })

    var hadNonLiteralKey = false
    elements.foreach(element => element.keys.foreach{
      case lit : EnumLiteral[_] => if(!coverage.allocate(lit.enum.position)){
        PendingError(s"Condition duplication in the switch statement at \n" + this.getScalaLocationLong)
      }
      case lit : Literal => if(!coverage.allocate(lit.getValue())){
        PendingError(s"Condition duplication in the switch statement at \n" + this.getScalaLocationLong)
      }
      case _ => hadNonLiteralKey = true
    })

    return coverage.remaining == 0 && ! hadNonLiteralKey
  }
}

class ScopeStatement(var parentStatement : TreeStatement){
  var component : Component = if(parentStatement != null) parentStatement.component else null
  var head, last : Statement = null
  def isEmpty = head == null
  def nonEmpty = head != null

  def push() = GlobalData.get.dslScope.push(this)
  def pop() = GlobalData.get.dslScope.pop()

  class SwapContext(cHead : Statement, cLast : Statement){
    def appendBack() : Unit ={
      if(nonEmpty){
        last.nextScopeStatement = cHead
        cHead.lastScopeStatement = last
      } else {
        head = cHead
      }

      if(cLast != null) {
        last = cLast
      }
    }
  }

  def swap(): SwapContext ={
    val ret = new SwapContext(head,last)
    head = null
    last = null
    ret
  }


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
}


object GraphUtils{
  def walkAllComponents(root : Component, func : Component => Unit) : Unit = {
    func(root)
    root.children.foreach(walkAllComponents(_, func))
  }
}


class DefaultTag(val that : BaseType) extends SpinalTag
