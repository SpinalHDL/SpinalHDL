/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import spinal.core._

import scala.collection.immutable.Iterable
import scala.collection.mutable.ArrayBuffer
import spinal.idslplugin.Location


trait StatementDoubleLinkedContainer[SC <: Statement with DoubleLinkedContainer[SC, SE], SE <: Statement with DoubleLinkedContainerElement[SC, SE]] extends Statement with DoubleLinkedContainer[SC,SE]{
  def foreachStatements(func: SE => Unit) = dlcForeach(func)
  def hasOnlyOneStatement = dlcHasOnlyOne
  def head = dlcHead
  //  def statementsIterator = dlcIterator
  //  def statementsIterable = dlcIterable
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


class ScopeStatement(var parentStatement: TreeStatement) {
  var component: Component = if(parentStatement != null) parentStatement.component else null
  var head, last: Statement = null

  def isEmpty = head == null
  def nonEmpty = head != null



  def push() = DslScopeStack.set(this)

  def on(body : => Unit): Unit = {
    val ctx = push()
    body
    ctx.restore()
  }

  //Execute body on the head of the ScopeStatement list
  def onHead[T](body : => T) : T = {
    val ctx = push()
    val swapContext = swap()
    val ret = body
    ctx.restore()
    swapContext.appendBack()
    ret
  }

  class SwapContext(cHead: Statement, cLast: Statement){
    def appendBack(): Unit ={
      if(nonEmpty){
        last.nextScopeStatement = cHead
        if(cHead != null) cHead.lastScopeStatement = last
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

  def prepend(that: Statement): this.type = {
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

  def append(that: Statement): this.type = {
    that.parentScope        = this
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

  def foreachStatements(func: (Statement) => Unit) = {
    var ptr = head

    while(ptr != null){
      val current = ptr
      ptr = ptr.nextScopeStatement
      func(current)
    }
  }

  def walkStatements(func: Statement => Unit): Unit ={
    foreachStatements{
      case s: LeafStatement => func(s)
      case s: TreeStatement => func(s); s.walkStatements(func)
    }
  }

  def walkLeafStatements(func: LeafStatement => Unit): Unit ={
    foreachStatements {
      case s: LeafStatement => func(s)
      case s: TreeStatement => s.walkLeafStatements(func)
    }
  }

  def foreachDeclarations(func: DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s: DeclarationStatement => func(s)
      case s                       =>
    }
  }

  def walkDeclarations(func: DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s: DeclarationStatement => func(s)
      case s: TreeStatement        => s.walkDeclarations(func)
      case s                       =>
    }
  }
}

object Statement{

  def isFullToFullStatement(bt: BaseType): Boolean = bt.hasOnlyOneStatement && bt.head.parentScope == bt.rootScopeStatement && (bt.head match {
    case AssignmentStatement(a: DeclarationStatement, b: DeclarationStatement) => true
    case _ => false
  })

  def isFullToFullStatementOrLit(bt: BaseType): Boolean = bt.hasOnlyOneStatement && bt.head.parentScope == bt.rootScopeStatement && (bt.head match {
    case AssignmentStatement(a: DeclarationStatement, b: DeclarationStatement) => true
    case AssignmentStatement(a: DeclarationStatement, b: Literal) => true
    case _ => false
  })

  def isSomethingToFullStatement(bt: BaseType): Boolean = bt.hasOnlyOneStatement && bt.head.parentScope == bt.rootScopeStatement && (bt.head match {
    case AssignmentStatement(a: DeclarationStatement, _) =>
      true
    case _ =>
      false
  })
}


trait Statement extends ExpressionContainer with ContextUser with ScalaLocated with BaseNode{
  var lastScopeStatement, nextScopeStatement: Statement = null

  def rootScopeStatement: ScopeStatement = if(parentScope.parentStatement != null) parentScope.parentStatement.rootScopeStatement else parentScope

  def removeStatement(): Unit = {
    removeStatementFromScope()
  }

  def foreachClockDomain(func: ClockDomain => Unit): Unit = {}

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

  def walkParentTreeStatements(func: (TreeStatement) => Unit): Unit = {
    if(parentScope.parentStatement != null){
      func(parentScope.parentStatement)
      parentScope.parentStatement.walkParentTreeStatements(func)
    }
  }

  def walkParentTreeStatementsUntilRootScope(func: (TreeStatement) => Unit): Unit = {
    val root = rootScopeStatement

    if(root == null) return //Input of top level

    var ptr = parentScope

    while(ptr != root){
      func(ptr.parentStatement)
      ptr = ptr.parentStatement.parentScope
    }
  }

  def insertNext(s: Statement): Unit = {
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


trait LeafStatement extends Statement{}


trait TreeStatement extends Statement{
  def foreachStatements(func: Statement => Unit)

  def walkStatements(func : Statement => Unit): Unit ={
    foreachStatements{
      case s: LeafStatement => func(s)
      case s: TreeStatement => func(s); s.walkStatements(func)
    }
  }

  def walkLeafStatements(func: LeafStatement => Unit): Unit ={
    foreachStatements {
      case s: LeafStatement => func(s)
      case s: TreeStatement => s.walkLeafStatements(func)
    }
  }

  def foreachDeclarations(func: DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s: DeclarationStatement => func(s)
      case _ =>
    }
  }

  def walkDeclarations(func: DeclarationStatement => Unit): Unit ={
    foreachStatements{
      case s: DeclarationStatement => func(s)
      case s: TreeStatement        => s.walkDeclarations(func)
      case s =>
    }
  }
}


object AssignmentStatement{
  def unapply(x: AssignmentStatement): Option[(Expression, Expression)] = Some(x.target, x.source)
}


abstract class AssignmentStatement extends LeafStatement with StatementDoubleLinkedContainerElement[BaseType, AssignmentStatement]{
  var target, source: Expression = null
  var locationString : String = null

  def setLocation(loc : Location): this.type ={
    if(globalData.config.genLineComments) locationString = s"@ ${loc.file}.scala l${loc.line}"
    this
  }

  override def rootScopeStatement = finalTarget.rootScopeStatement

  override def dlcParent = finalTarget

  override def normalizeInputs: Unit = {
    target match {
      case t: WidthProvider =>
        val finalTarget = this.finalTarget
        source = InputNormalize.assignementResizedOrUnfixedLit(this)
      case _ =>
    }
  }

  def finalTarget: BaseType = target match{
    case n: BaseType             => n
    case a: AssignmentExpression => a.finalTarget
  }

  def foreachExpression(func: (Expression) => Unit): Unit = {
    func(target)
    func(source)
  }

  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {
    target match {
      case ref: BaseType           =>
      case a: AssignmentExpression => a.foreachDrivingExpression(func)
    }
    func(source)
  }

  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    target match {
      case ref: BaseType           =>
      case a: AssignmentExpression => a.remapDrivingExpressions(func)
    }
    source = stabilized(func, source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    target = stabilized(func, target)
    source = stabilized(func, source)
  }

  override def toString: String = s"$target := $source"

}


object DataAssignmentStatement{

  def apply(target: Expression, source: Expression) = {
    val ret = new DataAssignmentStatement

    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
    ret
  }

  def unapply(x: DataAssignmentStatement): Option[(Expression, Expression)] = Some(x.target, x.source)
}


class DataAssignmentStatement extends AssignmentStatement{}


object InitAssignmentStatement{

  def apply(target: Expression, source: Expression) = {
    val ret = new InitAssignmentStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
    ret
  }
}


class InitAssignmentStatement extends AssignmentStatement{}


object InitialAssignmentStatement{
  def apply(target: Expression, source: Expression) = {
    val ret = new InitialAssignmentStatement
    ret.target = target
    ret.source = source
    ret.finalTarget.dlcAppend(ret)
    ret
  }
}

class InitialAssignmentStatement extends AssignmentStatement{}


class WhenStatement(var cond: Expression) extends TreeStatement{
  val whenTrue, whenFalse = new ScopeStatement(this)

  override def normalizeInputs: Unit = {}

  def foreachStatements(func: (Statement) => Unit) = {
    whenTrue.foreachStatements(func)
    whenFalse.foreachStatements(func)
  }

  def foreachExpression(func: (Expression) => Unit) = {
    func(cond)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    cond = stabilized(func, cond)
  }
}


class SwitchStatement(var value: Expression) extends TreeStatement{
  val elements = ArrayBuffer[SwitchStatementElement]()
  var defaultScope: ScopeStatement = null
  var coverUnreachable = false
  var removeDuplication = false

  override def foreachStatements(func: (Statement) => Unit): Unit = {
    elements.foreach(x => x.scopeStatement.foreachStatements(func))

    if(defaultScope != null) defaultScope.foreachStatements(func)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    value = stabilized(func, value)
    remapElementsExpressions(func)
  }

  def remapElementsExpressions(func: (Expression) => Expression): Unit = {
    elements.foreach(x => {
      for(i <- x.keys.indices){
        x.keys(i) = stabilized(func, x.keys(i))
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
      for(e <- elements; eKeys = e.keys.toArray; k <- eKeys){
        for(i <- eKeys.indices) {
          val k = eKeys(i)

          e.keys(i) = k match {
            case k: SwitchStatementKeyBool        => k
            case k: Expression with WidthProvider => InputNormalize.resizedOrUnfixedLit(k, targetWidth, factory, value, e)
          }
        }
      }
      if(targetWidth == 0){
        def swap(scope : ScopeStatement) {
          scope.foreachStatements(_.parentScope = this.parentScope)
          if (this.lastScopeStatement != null)
            this.lastScopeStatement.nextScopeStatement = scope.head
          else
            parentScope.head = scope.head
          if (scope.head != null)
            scope.head.lastScopeStatement = this.lastScopeStatement

          if (this.nextScopeStatement != null)
            this.nextScopeStatement.lastScopeStatement = scope.last
          else
            parentScope.last = scope.last

          if (scope.last != null)
            scope.last.nextScopeStatement = this.nextScopeStatement
        }

        if(elements.nonEmpty){
          val element = elements.head
          assert(defaultScope == null)
          assert(element.keys.length == 1)
          assert(element.keys.head.asInstanceOf[WidthProvider].getWidth == 0)
          swap(element.scopeStatement)
        } else if(defaultScope != null){
          swap(defaultScope)
        } else {
          if(this.nextScopeStatement != null)
            this.nextScopeStatement.lastScopeStatement = this.lastScopeStatement
          if(this.lastScopeStatement != null)
            this.lastScopeStatement.nextScopeStatement = this.nextScopeStatement
        }

      }
    }


    //TODO IR senum encoding stuff
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
      def apply(size: BigInt): Exclusion = {
        if(size < 4096) new ExclusionByArray(size)
        else new ExclusionByNothing(size)
      }
    }

    abstract class Exclusion(val size: BigInt){
      var remaining = size
      def allocate(id: BigInt): Boolean
    }

    class ExclusionByNothing(size: BigInt) extends Exclusion(size){ //TODO better than nothing
      override def allocate(id: BigInt): Boolean = true
    }

    class ExclusionByArray(size: BigInt) extends Exclusion(size){
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
    elements.foreach{element => element.keys.foreach{
      case lit: EnumLiteral[_] =>
        if(!coverage.allocate(lit.senum.position)){
          PendingError(s"UNREACHABLE IS STATEMENT in the switch statement at \n" + element.getScalaLocationLong)
        }
      case lit: Literal =>
        if(!coverage.allocate(lit.getValue())){
          PendingError(s"UNREACHABLE IS STATEMENT in the switch statement at \n" + element.getScalaLocationLong)
        }
      case _ =>
        hadNonLiteralKey = true
    }}

    return coverage.remaining == BigInt(0) && !hadNonLiteralKey
  }
}


object AssertStatementHelper{

  def apply(cond: Bool, message: Seq[Any], severity: AssertNodeSeverity, kind: AssertStatementKind, trigger : AssertStatementTrigger, loc: Location): AssertStatement = {
    val node = AssertStatement(cond, message, severity, kind, trigger, loc)

    if(!GlobalData.get.phaseContext.config.noAssert){
      DslScopeStack.get.append(node)
    }

    node
  }

  def apply(cond: Bool, message: String, severity: AssertNodeSeverity, kind : AssertStatementKind, trigger : AssertStatementTrigger, loc: Location): AssertStatement ={
    AssertStatementHelper(cond, List(message), severity, kind, trigger, loc)
  }
}


class AssertStatementKind
object AssertStatementKind{
  val ASSERT = new AssertStatementKind
  val ASSUME = new AssertStatementKind
  val COVER = new AssertStatementKind
}

class AssertStatementTrigger
object AssertStatementTrigger{
  val CLOCKED = new AssertStatementTrigger
  val INITIAL = new AssertStatementTrigger
}

case class AssertStatement(var cond: Expression, message: Seq[Any], severity: AssertNodeSeverity, kind : AssertStatementKind, trigger : AssertStatementTrigger, loc: Location) extends LeafStatement with SpinalTagReady {
  var clockDomain = ClockDomain.current

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(cond)
    message.foreach {
      case e: Expression => func(e)
      case _ =>
    }
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = cond = stabilized(func, cond)

  override def foreachClockDomain(func: (ClockDomain) => Unit): Unit = trigger match {
    case AssertStatementTrigger.CLOCKED => func(clockDomain)
    case AssertStatementTrigger.INITIAL =>
  }
}
