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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

class ComponentEmitterTrace(val builders: Seq[mutable.StringBuilder], val strings: Seq[String]) {

  var hash: Integer = null

  override def hashCode(): Int = {
    if (hash == null) {
      hash = builders.foldLeft(0)(_ + _.hashCode) + strings.foldLeft(0)(_ + _.hashCode)
    }
    hash
  }

  override def equals(obj: scala.Any): Boolean = {
    if (this.hashCode() != obj.hashCode()) return false //Collision into hashmap implementation don't check it XD
    obj match {
      case that: ComponentEmitterTrace =>
        return (this.builders, that.builders).zipped.map(_ == _).forall(e => e) && (this.strings, that.strings).zipped.map(_ == _).forall(e => e)
    }
  }

}


abstract class ComponentEmitter {

  def spinalConfig : SpinalConfig
  def component: Component
  def algoIdIncrementalBase: Int
  def mergeAsyncProcess: Boolean
  def readedOutputWrapEnable : Boolean = false

  val wrappedExpressionToName = mutable.HashMap[Expression, String]()
  val referencesOverrides     = mutable.HashMap[Nameable,Any]()
  var algoIdIncrementalOffset = 0

  val syncGroups = mutable.LinkedHashMap[(ClockDomain, ScopeStatement, Boolean), SyncGroup]()
  val processes  = mutable.LinkedHashSet[AsyncProcess]()
  val initials  = mutable.ArrayBuffer[LeafStatement]()
  val analogs    = ArrayBuffer[BaseType]()
  val mems       = ArrayBuffer[Mem[_]]()
  val multiplexersPerSelect = mutable.LinkedHashMap[(Expression with WidthProvider,Int), ArrayBuffer[Multiplexer]]()

  val expressionToWrap   = mutable.LinkedHashSet[Expression]()
  val outputsToBufferize = mutable.LinkedHashSet[BaseType]() //Check if there is a reference to an output pin (read self outputed signal)
  val subComponentInputToNotBufferize = mutable.HashSet[Any]()
  val openSubIo = mutable.HashSet[BaseType]()

  def getOrDefault[X,Y](map: java.util.concurrent.ConcurrentHashMap[X,Y], key: X, default: Y) = map.get(key) match {
    case null => default
    case x    => x
  }

  def wrapSubInput(io: BaseType): Unit

  class AsyncProcess(val scope: ScopeStatement, val instanceCounter: Int, val allowMerge: Boolean){
    val leafStatements = ArrayBuffer[LeafStatement]() //.length should be Oc
    var nameableTargets = List[DeclarationStatement]()
  }


  class SyncGroup(val clockDomain: ClockDomain, val scope: ScopeStatement, val hasInit: Boolean, val instanceCounter: Int) {
    val initStatements = ArrayBuffer[LeafStatement]()
    val dataStatements = ArrayBuffer[LeafStatement]()
  }

  def allocateAlgoIncrementale(): Int = {
    val ret = algoIdIncrementalBase + algoIdIncrementalOffset
    algoIdIncrementalOffset += 1
    ret
  }

  def isSubComponentInputBinded(data: BaseType) = {
    if(data.isInput && data.isComb && Statement.isFullToFullStatement(data)/* && data.head.asInstanceOf[AssignmentStatement].source.asInstanceOf[BaseType].component == data.component.parent*/)
      data.head.source
    else
      null
  }

  def commentTagsToString(host : SpinalTagReady, comment : String) : String = {
    val strings = host.getTags().collect{case t : CommentTag => comment + t.comment.replace("\n","\n" + comment)}
    if(strings.isEmpty) "" else strings.mkString("\n") + "\n"
  }

  def elaborate() = {
    val asyncStatement = ArrayBuffer[LeafStatement]()

    //Sort all leaf statements into their nature (sync/async)
    var syncGroupInstanceCounter = 0
    component.dslBody.walkLeafStatements {
      case s: InitialAssignmentStatement => initials += s
      case s: AssignmentStatement =>
        s.finalTarget match {
          case target: BaseType if target.isComb => asyncStatement += s
          case target: BaseType if target.isReg  =>
            val group = syncGroups.getOrElseUpdate((target.clockDomain, s.rootScopeStatement, target.hasInit), new SyncGroup(target.clockDomain, s.rootScopeStatement, target.hasInit, syncGroupInstanceCounter))
            syncGroupInstanceCounter += 1
            s match {
              case s: InitAssignmentStatement => group.initStatements += s
              case s: DataAssignmentStatement => group.dataStatements += s
            }
          case target: BaseType if target.isAnalog =>
        }
      case assertStatement: AssertStatement => assertStatement.trigger match {
        case AssertStatementTrigger.CLOCKED => {
          val group = syncGroups.getOrElseUpdate((assertStatement.clockDomain, assertStatement.rootScopeStatement, true), new SyncGroup(assertStatement.clockDomain, assertStatement.rootScopeStatement, true, syncGroupInstanceCounter))
          syncGroupInstanceCounter += 1
          group.dataStatements += assertStatement
        }
        case AssertStatementTrigger.INITIAL => {
          initials += assertStatement
        }
      }
      case x: MemPortStatement       =>
      case x: Mem[_]                 => mems += x
      case x: BaseType if x.isAnalog => analogs += x
      case x: DeclarationStatement   =>
    }

    //Generate AsyncProcess per target
    val asyncProcessFromNameableTarget   = mutable.LinkedHashMap[Nameable, AsyncProcess]()
    val rootTreeStatementPerAsyncProcess = mutable.LinkedHashMap[TreeStatement, AsyncProcess]()
    var asyncGroupInstanceCounter        = 0

    for(s <- asyncStatement) s match{
      case s: AssignmentStatement =>
        var rootTreeStatement: TreeStatement = null

        var scopePtr    = s.parentScope
        val finalTarget = s.finalTarget
        val rootScope = finalTarget.rootScopeStatement
        val allowMerge = !finalTarget.hasTag(noBackendCombMerge)
        while (scopePtr != rootScope) {
          rootTreeStatement = scopePtr.parentStatement
          scopePtr = scopePtr.parentStatement.parentScope
        }

        if (rootTreeStatement != null) {
          val preExistingTargetProcess = asyncProcessFromNameableTarget.getOrElse(finalTarget, null)
          val preExistingRootTreeProcess = if(mergeAsyncProcess && allowMerge) {
            val process = rootTreeStatementPerAsyncProcess.getOrElse(rootTreeStatement, null)
            if(process != null && process.allowMerge) process else null
          } else {
            null
          }

          if(preExistingTargetProcess == null && preExistingRootTreeProcess == null){ //Create new process
            val process = new AsyncProcess(rootScope, asyncGroupInstanceCounter, allowMerge)
            asyncGroupInstanceCounter += 1
            asyncProcessFromNameableTarget(finalTarget) = process
            if(allowMerge) rootTreeStatementPerAsyncProcess(rootTreeStatement) = process
            process.nameableTargets = finalTarget :: process.nameableTargets

          }else if(preExistingTargetProcess != null && preExistingRootTreeProcess == null){

            val process = preExistingTargetProcess
            if(allowMerge) rootTreeStatementPerAsyncProcess(rootTreeStatement) = process
          } else if(preExistingTargetProcess == null && preExistingRootTreeProcess != null){
            val process = preExistingRootTreeProcess
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets = finalTarget :: process.nameableTargets

          }else if(preExistingTargetProcess != preExistingRootTreeProcess) { //Merge

            val process = preExistingRootTreeProcess
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets ++= preExistingTargetProcess.nameableTargets
            preExistingTargetProcess.nameableTargets.foreach(asyncProcessFromNameableTarget(_) = process)

          }
        } else { //No when stuff
          val preExistingTargetProcess = asyncProcessFromNameableTarget.getOrElse(finalTarget, null)

          if(preExistingTargetProcess == null) {
            //Create new process
            val process = new AsyncProcess(rootScope,asyncGroupInstanceCounter, allowMerge)
            asyncGroupInstanceCounter += 1
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets = finalTarget :: process.nameableTargets
          }
        }
    }

    //Add statements into AsyncProcesses
    asyncProcessFromNameableTarget.valuesIterator.foreach(p => processes += p)
    for(s <- asyncStatement) s match {
      case s: AssignmentStatement =>
        var process = asyncProcessFromNameableTarget.getOrElse(s.finalTarget,null)
        if(process == null){ // ???
          process = new AsyncProcess(s.rootScopeStatement,asyncGroupInstanceCounter,false)
          asyncGroupInstanceCounter += 1
          process.nameableTargets = s.finalTarget :: process.nameableTargets
        }
        process.leafStatements += s
        processes += process
    }

    //identify duplicated expression due to `when`/'switch' spliting/duplication
    {
      val whenCondOccurences = mutable.LinkedHashMap[Expression, Int]()
      def walker(statements: ArrayBuffer[LeafStatement], statementIndexInit: Int, scope: ScopeStatement, algoId: Int): Int ={
        var statementIndex = statementIndexInit

        while(statementIndex < statements.length){
          val statement = statements(statementIndex)

          statement match {
            case AssignmentStatement(target : RangedAssignmentFloating, _) => expressionToWrap += target.offset
            case _ =>
          }

          val targetScope = statement.parentScope

          if(targetScope == scope){
            statementIndex += 1
          }else {
            var scopePtr = targetScope

            while(scopePtr.parentStatement != null && scopePtr.parentStatement.parentScope != scope){
              scopePtr = scopePtr.parentStatement.parentScope
            }

            if(scopePtr.parentStatement == null) {
              return statementIndex
            }

            val treeStatement = scopePtr.parentStatement

            if(treeStatement.algoIncrementale != algoId) {

              treeStatement.algoIncrementale = algoId

              treeStatement match {
                case w: WhenStatement =>
                  if (!w.cond.isInstanceOf[DeclarationStatement]) {
                    val counter = whenCondOccurences.getOrElseUpdate(w.cond, 0)
                    if (counter < 2) {
                      whenCondOccurences(w.cond) = counter + 1
                    }
                  }
                case s: SwitchStatement =>
                  if (!s.value.isInstanceOf[DeclarationStatement]) {
                    val counter = whenCondOccurences.getOrElseUpdate(s.value, 0)
                    if (counter < 2) {
                      whenCondOccurences(s.value) = counter + 1
                    }
                  }
              }
            }
            statementIndex = walker(statements,statementIndex, scopePtr, algoId)
          }
        }
        return statementIndex
      }

      for (process <- processes) {
        walker(process.leafStatements, 0, process.scope, allocateAlgoIncrementale())
      }

      syncGroups.valuesIterator.foreach(group => {
        walker(group.initStatements, 0, group.scope, allocateAlgoIncrementale())
        walker(group.dataStatements, 0, group.scope, allocateAlgoIncrementale())
      })

      if(!spinalConfig.inlineConditionalExpression) {
        for ((c, n) <- whenCondOccurences if n > 1) {
          expressionToWrap += c
        }
      }
    }



    //Manage subcomponents input bindings
    for(sub <- component.children){
      for(io <- sub.getOrdredNodeIo if io.isInput){
        var subInputBinded = isSubComponentInputBinded(io)

        if(subInputBinded != null) {
          referencesOverrides(io) = subInputBinded
          subComponentInputToNotBufferize += io
        }else {
          wrapSubInput(io)
        }
      }
    }

    //Fill multiplexersPerSelect
    component.dslBody.walkStatements(s => {
      s.walkDrivingExpressions{
        case e : Multiplexer =>
          multiplexersPerSelect.getOrElseUpdate((e.select, e.inputs.length), new ArrayBuffer[Multiplexer]) += e
        case _ =>
      }
    })

    //Get all component outputs which are read internaly
    //And also fill some expressionToWrap from switch(xx)

    val clockDomains = mutable.LinkedHashSet[ClockDomain]()

    component.dslBody.walkStatements(s => {
      s.foreachClockDomain(clockDomains += _)
      s match {
        case s: SwitchStatement if !spinalConfig.inlineConditionalExpression => expressionToWrap += s.value
        case _                  =>
      }
      if(readedOutputWrapEnable) {
        s.walkDrivingExpressions {
          case bt: BaseType =>
            if (bt.component == component && bt.isOutput) {
              outputsToBufferize += bt
            }
          case _ =>
        }
      }
    })

    clockDomains.foreach(cd => {

      def check(that: Bool) = {
        if(that != null) {
          val pulled = component.pulledDataCache.getOrElse(that, throw new Exception("???")).asInstanceOf[Bool]
          if (that.component == component && that.isOutput) {
            outputsToBufferize += that
          }
        }
      }

      check(cd.clock)
      check(cd.reset)
      check(cd.softReset)
      check(cd.clockEnable)
    })

  }

  def cutLongExpressions(): Unit ={

    //Avoid too deep expressions generation
    component.dslBody.walkStatements{
      case s : AssignmentStatement => {

        def filterMux(that : Multiplexer): Unit ={
          that.foreachDrivingExpression {
            case subExpression : Multiplexer => filterMux(subExpression)
            case subExpression => walk(subExpression)
          }
        }

        def walk(root : ExpressionContainer): Unit = {
          var size = 0
          val maximalDepth = 32
          var oldDeepBuffer, newDeepBuffer = ArrayBuffer[Expression]()
          root.foreachDrivingExpression{
            case subExpression : Multiplexer => filterMux(subExpression)
            case subExpression => oldDeepBuffer += subExpression
          }
          while (oldDeepBuffer.nonEmpty) {
            newDeepBuffer.clear()
            size += oldDeepBuffer.length
            if (size >= maximalDepth) {
              size = 0
              expressionToWrap ++= oldDeepBuffer
            }

            oldDeepBuffer.foreach { expression =>
              expression.foreachDrivingExpression { subExpression =>
                if (!expressionToWrap.contains(subExpression)) {
                  subExpression match{
                    case subExpression : Multiplexer => filterMux(subExpression)
                    case _ => newDeepBuffer += subExpression
                  }
                } else if(!subExpression.isInstanceOf[DeclarationStatement]){
                  walk(subExpression)
                }
              }
            }
            val tmp = newDeepBuffer
            newDeepBuffer = oldDeepBuffer
            oldDeepBuffer = tmp
          }
        }

        walk(s)
      }
      case _ =>
    }
  }
}
