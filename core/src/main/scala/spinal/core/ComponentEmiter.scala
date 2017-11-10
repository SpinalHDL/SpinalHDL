package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ComponentEmiterTrace(val builders : Seq[mutable.StringBuilder], val strings : Seq[String]){
  var hash: Integer = null

  override def hashCode(): Int = {
    if (hash == null) {
      hash = builders.foldLeft(0)(_ + _.hashCode) + strings.foldLeft(0)(_ + _.hashCode)
    }
    hash
  }

  override def equals(obj: scala.Any): Boolean = {
    if (this.hashCode() != obj.hashCode()) return false //Colision into hashmap implementation don't check it XD
    obj match {
      case that: ComponentEmiterTrace => {
        return (this.builders, that.builders).zipped.map(_ == _).reduce(_ && _) && (this.strings, that.strings).zipped.map(_ == _).reduce(_ && _)
      }
    }
  }

}

abstract class ComponentEmiter {
  def component : Component
  def algoIdIncrementalBase : Int
  def mergeAsyncProcess : Boolean
  val wrappedExpressionToName = mutable.HashMap[Expression, String]()
  val referencesOverrides = mutable.HashMap[Nameable,Any]()
  var algoIdIncrementalOffset = 0

  val syncGroups = mutable.LinkedHashMap[(ClockDomain, ScopeStatement, Boolean), SyncGroup]()
  val processes = mutable.LinkedHashSet[AsyncProcess]()
  val analogs = ArrayBuffer[BaseType]()
  val mems = ArrayBuffer[Mem[_]]()
  val expressionToWrap = mutable.LinkedHashSet[Expression]()
  val outputsToBufferize = mutable.LinkedHashSet[BaseType]() //Check if there is a reference to an output pin (read self outputed signal)
  val subComponentInputToNotBufferize = mutable.HashSet[Any]()
  val openSubIo = mutable.HashSet[BaseType]()

  def getOrDefault[X,Y](map : java.util.concurrent.ConcurrentHashMap[X,Y], key : X, default : Y) = map.get(key) match {
    case null => default
    case x => x
  }


  def wrapSubInput(io: BaseType) : Unit

  class AsyncProcess(val scope : ScopeStatement, val instanceCounter : Int){
    val leafStatements = ArrayBuffer[LeafStatement]() //.length should be Oc
    var nameableTargets = List[DeclarationStatement]()
  }

  class SyncGroup(val clockDomain: ClockDomain, val scope: ScopeStatement, val hasInit : Boolean, val instanceCounter : Int){
    val initStatements = ArrayBuffer[LeafStatement]()
    val dataStatements = ArrayBuffer[LeafStatement]()
  }




  def allocateAlgoIncrementale() : Int = {
    val ret = algoIdIncrementalBase + algoIdIncrementalOffset
    algoIdIncrementalOffset += 1
    ret
  }

  def isSubComponentInputBinded(data : BaseType) = {
    if(data.isInput && data.isComb && Statement.isFullToFullStatement(data)/* && data.head.asInstanceOf[AssignmentStatement].source.asInstanceOf[BaseType].component == data.component.parent*/)
      data.head.source.asInstanceOf[BaseType]
    else
      null
  }


  def elaborate() ={
    val asyncStatement = ArrayBuffer[LeafStatement]()

    //Sort all leaf statements into their nature (sync/async)
    var syncGroupInstanceCounter = 0
    component.dslBody.walkLeafStatements(_ match {
      case s : AssignmentStatement => s.finalTarget match {
        case target : BaseType if target.isComb => asyncStatement += s
        case target : BaseType if target.isReg  => {
          val group = syncGroups.getOrElseUpdate((target.clockDomain, s.rootScopeStatement, target.hasInit) , new SyncGroup(target.clockDomain ,s.rootScopeStatement, target.hasInit, syncGroupInstanceCounter))
          syncGroupInstanceCounter += 1
          s match {
            case s : InitAssignmentStatement => group.initStatements += s
            case s : DataAssignmentStatement => group.dataStatements += s
          }
        }
        case target : BaseType if target.isAnalog =>
      }
      case assertStatement : AssertStatement => {
        val group = syncGroups.getOrElseUpdate((assertStatement.clockDomain, assertStatement.rootScopeStatement, false) , new SyncGroup(assertStatement.clockDomain ,assertStatement.rootScopeStatement, false, syncGroupInstanceCounter))
        syncGroupInstanceCounter += 1
        group.dataStatements += assertStatement
      }
      case x : MemPortStatement =>
      case x : Mem[_] => mems += x
      case x : BaseType if x.isAnalog => analogs += x
      case x : DeclarationStatement =>
    })

    //Generate AsyncProcess per target
    val asyncProcessFromNameableTarget = mutable.LinkedHashMap[Nameable,AsyncProcess]()
    val rootTreeStatementPerAsyncProcess = mutable.LinkedHashMap[TreeStatement,AsyncProcess]()
    var asyncGroupInstanceCounter = 0
    for(s <- asyncStatement) s match{
      case s : AssignmentStatement => {
        var rootTreeStatement: TreeStatement = null
        var scopePtr = s.parentScope
        val finalTarget = s.finalTarget
        val rootScope = finalTarget.rootScopeStatement

        while (scopePtr != rootScope) {
          rootTreeStatement = scopePtr.parentStatement
          scopePtr = scopePtr.parentStatement.parentScope
        }
        if (rootTreeStatement != null) {
          val preExistingTargetProcess = asyncProcessFromNameableTarget.getOrElse(finalTarget, null)
          val preExistingRootTreeProcess = if(mergeAsyncProcess) rootTreeStatementPerAsyncProcess.getOrElse(rootTreeStatement, null) else null
          if(preExistingTargetProcess == null && preExistingRootTreeProcess == null){ //Create new process
            val process = new AsyncProcess(rootScope, asyncGroupInstanceCounter)
            asyncGroupInstanceCounter += 1
            asyncProcessFromNameableTarget(finalTarget) = process
            rootTreeStatementPerAsyncProcess(rootTreeStatement) = process
            process.nameableTargets = finalTarget :: process.nameableTargets
          } else if(preExistingTargetProcess != null && preExistingRootTreeProcess == null){
            val process = preExistingTargetProcess
            rootTreeStatementPerAsyncProcess(rootTreeStatement) = process
          } else if(preExistingTargetProcess == null && preExistingRootTreeProcess != null){
            val process = preExistingRootTreeProcess
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets = finalTarget :: process.nameableTargets
          } else if(preExistingTargetProcess != preExistingRootTreeProcess) { //Merge
            val process = preExistingRootTreeProcess
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets ++= preExistingTargetProcess.nameableTargets
            preExistingTargetProcess.nameableTargets.foreach(asyncProcessFromNameableTarget(_) = process)
          }
        } else {
          val preExistingTargetProcess = asyncProcessFromNameableTarget.getOrElse(finalTarget, null)
          if(preExistingTargetProcess == null) {
            //Create new process
            val process = new AsyncProcess(rootScope,asyncGroupInstanceCounter)
            asyncGroupInstanceCounter += 1
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets = finalTarget :: process.nameableTargets
          }
        }
      }
    }

    //Add statements into AsyncProcesses
    asyncProcessFromNameableTarget.valuesIterator.foreach(p => processes += p)
    for(s <- asyncStatement) s match {
      case s: AssignmentStatement => {
        var process = asyncProcessFromNameableTarget.getOrElse(s.finalTarget,null)
        if(process == null){
          process = new AsyncProcess(s.rootScopeStatement,asyncGroupInstanceCounter)
          asyncGroupInstanceCounter += 1
          process.nameableTargets = s.finalTarget :: process.nameableTargets
        }
        process.leafStatements += s
        processes += process
      }
    }

    //identify duplicated expression due to `when` spliting/duplication

    {
      val whenCondOccurences = mutable.HashMap[Expression, Int]()
      def walker(statements : ArrayBuffer[LeafStatement], statementIndexInit : Int, scope : ScopeStatement, algoId : Int): Int ={
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
          } else {
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
                case w: WhenStatement => {
                  if (!w.cond.isInstanceOf[DeclarationStatement]) {
                    val counter = whenCondOccurences.getOrElseUpdate(w.cond, 0)
                    if (counter < 2) {
                      whenCondOccurences(w.cond) = counter + 1
                    }
                  }
                }
                case s: SwitchStatement => {
                  if (!s.value.isInstanceOf[DeclarationStatement]) {
                    val counter = whenCondOccurences.getOrElseUpdate(s.value, 0)
                    if (counter < 2) {
                      whenCondOccurences(s.value) = counter + 1
                    }
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

      for ((c, n) <- whenCondOccurences if n > 1) {
        expressionToWrap += c
      }
    }

    //Manage subcomponents input bindings
    for(sub <- component.children){
      for(io <- sub.getOrdredNodeIo if io.isInput){
        var subInputBinded = isSubComponentInputBinded(io)
        if(subInputBinded != null) {
          referencesOverrides(io) = subInputBinded
          subComponentInputToNotBufferize += io
        } else {
          wrapSubInput(io)
        }
      }
    }


    //Get all component outputs which are read internaly
    //And also fill some expressionToWrap from switch(xx)
    component.dslBody.walkStatements(s => {
      s match {
        case s : SwitchStatement => expressionToWrap += s.value
        case _ =>
      }
      s.walkDrivingExpressions(_ match {
        case bt: BaseType => {
          if (bt.component == component && bt.isOutput) {
            outputsToBufferize += bt
          }
        }
        case _ =>
      })
    })

  }


}
