package spinal.core

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

//
//import scala.collection.mutable
//import scala.collection.mutable.{ArrayBuffer, StringBuilder, HashMap}
//import scala.util.Random
//
///**
// * Created by PIC32F_USER on 05/06/2016.
// */
//
//
//
class PhaseVhdl(pc : PhaseContext) extends PhaseMisc with VhdlBase {
  import pc._

  override def useNodeConsumers: Boolean = false

  override def impl(pc: PhaseContext): Unit = {

    for (c <- sortedComponents) {
      if (!c.isInBlackBoxTree) {
        SpinalProgress(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
        compile(c)
      }
    }

  }

  def compile(component: Component): Unit = {
    val text = emitComponent(component)
//    println("SIZE " + text.length + "   " + text.lines.length)
    println(text)
//    outFile.write(text)
  }

  class AsyncProcess(val scope : ScopeStatement){
    val leafStatements = ArrayBuffer[LeafStatement]() //.length should be Oc
    var nameableTargets = List[NameableNode]()
  }

  class SyncGroup(val clockDomain: ClockDomain, val scope: ScopeStatement, val hasInit : Boolean){
    val initStatements = ArrayBuffer[LeafStatement]()
    val dataStatements = ArrayBuffer[LeafStatement]()
  }

  class ComponentBuilder(val c : Component){
    val portMaps = ArrayBuffer[String]()
    val declarations = new StringBuilder()
    val logics = new StringBuilder()


    def result : String = {
      val ret = new StringBuilder()
      emitLibrary(ret)
      ret ++= s"\nentity ${c.definitionName} is\n"
      ret ++= s"  port("
      var first = true
      for(portMap <- portMaps){
        if(first){
          ret ++= s"\n    $portMap"
          first = false
        } else {
          ret ++= s",\n    $portMap"
        }
      }
      ret ++= "\n  );\n"

      ret ++= s"end ${c.definitionName};\n"
      ret ++= s"\n"

      ret ++= s"architecture arch of ${c.definitionName} is\n"
      ret ++= declarations
      ret ++= s"begin\n"
      ret ++= logics
      ret ++= s"end arch;\n"
      ret ++= s"\n"
      ret.toString()
    }

    var hash: Integer = null

    override def hashCode(): Int = {
      if (hash == null) {
        hash = declarations.hashCode() + logics.hashCode() + portMaps.foldLeft(0)(_ + _.hashCode)
      }
      hash
    }

    override def equals(obj: scala.Any): Boolean = {
      if (this.hashCode() != obj.hashCode()) return false //Colision into hashmap implementation don't check it XD
      obj match {
        case that: ComponentBuilder => {
          return this.declarations == that.declarations && this.logics == that.logics && (this.portMaps.length == that.portMaps.length && (this.portMaps, that.portMaps).zipped.map(_ == _).reduce(_ && _))
        }
        case _ => return ???
      }
    }
  }

  val wrappedExpressionToName = mutable.HashMap[Expression, String]()
  val referencesOverrides = mutable.HashMap[NameableNode,String]()
  val emitedComponent = mutable.Map[ComponentBuilder, ComponentBuilder]()
  val emitedComponentRef = mutable.Map[Component, Component]()
//  val emitedComponent = mutable.HashSet[ComponentBuilder]()

  def emitComponent(component: Component): String = {
    referencesOverrides.clear()
    wrappedExpressionToName.clear()
    val ret = new StringBuilder()
    val b = new ComponentBuilder(component)
//    emitLibrary(builder)
    emitEntity(component, b)
    emitArchitecture(component, b)


//
    val oldBuilder = emitedComponent.getOrElse(b, null)
    if (oldBuilder == null) {
      emitedComponent += (b -> b)
      return b.result
    } else {
      emitedComponentRef += (component -> oldBuilder.c)
      return s"\n--${component.definitionName} remplaced by ${oldBuilder.c.definitionName}\n\n"
    }


    println(b.declarations.toString + b.logics.toString)
    ""
  }


  def emitEntity(component: Component, builder: ComponentBuilder): Unit = {
    component.getOrdredNodeIo.foreach(baseType =>
      builder.portMaps += s"${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)}${getBaseTypeSignalInitialisation(baseType)}"
    )
  }

  def emitArchitecture(component: Component, b : ComponentBuilder): Unit = {


    val syncGroups = mutable.HashMap[(ClockDomain, ScopeStatement, Boolean), SyncGroup]()
    val asyncStatement = ArrayBuffer[LeafStatement]()

    //Sort all statements into their kinds
    component.dslBody.walkLeafStatements(_ match {
      case s : AssignementStatement => s.finalTarget match {
        case target : BaseType if target.isComb => asyncStatement += s
        case target : BaseType if target.isReg  => {
          val group = syncGroups.getOrElseUpdate((target.dslContext.clockDomain, s.rootScopeStatement, target.hasInit) , new SyncGroup(target.dslContext.clockDomain ,s.rootScopeStatement, target.hasInit))
          s.kind match {
            case AssignementKind.INIT => group.initStatements += s
            case AssignementKind.DATA => group.dataStatements += s
          }
        }
      }
      case assertStatement : AssertStatement => {
        val group = syncGroups.getOrElseUpdate((assertStatement.dslContext.clockDomain, assertStatement.rootScopeStatement, false) , new SyncGroup(assertStatement.dslContext.clockDomain ,assertStatement.rootScopeStatement, false))
        group.dataStatements += assertStatement
      }
    })

    //Generate AsyncProcess per target
    val asyncProcessFromNameableTarget = mutable.HashMap[Nameable,AsyncProcess]()
    val rootTreeStatementPerAsyncProcess = mutable.HashMap[TreeStatement,AsyncProcess]()
    for(s <- asyncStatement) s match{
      case s : AssignementStatement => {
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
          val preExistingRootTreeProcess = rootTreeStatementPerAsyncProcess.getOrElse(rootTreeStatement, null)

          if(preExistingTargetProcess == null && preExistingRootTreeProcess == null){ //Create new process
            val process = new AsyncProcess(rootScope)
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
            //TODO merge to smallest into the bigger (faster)
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets ++= preExistingTargetProcess.nameableTargets
            preExistingTargetProcess.nameableTargets.foreach(asyncProcessFromNameableTarget(_) = process)
          }
        } else {
          val preExistingTargetProcess = asyncProcessFromNameableTarget.getOrElse(finalTarget, null)
          if(preExistingTargetProcess == null) {
            //Create new process
            val process = new AsyncProcess(rootScope)
            asyncProcessFromNameableTarget(finalTarget) = process
            process.nameableTargets = finalTarget :: process.nameableTargets
          }
        }
      }
    }

    //Add statements into AsyncProcesses
    val processes = mutable.HashSet[AsyncProcess]() ++= rootTreeStatementPerAsyncProcess.valuesIterator
    for(s <- asyncStatement) s match {
      case s: AssignementStatement => {
        var process = asyncProcessFromNameableTarget.getOrElse(s.finalTarget,null)
        if(process == null){
          process = new AsyncProcess(s.rootScopeStatement)
          process.nameableTargets = s.finalTarget :: process.nameableTargets
        }
        process.leafStatements += s
        processes += process
      }
    }

    //identify duplicated expression due to `when` spliting/duplication
    //TODO IR include sync stuff

    val expressionToWrap = mutable.HashSet[Expression]();
    {
      val whenCondOccurences = mutable.HashMap[Expression, Int]()


      def walker(statements : ArrayBuffer[LeafStatement], statementIndexInit : Int, scope : ScopeStatement): Int ={
        var statementIndex = statementIndexInit

        while(statementIndex < statements.length){
          val statement = statements(statementIndex)
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
            treeStatement match {
              case w : WhenStatement => {
                if(!w.cond.isInstanceOf[RefExpression]){
                  val counter = whenCondOccurences.getOrElseUpdate(w.cond, 0)
                  if(counter < 2){
                    whenCondOccurences(w.cond) = counter + 1
                  }
                }
              }
            }
            statementIndex = walker(statements,statementIndex, scopePtr)
          }
        }
        return statementIndex
      }
//      def walker(that : TreeStatement, rootScope : ScopeStatement) : Unit = {
//        if(that != null){
//          that match {
//            case w : WhenStatement => {
//              if(!w.cond.isInstanceOf[RefExpression]){
//                val counter = whenCondOccurences.getOrElseUpdate(w.cond, 0)
//                if(counter < 2){
//                  whenCondOccurences(w.cond) = counter + 1
//                }
//              }
//            }
//          }
//          if(that.parentScope != rootScope) {
//            walker(that.parentScope.parentStatement, rootScope)
//          }
//        }
//      }

      for (process <- processes) {
        walker(process.leafStatements, 0, process.scope)
      }

      syncGroups.valuesIterator.foreach(group => {
        walker(group.initStatements, 0, group.scope)
        walker(group.dataStatements, 0, group.scope)
      })

      for ((c, n) <- whenCondOccurences if n > 1) {
        expressionToWrap += c
      }
    }
    //Manage subcomponents input bindings
    val subComponentInputToNotBufferize = mutable.HashSet[Any]()
    for(sub <- component.children){
      for(io <- sub.getAllIo if io.isInput){
        var subInputBinded = isSubComponentInputBinded(io)
        if(subInputBinded != null) {
          def walkInputBindings(that : NameableNode) : NameableNode = { //Manage the case then sub input is drived by sub input
            val next = isSubComponentInputBinded(that.asInstanceOf[BaseType])
            if(next != null && next.component.parent == component)
              walkInputBindings(next)
            else
              that
          }
          referencesOverrides(io) = walkInputBindings(subInputBinded).getName()
          subComponentInputToNotBufferize += io
        } else {
          val name = component.localNamingScope.allocateName(globalData.anonymSignalPrefix)
          b.declarations ++= s"  signal $name : ${emitDataType(io)};\n"
          referencesOverrides(io) = name
        }
      }
    }

    //Check if there is a reference to an output pin
    val outputsToBufferize = mutable.HashSet[BaseType]()
    val subComponentOutputsToNotBufferize = mutable.HashSet[Any]()
    val subComponentOutputsToBufferize = mutable.HashSet[BaseType]()

    //Get all component outputs which are read internaly
    component.dslBody.walkStatements(s => s.walkDrivingExpressions(_ match {
      case RefExpression(bt : BaseType) => {
        if(bt.component == component && bt.isOutput){
          outputsToBufferize += bt
        }
      }
      case _ =>
    }))

    component.dslBody.walkLeafStatements(ls => ls match {
      //Identify direct combinatorial assignements
      case AssignementStatement(RefExpression(target : BaseType),RefExpression(source : BaseType),_)
       if(target.isComb && source.component.parent == component && source.isOutput && target.hasOnlyOneStatement &&  ls.parentScope == target.rootScopeStatement)=> {
        if(subComponentOutputsToNotBufferize.contains(source)){
          subComponentOutputsToBufferize += source
        }
        subComponentOutputsToNotBufferize += source
      }
      //Add all referenced sub component outputs into the KO list
      case _ => ls.walkDrivingExpressions(_ match {
        case RefExpression(source : BaseType) => {
          if(source.component.parent == component && source.isOutput){
            subComponentOutputsToBufferize += source
          }
        }
        case _ =>
      })
    })
    for(syncGroup <- syncGroups.valuesIterator){
      val clockDomain = syncGroup.clockDomain
      val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
      val reset = if (null == clockDomain.reset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
      val softReset = if (null == clockDomain.softReset) null else component.pulledDataCache.getOrElse(clockDomain.softReset, throw new Exception("???")).asInstanceOf[Bool]
      val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
      def addRefUsage(bt: BaseType): Unit ={
        if(bt == null) return
        if(bt.component == component && bt.isOutput){
          outputsToBufferize += bt
        }
        if(bt.component.parent == component && bt.isOutput){
          subComponentOutputsToBufferize += bt
        }
      }
      addRefUsage(clock)
      addRefUsage(reset)
      addRefUsage(softReset)
      addRefUsage(clockEnable)
    }

    subComponentOutputsToNotBufferize --= subComponentOutputsToBufferize

    for(output <- outputsToBufferize){
      val name = component.localNamingScope.allocateName(globalData.anonymSignalPrefix)
      b.declarations ++= s"  signal $name : ${emitDataType(output)};\n"
      b.logics ++= s"  ${emitReference(output, false)} <= $name;\n"
      referencesOverrides(output) = name
    }

    for(subOutput <- subComponentOutputsToBufferize){
      val name = component.localNamingScope.allocateName(globalData.anonymSignalPrefix)
      b.declarations ++= s"  signal $name : ${emitDataType(subOutput)};\n"
      referencesOverrides(subOutput) = name
    }



    //Wrap expression which need it
    for(e <- expressionToWrap){
      val name = component.localNamingScope.allocateName(globalData.anonymSignalPrefix)
      b.declarations ++= s"  signal $name : ${expressionTypeMap(e.getClass)(e)};\n"
      wrappedExpressionToName(e) = name
    }

    for(e <- expressionToWrap){
      b.logics ++= s"  ${wrappedExpressionToName(e)} <= ${emitExpressionNoWrappeForFirstOne(e)};\n"
    }


    //Flush all that mess out ^^
    emitSignals(component,b)
    emitSubComponents(component,b)
    processes.foreach(p => {
      if(p.leafStatements.nonEmpty ) {
        p.leafStatements.head match {
          case AssignementStatement(_, RefExpression(source),_) if subComponentOutputsToNotBufferize.contains(source) =>
          case AssignementStatement(RefExpression(target), _,_) if subComponentInputToNotBufferize.contains(target) =>
          case _ => emitAsyncronous(b, p)
        }
      } else {
        emitAsyncronous(b, p)
      }
    })
    syncGroups.values.foreach(emitSyncronous(component, b, _))
  }


  def emitSubComponents(component: Component, b: ComponentBuilder): Unit = {
    for (children <- component.children) {
//      val isBB = children.isInstanceOf[BlackBox]
//      val isBBUsingULogic = isBB && children.asInstanceOf[BlackBox].isUsingULogic
      val definitionString = /*if (isBB) */children.definitionName
//      else s"entity work.${
//        emitedComponentRef.getOrElse(children, children).definitionName
//      }"
      b.logics ++= s"  ${
        children.getName()
      } : $definitionString\n"



      def addULogicCast(bt: BaseType, io: String, logic: String, dir: IODirection): String = {

//        if (isBBUsingULogic)
//          if (dir == in) {
//            bt match {
//              case _: Bool => return s"      $io => std_ulogic($logic),\n"
////              case _: Bits => return s"      $io => std_ulogic_vector($logic),\n"
//              case _ => return s"      $io => $logic,\n"
//            }
//          } else if (dir == spinal.core.out) {
//            bt match {
//              case _: Bool => return s"      std_logic($io) => $logic,\n"
////              case _: Bits => return s"      std_logic_vector($io) => $logic,\n"
//              case _ => return s"      $io => $logic,\n"
//            }
//          } else SpinalError("???")
//
//        else
          return s"      $io => $logic,\n"
      }

//      if (kind.isInstanceOf[BlackBox]) {
//        val bb = kind.asInstanceOf[BlackBox]
//        val genericFlat = bb.getGeneric.flatten
//
//        if (genericFlat.size != 0) {
//          ret ++= s"    generic map( \n"
//
//
//          for (e <- genericFlat) {
//            e match {
//              case baseType: BaseType => ret ++= addULogicCast(baseType, emitReference(baseType), emitLogic(baseType.getInput(0)), in)
//              case (name : String,s: String) => ret ++= s"      ${name} => ${"\""}${s}${"\""},\n"
//              case (name : String,i : Int) => ret ++= s"      ${name} => $i,\n"
//              case (name : String,d: Double) => ret ++= s"      ${name} => $d,\n"
//              case (name : String,b: Boolean) => ret ++= s"      ${name} => $b,\n"
//              case (name : String,t: TimeNumber) => {
//                val d = t.decompose
//                ret ++= s"      ${name} => ${d._1} ${d._2},\n"
//              }
//            }
//          }
//          ret.setCharAt(ret.size - 2, ' ')
//          ret ++= s"    )\n"
//        }
//      }
      b.logics ++= s"    port map ( \n"
      for (data <- children.getOrdredNodeIo) {
        b.logics ++= addULogicCast(data, emitReferenceNoOverrides(data), emitReference(data, false), data.dir)
      }
      b.logics.setCharAt(b.logics.size - 2, ' ')

      b.logics ++= s"    );"
      b.logics ++= s"\n"
    }
  }

  def isSubComponentInputBinded(data : BaseType) = {
    if(data.isInput && data.isComb && data.hasOnlyOneStatement && data.headStatement.parentScope == data.rootScopeStatement && Statement.isFullToFullStatement(data.headStatement))
      data.headStatement.source.asInstanceOf[RefExpression].source
    else
      null
  }

//  val isSubComponentOutputBindedSet = mutable.HashSet[BaseType]()
//  def isSubComponentOutputBinded(data : BaseType) =
//    isSubComponentOutputBindedSet.contains(data)

  def emitSyncronous(component : Component, b : ComponentBuilder, group: SyncGroup): Unit = {
    import group._
    def withReset = hasInit

    val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
    val reset = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
    val softReset = if (null == clockDomain.softReset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.softReset, throw new Exception("???")).asInstanceOf[Bool]
    val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]

//    val clock = clockDomain.clock
//    val reset = if (null == clockDomain.reset || !withReset) null else clockDomain.reset
//    val softReset = if (null == clockDomain.softReset || !withReset) null else clockDomain.softReset
//    val clockEnable = if (null == clockDomain.clockEnable) null else clockDomain.clockEnable

    val asyncReset = (null != reset) && clockDomain.config.resetKind == ASYNC
    val syncReset = (null != reset) && clockDomain.config.resetKind == SYNC
    var tabLevel = 1
    def tabStr = "  " * tabLevel
    def inc = {
      tabLevel = tabLevel + 1
    }
    def dec = {
      tabLevel = tabLevel - 1
    }



    val initialStatlementsGeneration =  new StringBuilder()
    referenceSet = mutable.Set[String]()
    emitRegsInitialValue("      ", initialStatlementsGeneration)
    referenceSet += emitReference(clock,false)

    if (asyncReset) {
      referenceSet += emitReference(reset,false)
      b.logics ++= s"${tabStr}process(${referenceSet.mkString(", ")})\n"
    } else {
      b.logics ++= s"${tabStr}process(${referenceSet.mkString(", ")})\n"
    }

    b.logics ++= s"${tabStr}begin\n"
    inc
    if (asyncReset) {
      b.logics ++= s"${tabStr}if ${emitReference(reset, false)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\' then\n";
      inc
      b.logics ++= initialStatlementsGeneration
      dec
      b.logics ++= s"${tabStr}elsif ${emitClockEdge(emitReference(clock,false), clockDomain.config.clockEdge)}"
      inc
    } else {
      b.logics ++= s"${tabStr}if ${emitClockEdge(emitReference(clock,false), clockDomain.config.clockEdge)}"
      inc
    }
    if (clockEnable != null) {
      b.logics ++= s"${tabStr}if ${emitReference(clockEnable,false)} = \'${if (clockDomain.config.clockEnableActiveLevel == HIGH) 1 else 0}\' then\n"
      inc
    }

    if (syncReset || softReset != null) {
      var condList = ArrayBuffer[String]()
      if(syncReset) condList += s"${emitReference(reset,false)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\'"
      if(softReset != null) condList += s"${emitReference(softReset,false)} = \'${if (clockDomain.config.softResetActiveLevel == HIGH) 1 else 0}\'"

      b.logics ++= s"${tabStr}if ${condList.reduce(_ + " or " + _)} then\n"
      inc
      b.logics ++= initialStatlementsGeneration
      dec
      b.logics ++= s"${tabStr}else\n"
      inc
      emitRegsLogic(tabStr,b.logics)
      dec
      b.logics ++= s"${tabStr}end if;\n"
      dec
    } else {
      emitRegsLogic(tabStr,b.logics)
      dec
    }

    while (tabLevel != 1) {
      b.logics ++= s"${tabStr}end if;\n"
      dec
    }
    b.logics ++= s"${tabStr}end process;\n"
    dec
    b.logics ++= s"${tabStr}\n"


    def emitRegsInitialValue(tab: String, b : StringBuilder): Unit = {
      emitLeafStatements(group.initStatements, 0, group.scope, "<=", b , tab)
    }
    def emitRegsLogic(tab: String, b : StringBuilder): Unit = {
      emitLeafStatements(group.dataStatements, 0, group.scope, "<=", b , tab)
    }



  }

  def emitAsyncronous(b : ComponentBuilder, process: AsyncProcess): Unit = {
    process match {
      case _ if process.leafStatements.size == 1 && process.leafStatements.head.parentScope == process.nameableTargets.head.rootScopeStatement => process.leafStatements.head match {
        case s : AssignementStatement =>
          b.logics ++= emitAssignement(s, "  ", "<=")
      }
      case _ => {
        val tmp = new StringBuilder
        referenceSet = mutable.Set[String]()
        emitLeafStatements(process.leafStatements, 0, process.scope, "<=", tmp, "    ")

        if (referenceSet.nonEmpty) {
          b.logics ++= s"  process(${referenceSet.mkString(",")})\n"; referenceSet = null
          b.logics ++= "  begin\n"
          b.logics ++= tmp.toString()
          b.logics ++= "  end process;\n\n"
        } else {
          //TODO IR ! process without sensitivity !
          ???
        }
      }
    }
  }


  def emitLeafStatements(statements : ArrayBuffer[LeafStatement], statementIndexInit : Int, scope : ScopeStatement, assignementKind : String, b : StringBuilder, tab : String): Int ={
    var statementIndex = statementIndexInit
    var lastWhen : WhenStatement = null
    def closeSubs() : Unit = {
      if(lastWhen != null) {
        b ++= s"${tab}end if;\n"
        lastWhen = null
      }
    }
    while(statementIndex < statements.length){
      val leaf = statements(statementIndex)
      val statement = leaf
      val targetScope = statement.parentScope
      if(targetScope == scope){
        closeSubs()
        statement match {
          case assignement : AssignementStatement => b ++= emitAssignement(assignement,tab, assignementKind)
          case assertStatement : AssertStatement => {
            val cond = emitExpression(assertStatement.cond)
            require(assertStatement.message.size == 0 || (assertStatement.message.size == 1 && assertStatement.message(0).isInstanceOf[String]))
            val message = if(assertStatement.message.size == 1) s"""report "${assertStatement.message(0)}" """ else ""
            val severity = "severity " +  (assertStatement.severity match{
              case `NOTE`     => "NOTE"
              case `WARNING`  => "WARNING"
              case `ERROR`    => "ERROR"
              case `FAILURE`  => "FAILURE"
            })
            b ++= s"${tab}assert $cond = '1' $message $severity;\n"
          }
        }

        statementIndex += 1
      } else {
        var scopePtr = targetScope
        while(scopePtr.parentStatement != null && scopePtr.parentStatement.parentScope != scope){
          scopePtr = scopePtr.parentStatement.parentScope
        }
        if(scopePtr.parentStatement == null) {
          closeSubs()
          return statementIndex
        }
        val treeStatement = scopePtr.parentStatement
        if(treeStatement != lastWhen)
          closeSubs()
        treeStatement match {
          case treeStatement : WhenStatement => {
            if(scopePtr == treeStatement.whenTrue){
              b ++= s"${tab}if ${emitExpression(treeStatement.cond)} = '1' then\n"
            } else if(lastWhen == treeStatement){
//              if(scopePtr.sizeIsOne && scopePtr.head.isInstanceOf[WhenStatement]){
//                b ++= s"${tab}if ${emitExpression(treeStatement.cond)} = '1' then\n"
//              } else {
                b ++= s"${tab}else\n"
//              }
            } else {
              b ++= s"${tab}if ${emitExpression(treeStatement.cond)} = '0' then\n"
            }
            lastWhen = treeStatement
          }
        }

        statementIndex = emitLeafStatements(statements,statementIndex, scopePtr, assignementKind,b, tab + "  ")
      }
    }
    closeSubs()
    return statementIndex
  }
//        for (process <- processList if !process.needProcessDef) {
//          for (node <- process.nodes) {
//            emitAssignement(node, node.getInput(0), ret, "  ", "<=")
//          }
//        }
//
//        referenceSet = mutable.Set[Node with Nameable with ContextUser]()
//        for (process <- processList if process.needProcessDef) {
//          process.genSensitivity
//
//          val context = new AssignementLevel(process.nodes.map(n => AssignementLevelCmd(n,n.getInput(0))))
//
//          if (process.sensitivity.size != 0) {
//            val tmp = new StringBuilder
//            referenceSet.clear()
//            emitAssignementLevel(context,tmp, "    ", "<=",false,process.sensitivity)
//
//            //TODO sensitivity list generation no more required
//    //        ret ++= s"  process(${process.sensitivity.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
//            ret ++= s"  process(${referenceSet.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
//            ret ++= "  begin\n"
//            ret ++= tmp
//            ret ++= "  end process;\n\n"
//          } else {
//            //emit func as logic
//            assert(process.nodes.size == 1)
//            for (node <- process.nodes) {
//              val funcName = "zz_" + emitReference(node)
//              funcRet ++= emitFuncDef(funcName, node, context)
//              ret ++= s"  ${emitReference(node)} <= ${funcName};\n"
//              //          ret ++= s"  ${emitReference(node)} <= ${emitLogic(node.getInput(0))};\n"
//            }
//          }
//        }
//        referenceSet = null


  def emitAssignement(assignement : AssignementStatement, tab: String, assignementKind: String): String = {
    assignement match {
//      case from: AssignementNode => {
//        from match {
//          case assign: BitAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getBitId}) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: BitAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}(to_integer(${emitLogic(assign.getBitId)})) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getHi} downto ${assign.getLo}) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getBitCount.value - 1} + to_integer(${emitLogic(assign.getOffset)}) downto to_integer(${emitLogic(assign.getOffset)})) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//        }
//      }
      case _ => {
        val referenceSetBack = referenceSet
        referenceSet = null
        val targetStr = emitExpression(assignement.target)
        referenceSet = referenceSetBack
        s"$tab${targetStr} ${assignementKind} ${emitExpression(assignement.source)};\n"
      }
    }
  }
  var referenceSet : mutable.Set[String] = null
  def emitReference(that : NameableNode, sensitive : Boolean): String ={
    val name = referencesOverrides.getOrElse(that, that.getNameElseThrow)
    if(sensitive && referenceSet != null) referenceSet.add(name)
    name
  }

  def emitReferenceNoOverrides(that : NameableNode): String ={
    that.getNameElseThrow
  }


  def emitExpression(that : Expression) : String = {
    wrappedExpressionToName.get(that) match {
      case Some(name) => {
        if(referenceSet != null) referenceSet.add(name)
        name
      }
      case None => expressionImplMap.getOrElse(that.getClass, throw new Exception("can't find " + that.opName))(that)
    }
  }

  def emitExpressionNoWrappeForFirstOne(that : Expression) : String = {
    expressionImplMap.getOrElse(that.getClass, throw new Exception("can't find " + that.opName))(that)
  }
  //
  //    case lit: BitsLiteral => s"pkg_stdLogicVector(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //    case lit: UIntLiteral => s"pkg_unsigned(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //    case lit: SIntLiteral => s"pkg_signed(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //
  //    case lit: BitsAllToLiteral => lit.theConsumer match {
  //      case _: Bits => s"pkg_stdLogicVector(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //      case _: UInt => s"pkg_unsigned(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //      case _: SInt => s"pkg_signed(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
  //    }
  //    case lit: BoolLiteral => s"pkg_toStdLogic(${lit.value})"
  //    //  case lit: BoolLiteral => if(lit.value) "'1'" else "'0'" //Invalid VHDL when '1' = '1'
  //    case lit: EnumLiteral[_] => emitEnumLiteral(lit.enum, lit.encoding)
  //    case memRead: MemReadAsync => {
  //      if(memRead.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memRead.getMem} because of its mixed width ports")
  //      if (memRead.readUnderWrite == dontCare) SpinalWarning(s"memReadAsync with dontCare is as writeFirst into VHDL")
  //      val symbolCount = memRead.getMem.getMemSymbolCount
  //      if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
  //        s"${emitReference(memRead.getMem)}(to_integer(${emitReference(memRead.getAddress)}))"
  //      else
  //        (0 until symbolCount).reverse.map(i => (s"${emitReference(memRead.getMem)}_symbol$i(to_integer(${emitReference(memRead.getAddress)}))")).reduce(_ + " & " + _)
  //    }
  //    case whenNode: WhenNode => s"pkg_mux(${whenNode.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})" //Exeptional case with asyncrouns of literal
  //    case dc : DontCareNodeEnum => {
  //      if(dc.encoding.isNative)
  //        return "pkg_enum." + dc.enum.elements.head.getName()
  //      else
  //        return s"(${'"'}${"-" * dc.encoding.getWidth(dc.enum)}${'"'})"
  //    }
  //    case dc: DontCareNode => {
  //      dc.getBaseType match {
  //        case to: Bool => s"'-'"
  //        case to: BitVector => s"(${'"'}${"-" * dc.asInstanceOf[Widthable].getWidth}${'"'})"
  //      }
//      }


    //case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
 // }

//  override def useNodeConsumers: Boolean = true
//
//  var outFile: java.io.FileWriter = null
//  var memBitsMaskKind : MemBitsMaskKind = MULTIPLE_RAM
//
//  val emitedComponent = mutable.Map[ComponentBuilder, ComponentBuilder]()
//  val emitedComponentRef = mutable.Map[Component, Component]()
//
//
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    SpinalProgress("Write VHDL")
//
//    outFile = new java.io.FileWriter(pc.config.targetDirectory + "/" +  (if(pc.config.netlistFileName == null)(topLevel.definitionName + ".vhd") else pc.config.netlistFileName))
//    outFile.write(VhdlVerilogBase.getHeader("--",topLevel))
//    emitEnumPackage(outFile)
//    if(pc.config.genVhdlPkg)
//      emitPackage(outFile)
//
//    for (c <- sortedComponents) {
//      if (!c.isInBlackBoxTree) {
//        SpinalProgress(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
//        compile(c)
//      }
//    }
//
//    outFile.flush();
//    outFile.close();
//  }
//
//
//

//
//
//
//  case class WrappedStuff(originalName: String, newName: String)
//
//  def ioStdLogicVectorWrapNames(): HashMap[BaseType, WrappedStuff] = {
//    val map = HashMap[BaseType, WrappedStuff]()
//    def wrap(that: BaseType): Unit = {
//      val newName = that.getName() + "_wrappedName"
//      map(that) = WrappedStuff(that.getName, newName)
//      that.setName(newName)
//    }
//    for (e <- topLevel.getOrdredNodeIo) e match {
//      case e: UInt => wrap(e)
//      case e: SInt => wrap(e)
//      case _ =>
//    }
//    map
//  }
//
//  def ioStdLogicVectorRestoreNames(map: HashMap[BaseType, WrappedStuff]): Unit = {
//    if (map != null) {
//      for ((e, w) <- map) e.setName(w.originalName)
//    }
//  }
//
//  def emit(component: Component): String = {
//    val ret = new StringBuilder()
//    val builder = new ComponentBuilder(component)
//
//    emitLibrary(builder)
//    emitEntity(component, builder)
//    emitArchitecture(component, builder)
//
//    val oldBuilder = emitedComponent.getOrElse(builder, null)
//    if (oldBuilder == null) {
//      emitedComponent += (builder -> builder)
//      return builder.result
//    } else {
//      emitedComponentRef += (component -> oldBuilder.component)
//      return s"\n--${component.definitionName} remplaced by ${oldBuilder.component.definitionName}\n\n"
//    }
//  }
//
//  def getReEncodingFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding, target: SpinalEnumEncoding): String = {
//    s"${spinalEnum.getName()}_${source.getName()}_to_${target.getName()}"
//  }
//
//  def getEnumToDebugFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding): String = {
//    assert(!source.isNative)
//    s"${spinalEnum.getName()}_${source.getName()}_to_debug"
//  }
//
//  def getEnumDebugType(spinalEnum: SpinalEnum): String = {
//    s"${spinalEnum.getName()}_debug"
//  }
//
//  def emitEnumPackage(out: java.io.FileWriter): Unit = {
//    val ret = new StringBuilder();
//    ret ++=
//      s"""library IEEE;
//         |use IEEE.STD_LOGIC_1164.ALL;
//         |use IEEE.NUMERIC_STD.all;
//         |
//         |package $enumPackageName is
//                                    |""".stripMargin
//    for (enumDef <- enums.keys) {
//      ret ++= s"  type ${enumDef.getName()} is (${enumDef.elements.map(_.getName()).reduce(_ + "," + _)});\n"
//      ret ++= s"  type ${getEnumDebugType(enumDef)} is (${enumDef.elements.foldLeft("XXX")((str, e) => str + "," + e.getName())});\n"
//    }
//
//    ret ++= "\n"
//    for ((enumDef, encodings) <- enums) {
//      val enumName = enumDef.getName()
//      ret ++= s"  function pkg_mux (sel : std_logic;one : $enumName;zero : $enumName) return $enumName;\n"
//
//
//      for (encoding <- encodings if !encoding.isNative) {
//        val encodingName = encoding.getName()
//        val bitCount = encoding.getWidth(enumDef)
//        val vhdlEnumType = emitEnumType(enumDef, encoding)
//        ret ++= s"  subtype $vhdlEnumType is std_logic_vector(${bitCount - 1} downto 0);\n"
//        for (element <- enumDef.elements) {
//          ret ++= s"  constant ${emitEnumLiteral(element, encoding)} : $vhdlEnumType := ${idToBits(element, encoding)};\n"
//        }
//        ret ++= "\n"
//        //ret ++= s"  function pkg_to${enumName}_debug (value : std_logic_vector) return $enumName;\n"
//      }
//      for (encoding <- encodings) {
//        if (!encoding.isNative)
//          ret ++= s"  function ${getEnumToDebugFuntion(enumDef, encoding)} (value : ${emitEnumType(enumDef, encoding)}) return ${getEnumDebugType(enumDef)};\n"
//        else {
//          ret ++= s"  function pkg_toStdLogicVector_${encoding.getName()} (value : $enumName) return std_logic_vector;\n"
//          ret ++= s"  function pkg_to${enumName}_${encoding.getName()} (value : std_logic_vector(${encoding.getWidth(enumDef) - 1} downto 0)) return $enumName;\n"
//        }
//
//        for (targetEncoding <- encodings if targetEncoding != encoding && !(targetEncoding.isNative && encoding.isNative)) {
//          ret ++= s"  function ${getReEncodingFuntion(enumDef, encoding, targetEncoding)} (that : ${emitEnumType(enumDef, encoding)}) return ${emitEnumType(enumDef, targetEncoding)};\n"
//        }
//      }
//    }
//
//    def idToBits[T <: SpinalEnum](enum: SpinalEnumElement[T], encoding: SpinalEnumEncoding): String = {
//      val str = encoding.getValue(enum).toString(2)
//      "\"" + ("0" * (encoding.getWidth(enum.spinalEnum) - str.length)) + str + "\""
//    }
//
//
//    ret ++= s"end $enumPackageName;\n\n"
//    if (enums.size != 0) {
//      ret ++= s"package body $enumPackageName is\n"
//      for ((enumDef, encodings) <- enums) {
//        val enumName = enumDef.getName()
//        ret ++= s"  function pkg_mux (sel : std_logic;one : $enumName;zero : $enumName) return $enumName is\n"
//        ret ++= "  begin\n"
//        ret ++= "    if sel = '1' then\n"
//        ret ++= "      return one;\n"
//        ret ++= "    else\n"
//        ret ++= "      return zero;\n"
//        ret ++= "    end if;\n"
//        ret ++= "  end pkg_mux;\n\n"
//
//
//
//        for (encoding <- encodings) {
//          if (!encoding.isNative)
//            ret ++=
//              s"""  function ${getEnumToDebugFuntion(enumDef, encoding)} (value : ${emitEnumType(enumDef, encoding)}) return ${getEnumDebugType(enumDef)} is
//                                                                                                                                                           |  begin
//                                                                                                                                                           |    case value is
//                                                                                                                                                           |${
//                {
//                  for (e <- enumDef.elements) yield s"      when ${emitEnumLiteral(e, encoding)} => return ${e.getName()};"
//                }.reduce(_ + "\n" + _)
//              }
//                  |      when others => return XXX;
//                  |    end case;
//                  |  end;
//                  |""".stripMargin
//          else {
//            ret ++=
//              s"""  function pkg_to${enumName}_${encoding.getName()} (value : std_logic_vector(${encoding.getWidth(enumDef) - 1} downto 0)) return $enumName is
//                                                                                                                                                              |  begin
//                                                                                                                                                              |    case value is
//                                                                                                                                                              |${
//                {
//                  for (e <- enumDef.elements) yield s"      when ${idToBits(e, encoding)} => return ${e.getName()};"
//                }.reduce(_ + "\n" + _)
//              }
//                  |      when others => return ${enumDef.elements.head.getName()};
//                                                                                 |    end case;
//                                                                                 |  end;
//                                                                                 |""".stripMargin
//
//            ret ++=
//              s"""  function pkg_toStdLogicVector_${encoding.getName()} (value : $enumName) return std_logic_vector is
//                                                                                            |  begin
//                                                                                            |    case value is
//                                                                                            |${
//                {
//                  for (e <- enumDef.elements) yield s"      when ${e.getName()} => return ${idToBits(e, encoding)};"
//                }.reduce(_ + "\n" + _)
//              }
//                  |      when others => return ${idToBits(enumDef.elements.head, encoding)};
//                                                                                           |    end case;
//                                                                                           |  end;
//                                                                                           |""".stripMargin
//          }
//
//
//
//          for (targetEncoding <- encodings if targetEncoding != encoding) {
//            ret ++= s"  function ${getReEncodingFuntion(enumDef, encoding, targetEncoding)} (that : ${emitEnumType(enumDef, encoding)}) return ${emitEnumType(enumDef, targetEncoding)} is\n"
//            ret ++= "  begin\n"
//            ret ++= "    case that is \n"
//            for (e <- enumDef.elements) {
//              ret ++= s"      when ${emitEnumLiteral(e, encoding)} => return ${emitEnumLiteral(e, targetEncoding)};\n"
//            }
//            ret ++= s"      when others => return ${emitEnumLiteral(enumDef.elements.head, targetEncoding)};\n"
//            ret ++= "    end case;\n"
//            ret ++= "  end;\n\n"
//          }
//        }
//      }
//      ret ++= s"end $enumPackageName;\n\n\n"
//    }
//    out.write(ret.result())
//  }
//
//  def emitPackage(out: java.io.FileWriter): Unit = {
//
//    def pkgExtractBool(kind: String): Tuple2[String, String] = {
//      val ret = new StringBuilder();
//      (s"function pkg_extract (that : $kind; bitId : integer) return std_logic", {
//        ret ++= "  begin\n"
//        ret ++= "    return that(bitId);\n"
//        ret ++= "  end pkg_extract;\n\n"
//        ret.result()
//      })
//    }
//    def pkgExtract(kind: String): Tuple2[String, String] = {
//      val ret = new StringBuilder();
//      (s"function pkg_extract (that : $kind; base : unsigned; size : integer) return $kind", {
//        ret ++= "   constant elementCount : integer := (that'length-size)+1;\n"
//        ret ++= s"   type tableType is array (0 to elementCount-1) of $kind(size-1 downto 0);\n"
//        ret ++= "   variable table : tableType;\n"
//        ret ++= "  begin\n"
//        ret ++= "    for i in 0 to elementCount-1 loop\n"
//        ret ++= "      table(i) := that(i + size - 1 downto i);\n"
//        ret ++= "    end loop;\n"
//        ret ++= "    return table(to_integer(base));\n"
//        ret ++= "  end pkg_extract;\n\n"
//        ret.result()
//      })
//    }
//
//
//    def pkgCat(kind: String): Tuple2[String, String] = {
//      val ret = new StringBuilder();
//      (s"function pkg_cat (a : $kind; b : $kind) return $kind", {
//        ret ++= s"    variable cat : $kind(a'length + b'length-1 downto 0);\n"
//        ret ++= s"  begin\n"
//        ret ++= s"    cat := a & b;\n"
//        ret ++= s"    return cat;\n"
//        ret ++= s"  end pkg_cat;\n\n"
//        ret.result()
//      })
//    }
//
//
//    val vectorTypes = "std_logic_vector" :: "unsigned" :: "signed" :: Nil
//    val funcs = ArrayBuffer[Tuple2[String, String]]()
//    vectorTypes.foreach(kind => {
//      funcs += pkgExtractBool(kind)
//      funcs += pkgExtract(kind)
//      funcs += pkgCat(kind)
//    })
//
//
//
//    val ret = new StringBuilder();
//    ret ++= s"library IEEE;\n"
//    ret ++= "use ieee.std_logic_1164.all;\n"
//    ret ++= "use ieee.numeric_std.all;\n"
//    ret ++= "use ieee.math_real.all;\n"
//    ret ++= "\n"
//    ret ++= s"package $packageName is\n"
//    ret ++= s"${funcs.map("  " + _._1 + ";\n").reduce(_ + _)}\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic;\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector;\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned;\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed;\n"
//    ret ++= s"\n"
//    ret ++= s"\n"
//    ret ++= "  function pkg_toStdLogic (value : boolean) return std_logic;\n"
//    ret ++= "  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector;\n"
//    ret ++= "  function pkg_toUnsigned(value : std_logic) return unsigned;\n"
//    ret ++= "  function pkg_toSigned (value : std_logic) return signed;\n"
//    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector) return std_logic_vector;\n"
//    ret ++= "  function pkg_unsigned (lit : unsigned) return unsigned;\n"
//    ret ++= "  function pkg_signed (lit : signed) return signed;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector;\n"
//    ret ++= "  function pkg_resize (that : unsigned; width : integer) return unsigned;\n"
//    ret ++= "  function pkg_resize (that : signed; width : integer) return signed;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector;\n"
//    ret ++= "  function pkg_extract (that : unsigned; high : integer; low : integer) return unsigned;\n"
//    ret ++= "  function pkg_extract (that : signed; high : integer; low : integer) return signed;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector;\n"
//    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
//    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector;\n"
//    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : unsigned; size : natural) return unsigned;\n"
//    ret ++= "  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned;\n"
//    ret ++= "  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned;\n"
//    ret ++= "  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : signed; size : natural) return signed;\n"
//    ret ++= "  function pkg_shiftRight (that : signed; size : unsigned) return signed;\n"
//    ret ++= "  function pkg_shiftLeft (that : signed; size : natural) return signed;\n"
//    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned) return signed;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_rotateLeft (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
//    ret ++= s"end  $packageName;\n"
//    ret ++= "\n"
//    ret ++= s"package body $packageName is\n"
//    ret ++= s"${funcs.map(f => "  " + f._1 + " is\n" + f._2 + "\n").reduce(_ + _)}"
//    ret ++= "\n"
//    ret ++= "  -- unsigned shifts\n"
//    ret ++= "  function pkg_shiftRight (that : unsigned; size : natural) return unsigned is\n"
//    ret ++= "  begin\n"
//    ret ++= "    if size >= that'length then\n"
//    ret ++= "      return \"\";\n"
//    ret ++= "    else\n"
//    ret ++= "      return shift_right(that,size)(that'high-size downto 0);\n"
//    ret ++= "    end if;\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return shift_right(that,to_integer(size));\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return shift_left(resize(that,that'length + size),size);\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return shift_left(resize(that,that'length + 2**size'length - 1),to_integer(size));\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "\n"
//    ret ++= "  -- std_logic_vector shifts\n"
//    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(pkg_shiftRight(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(pkg_shiftRight(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "  -- signed shifts\n"
//    ret ++= "  function pkg_shiftRight (that : signed; size : natural) return signed is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return signed(pkg_shiftRight(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftRight (that : signed; size : unsigned) return signed is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return shift_right(that,to_integer(size));\n"
//    ret ++= "  end pkg_shiftRight;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : signed; size : natural) return signed is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return signed(pkg_shiftLeft(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned) return signed is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return signed(pkg_shiftLeft(unsigned(that),size));\n"
//    ret ++= "  end pkg_shiftLeft;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_rotateLeft (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(rotate_left(unsigned(that),to_integer(size)));\n"
//    ret ++= "  end pkg_rotateLeft;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector is\n"
//    ret ++= "    variable temp : std_logic_vector(high-low downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    temp := that(high downto low);\n"
//    ret ++= "    return temp;\n"
//    ret ++= "  end pkg_extract;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_extract (that : unsigned; high : integer; low : integer) return unsigned is\n"
//    ret ++= "    variable temp : unsigned(high-low downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    temp := that(high downto low);\n"
//    ret ++= "    return temp;\n"
//    ret ++= "  end pkg_extract;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_extract (that : signed; high : integer; low : integer) return signed is\n"
//    ret ++= "    variable temp : signed(high-low downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    temp := that(high downto low);\n"
//    ret ++= "    return temp;\n"
//    ret ++= "  end pkg_extract;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic is\n"
//    ret ++= "  begin\n"
//    ret ++= "    if sel = '1' then\n"
//    ret ++= "      return one;\n"
//    ret ++= "    else\n"
//    ret ++= "      return zero;\n"
//    ret ++= "    end if;\n"
//    ret ++= "  end pkg_mux;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector is\n"
//    ret ++= "    variable ret : std_logic_vector(zero'range);"
//    ret ++= "  begin\n"
//    ret ++= "    if sel = '1' then\n"
//    ret ++= "      ret := one;\n"
//    ret ++= "    else\n"
//    ret ++= "      ret := zero;\n"
//    ret ++= "    end if;\n"
//    ret ++= "    return ret;"
//    ret ++= "  end pkg_mux;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned is\n"
//    ret ++= "    variable ret : unsigned(zero'range);"
//    ret ++= "  begin\n"
//    ret ++= "    if sel = '1' then\n"
//    ret ++= "      ret := one;\n"
//    ret ++= "    else\n"
//    ret ++= "      ret := zero;\n"
//    ret ++= "    end if;\n"
//    ret ++= "    return ret;"
//    ret ++= "  end pkg_mux;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed is\n"
//    ret ++= "    variable ret : signed(zero'range);"
//    ret ++= "  begin\n"
//    ret ++= "    if sel = '1' then\n"
//    ret ++= "      ret := one;\n"
//    ret ++= "    else\n"
//    ret ++= "      ret := zero;\n"
//    ret ++= "    end if;\n"
//    ret ++= "    return ret;"
//    ret ++= "  end pkg_mux;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_toStdLogic (value : boolean) return std_logic is\n"
//    ret ++= "  begin\n"
//    ret ++= "    if value = true then\n"
//    ret ++= "      return '1';\n"
//    ret ++= "    else\n"
//    ret ++= "      return '0';\n"
//    ret ++= "    end if;\n"
//    ret ++= "  end pkg_toStdLogic;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector is\n"
//    ret ++= "    variable ret : std_logic_vector(0 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret(0) := value;\n"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_toStdLogicVector;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_toUnsigned (value : std_logic) return unsigned is\n"
//    ret ++= "    variable ret : unsigned(0 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret(0) := value;\n"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_toUnsigned;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_toSigned (value : std_logic) return signed is\n"
//    ret ++= "    variable ret : signed(0 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret(0) := value;\n"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_toSigned;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector) return std_logic_vector is\n"
//    ret ++= "    variable ret : std_logic_vector(lit'length-1 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret := lit;"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_stdLogicVector;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_unsigned (lit : unsigned) return unsigned is\n"
//    ret ++= "    variable ret : unsigned(lit'length-1 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret := lit;"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_unsigned;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_signed (lit : signed) return signed is\n"
//    ret ++= "    variable ret : signed(lit'length-1 downto 0);\n"
//    ret ++= "  begin\n"
//    ret ++= "    ret := lit;"
//    ret ++= "    return ret;\n"
//    ret ++= "  end pkg_signed;\n"
//    ret ++= "\n"
//    ret ++= "  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector is\n"
//    ret ++= "  begin\n"
//    ret ++= "    return std_logic_vector(resize(unsigned(that),width));\n"
//    ret ++= "  end pkg_resize;\n"
//    ret ++= "\n"
//    ret ++=
//      """
//        |  function pkg_resize (that : unsigned; width : integer) return unsigned is
//        |	  variable ret : unsigned(width-1 downto 0);
//        |  begin
//        |    if that'length = 0 then
//        |       ret := (others => '0');
//        |    else
//        |       ret := resize(that,width);
//        |    end if;
//        |		return ret;
//        |  end pkg_resize;
//        | """.stripMargin
//
//
//    ret ++=
//      """
//        |  function pkg_resize (that : signed; width : integer) return signed is
//        |	  variable ret : signed(width-1 downto 0);
//        |  begin
//        |    if that'length = 0 then
//        |       ret := (others => '0');
//        |    elsif that'length >= width then
//        |       ret := that(width-1 downto 0);
//        |    else
//        |       ret := resize(that,width);
//        |    end if;
//        |		return ret;
//        |  end pkg_resize;
//        | """.stripMargin
//    ret ++= s"end $packageName;\n"
//    ret ++= "\n"
//    ret ++= "\n"
//
//    out.write(ret.result())
//  }
//
//  def emitLibrary(builder: ComponentBuilder): Unit = {
//    val ret = builder.newPart(true)
//    emitLibrary(ret)
//  }
//
//
//
//  def emitEntity(component: Component, builder: ComponentBuilder): Unit = {
//    var ret = builder.newPart(false)
//    ret ++= s"\nentity ${component.definitionName} is\n"
//    ret = builder.newPart(true)
//    ret ++= s"  port( \n"
//    if (!(config.onlyStdLogicVectorAtTopLevelIo && component == topLevel)) {
//      component.getOrdredNodeIo.foreach(baseType =>
//        ret ++= s"    ${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)}${getBaseTypeSignalInitialisation(baseType)};\n"
//      )
//    } else {
//      component.getOrdredNodeIo.foreach(baseType => {
//        val originalType = emitDataType(baseType)
//        val correctedType = originalType.replace("unsigned", "std_logic_vector").replace("signed", "std_logic_vector")
//        ret ++= s"    ${baseType.getName()} : ${emitDirection(baseType)} ${correctedType};\n"
//      })
//    }
//    /*component.getOrdredNodeIo.foreach(baseType =>
//      ret ++= s"    ${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)};\n"
//    )*/
//    ret.setCharAt(ret.size - 2, ' ')
//    ret ++= s"  );\n"
//    ret = builder.newPart(false)
//    ret ++= s"end ${component.definitionName};\n"
//    ret ++= s"\n"
//
//  }
//
//
//  def emitArchitecture(component: Component, builder: ComponentBuilder): Unit = {
////    val buffer = ArrayBuffer[AssignementLevelCmd]()
////    for(node <- component.nodes){
////      node match{
////        case node : BaseType => {
////          if( (!((node.isIo && node.isInput) || component.kindsOutputsBindings.contains(node))))
////            buffer += AssignementLevelCmd(node,node.getInput(0))
////        }
////        case _ =>
////      }
////    }
////    val context = new AssignementLevel(buffer)
//
//    var ret = builder.newPart(false)
//    val wrappedIo = if (config.onlyStdLogicVectorAtTopLevelIo && component == topLevel) ioStdLogicVectorWrapNames() else HashMap[BaseType, WrappedStuff]()
//    ret ++= s"architecture arch of ${component.definitionName} is\n"
//    ret = builder.newPart(true)
//    emitBlackBoxComponents(component, ret)
//    emitAttributesDef(component, ret)
//    val enumDebugSignals = ArrayBuffer[SpinalEnumCraft[_]]()
//    emitSignals(component, ret, enumDebugSignals)
//    emitWrappedIoSignals(ret, wrappedIo)
//    val retTemp = new StringBuilder
//    retTemp ++= s"begin\n"
//    emitComponentInstances(component, retTemp)
//    emitAsyncronous(component, retTemp, ret)
//    emitSyncronous(component, retTemp)
//    emitWrappedIoConnection(retTemp, wrappedIo)
//    emitDebug(component, retTemp, enumDebugSignals)
//    retTemp ++= s"end arch;\n"
//    retTemp ++= s"\n"
//
//    ioStdLogicVectorRestoreNames(wrappedIo)
//
//    ret ++= retTemp
//
//  }
//
//  def emitWrappedIoSignals(buff: StringBuilder, map: HashMap[BaseType, WrappedStuff]): Unit = {
//    for ((e, w) <- map) {
//      buff ++= s"  signal ${w.newName} : ${emitDataType(e)};\n"
//    }
//  }
//
//  def emitWrappedIoConnection(buff: StringBuilder, map: HashMap[BaseType, WrappedStuff]): Unit = {
//    for ((e, w) <- map) e.dir match {
//      case spinal.core.in => e match {
//        case e: UInt => buff ++= s"  ${w.newName} <= unsigned(${w.originalName});\n"
//        case e: SInt => buff ++= s"  ${w.newName} <= signed(${w.originalName});\n"
//      }
//      case spinal.core.out => buff ++= s"  ${w.originalName} <= std_logic_vector(${w.newName});\n"
//    }
//  }
//
//  def emitBlackBoxComponents(component: Component, ret: StringBuilder): Unit = {
//    val emited = mutable.Set[String]()
//    for (c <- component.children) c match {
//      case blackBox: BlackBox => {
//        if (!emited.contains(blackBox.definitionName)) {
//          emited += blackBox.definitionName
//          emitBlackBoxComponent(blackBox, ret)
//        }
//      }
//      case _ =>
//    }
//  }
//
//  def blackBoxRemplaceULogic(b: BlackBox, str: String): String = {
//    if (b.isUsingULogic)
//      str.replace("std_logic", "std_ulogic")
//    else
//      str
//  }
//
//  def emitBlackBoxComponent(component: BlackBox, ret: StringBuilder): Unit = {
//    ret ++= s"\n  component ${component.definitionName} is\n"
//    val genericFlat = component.getGeneric.flatten
//    if (genericFlat.size != 0) {
//      ret ++= s"    generic( \n"
//      for (e <- genericFlat) {
//        e match {
//          case baseType: BaseType => ret ++= s"      ${emitReference(baseType)} : ${blackBoxRemplaceULogic(component, emitDataType(baseType, true))};\n"
//          case (name : String,s: String) => ret ++= s"      $name : string;\n"
//          case (name : String,i : Int) => ret ++= s"      $name : integer;\n"
//          case (name : String,d: Double) => ret ++= s"      $name : real;\n"
//          case (name : String,b: Boolean) => ret ++= s"      $name : boolean;\n"
//          case (name : String,t: TimeNumber) => ret ++= s"      $name : time;\n"
//        }
//      }
//
//      ret.setCharAt(ret.size - 2, ' ')
//      ret ++= s"    );\n"
//    }
//    ret ++= s"    port( \n"
//    component.nodes.foreach(_ match {
//      case baseType: BaseType => {
//        if (baseType.isIo) {
//          ret ++= s"      ${baseType.getName()} : ${emitDirection(baseType)} ${blackBoxRemplaceULogic(component, emitDataType(baseType, true))};\n"
//        }
//      }
//      case _ =>
//    })
//    ret.setCharAt(ret.size - 2, ' ')
//    ret ++= s"    );\n"
//    ret ++= s"  end component;\n"
//    ret ++= s"  \n"
//  }
//
//  def emitAttributesDef(component: Component, ret: StringBuilder): Unit = {
//    val map = mutable.Map[String, Attribute]()
//
//    for (node <- component.nodes) {
//      for (attribute <- node.instanceAttributes(Language.VHDL)) {
//        val mAttribute = map.getOrElseUpdate(attribute.getName, attribute)
//        if (!mAttribute.sameType(attribute)) SpinalError(s"There is some attributes with different nature (${attribute} and ${mAttribute} at\n${node.component}})")
//      }
//    }
//
//    for (attribute <- map.values) {
//      val typeString = attribute match {
//        case _: AttributeString => "string"
//        case _: AttributeFlag => "boolean"
//      }
//      ret ++= s"  attribute ${attribute.getName} : $typeString;\n"
//    }
//
//    ret ++= "\n"
//  }
//
//  def toSpinalEnumCraft[T <: SpinalEnum](that: Any) = that.asInstanceOf[SpinalEnumCraft[T]]
//
  def getBaseTypeSignalInitialisation(signal : BaseType) : String = {
//    val reg = if(signal.isReg) signal.input.asInstanceOf[Reg] else null
//    if(reg != null){
//      if(reg.initialValue != null && reg.getClockDomain.config.resetKind == BOOT) {
//        return " := " + (reg.initialValue match {
//          case init : BaseType => emitLogic(init.getLiteral)
//          case init =>  emitLogic(init)
//        })
//      }else if (reg.hasTag(randomBoot)) {
//        return signal match {
//          case b: Bool => " := " + {
//            if (Random.nextBoolean()) "'1'" else "'0'"
//          }
//          case bv: BitVector => {
//            val rand = BigInt(bv.getWidth, Random).toString(2)
//            " := \"" + "0" * (bv.getWidth - rand.length) + rand + "\""
//          }
//          case e: SpinalEnumCraft[_] => {
//            val vec = e.spinalEnum.elements.toVector
//            val rand = vec(Random.nextInt(vec.size))
//            " := " + emitEnumLiteral(rand, e.getEncoding)
//          }
//        }
//      }
//    }
    "" //TODO IR !
  }
//


  def emitSignals(component: Component, b: ComponentBuilder): Unit = {
    for (node <- component.ownNameableNodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isIo) {
            b.declarations ++= s"  signal ${emitReference(signal, false)} : ${emitDataType(signal)}${getBaseTypeSignalInitialisation(signal)};\n"

//            if (signal.isInstanceOf[SpinalEnumCraft[_]]) {
//              val craft = toSpinalEnumCraft(signal)
//              if (!craft.getEncoding.isNative) {
//                ret ++= s"  signal ${emitReference(signal)}_debug : ${getEnumDebugType(craft.spinalEnum)};\n"
//                enumDebugSignals += toSpinalEnumCraft(signal)
//              }
//            }
          }


          emitAttributes(signal,signal.instanceAttributes(Language.VHDL), "signal", b.declarations)
        }
//        case readSync : MemReadSync => {
//          if(readSync.writeToReadKind == `writeFirst`){
//            ret ++= s"  signal ${emitReference(readSync.consumers(0))}_mem_addr : unsigned(${readSync.mem.addressWidth-1}} downto 0);\n"
//          }
//        }
//        case mem: Mem[_] => {
//          //ret ++= emitSignal(mem, mem);
//          val symbolWidth = mem.getMemSymbolWidth()
//          val symbolCount = mem.getMemSymbolCount
//
//          val initAssignementBuilder = for(i <- 0 until symbolCount) yield {
//            val builder = new StringBuilder()
//            val mask = (BigInt(1) << symbolWidth)-1
//            if (mem.initialContent != null) {
//              builder ++= " := ("
//
//              var first = true
//              for ((value, index) <- mem.initialContent.zipWithIndex) {
//                if (!first)
//                  builder ++= ","
//                else
//                  first = false
//
//                if ((index & 15) == 0) {
//                  builder ++= "\n     "
//                }
//
//                val unfilledValue = ((value>>(i*symbolWidth)) & mask).toString(2)
//                val filledValue = "0" * (symbolWidth-unfilledValue.length) + unfilledValue
//                builder ++= "\"" + filledValue + "\""
//              }
//
//              builder ++= ")"
//            }else if(mem.hasTag(randomBoot)){
//              builder ++= " := (others => (others => '1'))"
//            }
//            builder
//          }
//
//
//          if(memBitsMaskKind == MULTIPLE_RAM && symbolCount != 1) {
//            //if(mem.initialContent != null) SpinalError("Memory with multiple symbol per line + initial contant are not suported currently")
//
//            ret ++= s"  type ${emitReference(mem)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${symbolWidth - 1} downto 0);\n"
//            for(i <- 0 until symbolCount) {
//              val postfix = "_symbol" + i
//              ret ++= s"  signal ${emitReference(mem)}$postfix : ${emitDataType(mem)}${initAssignementBuilder(i).toString()};\n"
//              emitAttributes(mem,mem.instanceAttributes(Language.VHDL), "signal", ret,postfix = postfix)
//            }
//          }else{
//            ret ++= s"  type ${emitReference(mem)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${mem.getWidth - 1} downto 0);\n"
//            ret ++= s"  signal ${emitReference(mem)} : ${emitDataType(mem)}${initAssignementBuilder.head.toString()};\n"
//            emitAttributes(mem, mem.instanceAttributes(Language.VHDL), "signal", ret)
//          }
//        }
        case _ =>
      }


    }
  }
//
//
  def emitAttributes(node : NameableNode,attributes: Iterable[Attribute], vhdlType: String, ret: StringBuilder,postfix : String = ""): Unit = {
    for (attribute <- attributes){
      val value = attribute match {
        case attribute: AttributeString => "\"" + attribute.value + "\""
        case attribute: AttributeFlag => "true"
      }

      ret ++= s"  attribute ${attribute.getName} of ${emitReference(node, false)}: signal is $value;\n"
    }
  }
//
//
//
//  def emitAsyncronous(component: Component, ret: StringBuilder, funcRet: StringBuilder): Unit = {
//    val processList = getAsyncProcesses(component)
//
//    for (process <- processList if !process.needProcessDef) {
//      for (node <- process.nodes) {
//        emitAssignement(node, node.getInput(0), ret, "  ", "<=")
//      }
//    }
//
//    referenceSet = mutable.Set[Node with Nameable with ContextUser]()
//    for (process <- processList if process.needProcessDef) {
//      process.genSensitivity
//
//      val context = new AssignementLevel(process.nodes.map(n => AssignementLevelCmd(n,n.getInput(0))))
//
//      if (process.sensitivity.size != 0) {
//        val tmp = new StringBuilder
//        referenceSet.clear()
//        emitAssignementLevel(context,tmp, "    ", "<=",false,process.sensitivity)
//
//        //TODO sensitivity list generation no more required
////        ret ++= s"  process(${process.sensitivity.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
//        ret ++= s"  process(${referenceSet.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
//        ret ++= "  begin\n"
//        ret ++= tmp
//        ret ++= "  end process;\n\n"
//      } else {
//        //emit func as logic
//        assert(process.nodes.size == 1)
//        for (node <- process.nodes) {
//          val funcName = "zz_" + emitReference(node)
//          funcRet ++= emitFuncDef(funcName, node, context)
//          ret ++= s"  ${emitReference(node)} <= ${funcName};\n"
//          //          ret ++= s"  ${emitReference(node)} <= ${emitLogic(node.getInput(0))};\n"
//        }
//      }
//    }
//    referenceSet = null
//
//  }
//
//
//  def emitFuncDef(funcName: String, node: Node, context: AssignementLevel): StringBuilder = {
//    val ret = new StringBuilder
//    //context.emitContext(ret, "    ",":=")
//    ret ++= s"  function $funcName return ${emitDataType(node, false)} is\n"
//    ret ++= s"    variable ${emitReference(node)} : ${emitDataType(node, true)};\n"
//    ret ++= s"  begin\n"
//    emitAssignementLevel(context,ret, "    ", ":=")
//    ret ++= s"    return ${emitReference(node)};\n"
//    ret ++= s"  end function;\n"
//  }
//
//

  def refImpl(op: Expression): String = emitReference(op.asInstanceOf[RefExpression].source, true)


  def operatorImplAsBinaryOperator(vhd: String)(op: Expression): String = {
    val binaryOp = op.asInstanceOf[BinaryOperator]
    val temp = s"(${emitExpression(binaryOp.left)} $vhd ${emitExpression(binaryOp.right)})"
    if (opThatNeedBoolCast.contains(binaryOp.opName))
      return "pkg_toStdLogic" + temp
    else
      return temp
  }

  def boolLiteralImpl(op: Expression) : String = s"pkg_toStdLogic(${op.asInstanceOf[BoolLiteral].value})"
//
//  def moduloImpl(op: Modifier): String = {
//    val mod = op.asInstanceOf[Operator.BitVector.Mod]
//    s"resize(${emitLogic(mod.left)} mod ${emitLogic(mod.right)},${mod.getWidth})"
//  }
//
  def operatorImplAsUnaryOperator(vhd: String)(op: Expression): String = {
    val unary = op.asInstanceOf[UnaryOperator]
    s"($vhd ${emitExpression(unary.source)})"
  }
//
//  def operatorImplAsFunction(vhd: String)(func: Modifier): String = {
//    s"$vhd(${func.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})"
//  }
//
//  def shiftRightByIntImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByInt]
//    s"pkg_shiftRight(${emitLogic(node.input)},${node.shift})"
//  }
//
//  def shiftLeftByIntImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByInt]
//    s"pkg_shiftLeft(${emitLogic(node.input)},${node.shift})"
//  }
//
//  def shiftRightByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByIntFixedWidth]
//    s"shift_right(${emitLogic(node.input)},${node.shift})"
//  }
//
//  def shiftLeftByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByIntFixedWidth]
//    s"shift_left(${emitLogic(node.input)},${node.shift})"
//  }
//
//  def shiftRightBitsByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByIntFixedWidth]
//    s"std_logic_vector(shift_right(unsigned(${emitLogic(node.input)}),${node.shift}))"
//  }
//
//  def shiftLeftBitsByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByIntFixedWidth]
//    s"std_logic_vector(shift_left(unsigned(${emitLogic(node.input)}),${node.shift}))"
//  }
//
//
//  def shiftLeftByUIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByUIntFixedWidth]
//    s"shift_left(${emitLogic(node.left)},to_integer(${emitLogic(node.right)}))"
//  }
//
//  def shiftLeftBitsByUIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByUIntFixedWidth]
//    s"std_logic_vector(shift_left(unsigned(${emitLogic(node.left)}),to_integer(${emitLogic(node.right)})))"
//  }
//
//
//
//  def resizeFunction(vhdlFunc : String)(func: Modifier): String = {
//    val resize = func.asInstanceOf[Resize]
//    resize.input.getWidth match{
//      case 0 => {
//        func.getInput(0) match {
//          /*case lit: BitsLiteral => {
//            val bitString =  '"' + "0" ASFASF* func.getWidth + '"'
//            lit.kind match {
//              case _: Bits => s"pkg_stdLogicVector($bitString)"
//              case _: UInt => s"pkg_unsigned($bitString)"
//              case _: SInt => s"pkg_signed($bitString)"
//            }
//          }*/
//          case _ => s"pkg_resize(${emitLogic(resize.input)},${resize.size})"
//        }
//      }
//      case _ => s"pkg_resize(${emitLogic(resize.input)},${resize.size})"
//    }
//  }
//
//
//  def enumEgualsImpl(eguals: Boolean)(op: Modifier): String = {
//    val enumDef = op.asInstanceOf[EnumEncoded].getDefinition
//    val encoding = op.asInstanceOf[EnumEncoded].getEncoding
//
//    encoding match {
//      case `binaryOneHot` => s"pkg_toStdLogic((${emitLogic(op.getInput(0))} and ${emitLogic(op.getInput(1))}) ${if (eguals) "/=" else "="} ${'"' + "0" * encoding.getWidth(enumDef) + '"'})"
//      case _ => s"pkg_toStdLogic(${emitLogic(op.getInput(0))} ${if (eguals) "=" else "/="} ${emitLogic(op.getInput(1))})"
//    }
//  }
//
//
//  def operatorImplAsBitsToEnum(func: Modifier): String = {
//    val node = func.asInstanceOf[CastBitsToEnum]
//    val enumDef = node.getDefinition
//    val encoding = node.encoding
//
//    if (!encoding.isNative) {
//      emitLogic(func.getInput(0))
//    } else {
//      s"pkg_to${enumDef.getName()}_${encoding.getName()}(${(emitLogic(func.getInput(0)))})"
//    }
//  }
//
//  def operatorImplAsEnumToBits(func: Modifier): String = {
//    val cast = func.asInstanceOf[CastEnumToBits]
//    val enumDef = cast.input.getDefinition
//    val encoding = cast.input.getEncoding
//
//    if (!encoding.isNative) {
//      emitLogic(func.getInput(0))
//    } else {
//      s"pkg_toStdLogicVector_${encoding.getName()}(${(emitLogic(func.getInput(0)))})"
//    }
//  }
//
//  def operatorImplAsEnumToEnum(func: Modifier): String = {
//    val enumCast = func.asInstanceOf[CastEnumToEnum]
//    val enumDefSrc = enumCast.input.getDefinition
//    val encodingSrc = enumCast.input.getEncoding
//    val enumDefDst = enumCast.getDefinition
//    val encodingDst = enumCast.getEncoding
//
//    if (encodingDst.isNative && encodingSrc.isNative)
//      emitLogic(func.getInput(0))
//    else {
//      s"${getReEncodingFuntion(enumDefDst, encodingSrc,encodingDst)}(${emitLogic(enumCast.input)})"
//    }
//  }
//
//  def unaryAllBy(cast : String)(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.AllByBool]
//    s"${cast}'(${node.getWidth-1} downto 0 => ${emitLogic(node.input)})"
//  }
//
//
//
  val expressionImplMap = mutable.Map[Class[_ <: Expression], Expression => String]()
  val expressionTypeMap = mutable.Map[Class[_ <: Expression], Expression => String]()


  def expressionMapAdd(c : Class[_ <: Expression], impl : Expression => String, t : Expression => String): Unit ={
    expressionImplMap(c) = impl
    expressionTypeMap(c) = t
  }

  expressionMapAdd(classOf[RefExpression], refImpl, e => "std_logic")
  expressionMapAdd(classOf[BoolLiteral], boolLiteralImpl, e => "std_logic")

//  //unsigned
//  expressionImplMap.put("u+u", operatorImplAsBinaryOperator("+"))
//  expressionImplMap.put("u-u", operatorImplAsBinaryOperator("-"))
//  expressionImplMap.put("u*u", operatorImplAsBinaryOperator("*"))
//  expressionImplMap.put("u/u", operatorImplAsBinaryOperator("/"))
//  expressionImplMap.put("u%u", moduloImpl)
//
//  expressionImplMap.put("u|u", operatorImplAsBinaryOperator("or"))
//  expressionImplMap.put("u&u", operatorImplAsBinaryOperator("and"))
//  expressionImplMap.put("u^u", operatorImplAsBinaryOperator("xor"))
//  expressionImplMap.put("~u",  operatorImplAsUnaryOperator("not"))
//
//  expressionImplMap.put("u==u", operatorImplAsBinaryOperator("="))
//  expressionImplMap.put("u!=u", operatorImplAsBinaryOperator("/="))
//  expressionImplMap.put("u<u",  operatorImplAsBinaryOperator("<"))
//  expressionImplMap.put("u<=u", operatorImplAsBinaryOperator("<="))
//
//
//  expressionImplMap.put("u>>i", shiftRightByIntImpl)
//  expressionImplMap.put("u<<i", shiftLeftByIntImpl)
//  expressionImplMap.put("u>>u", operatorImplAsFunction("pkg_shiftRight"))
//  expressionImplMap.put("u<<u", operatorImplAsFunction("pkg_shiftLeft"))
//  expressionImplMap.put("u|>>i",  shiftRightByIntFixedWidthImpl)
//  expressionImplMap.put("u|<<i",  shiftLeftByIntFixedWidthImpl)
//  expressionImplMap.put("u|<<u",  shiftLeftByUIntFixedWidthImpl)
//
//
//  //signed
//  expressionImplMap.put("s+s", operatorImplAsBinaryOperator("+"))
//  expressionImplMap.put("s-s", operatorImplAsBinaryOperator("-"))
//  expressionImplMap.put("s*s", operatorImplAsBinaryOperator("*"))
//  expressionImplMap.put("s/s", operatorImplAsBinaryOperator("/"))
//  expressionImplMap.put("s%s", moduloImpl)
//
//  expressionImplMap.put("s|s", operatorImplAsBinaryOperator("or"))
//  expressionImplMap.put("s&s", operatorImplAsBinaryOperator("and"))
//  expressionImplMap.put("s^s", operatorImplAsBinaryOperator("xor"))
//  expressionImplMap.put("~s", operatorImplAsUnaryOperator("not"))
//  expressionImplMap.put("-s", operatorImplAsUnaryOperator("-"))
//
//  expressionImplMap.put("s==s", operatorImplAsBinaryOperator("="))
//  expressionImplMap.put("s!=s", operatorImplAsBinaryOperator("/="))
//  expressionImplMap.put("s<s", operatorImplAsBinaryOperator("<"))
//  expressionImplMap.put("s<=s", operatorImplAsBinaryOperator("<="))
//
//
//  expressionImplMap.put("s>>i", shiftRightByIntImpl)
//  expressionImplMap.put("s<<i", shiftLeftByIntImpl)
//  expressionImplMap.put("s>>u", operatorImplAsFunction("pkg_shiftRight"))
//  expressionImplMap.put("s<<u", operatorImplAsFunction("pkg_shiftLeft"))
//  expressionImplMap.put("s|>>i",  shiftRightByIntFixedWidthImpl)
//  expressionImplMap.put("s|<<i",  shiftLeftByIntFixedWidthImpl)
//  expressionImplMap.put("s|<<u",  shiftLeftByUIntFixedWidthImpl)
//
//
//
//  //bits
//  expressionImplMap.put("b##b", operatorImplAsFunction("pkg_cat"))
//
//  expressionImplMap.put("b|b", operatorImplAsBinaryOperator("or"))
//  expressionImplMap.put("b&b", operatorImplAsBinaryOperator("and"))
//  expressionImplMap.put("b^b", operatorImplAsBinaryOperator("xor"))
//  expressionImplMap.put("~b",  operatorImplAsUnaryOperator("not"))
//
//  expressionImplMap.put("b==b", operatorImplAsBinaryOperator("="))
//  expressionImplMap.put("b!=b", operatorImplAsBinaryOperator("/="))
//
//  expressionImplMap.put("b>>i", shiftRightByIntImpl)
//  expressionImplMap.put("b<<i", shiftLeftByIntImpl)
//  expressionImplMap.put("b>>u", operatorImplAsFunction("pkg_shiftRight"))
//  expressionImplMap.put("b<<u", operatorImplAsFunction("pkg_shiftLeft"))
//  expressionImplMap.put("b|>>i",  shiftRightBitsByIntFixedWidthImpl)
//  expressionImplMap.put("b|<<i",  shiftLeftBitsByIntFixedWidthImpl)
//  expressionImplMap.put("b|<<u",  shiftLeftBitsByUIntFixedWidthImpl)
//
////  expressionImplMap.put("brotlu", operatorImplAsFunction("pkg_rotateLeft"))
//
//
//
//  //bool

  expressionMapAdd(classOf[BoolLiteral], boolLiteralImpl, e => "std_logic")
  expressionMapAdd(classOf[Operator.Bool.Equal], operatorImplAsBinaryOperator("="), e => "std_logic")
  expressionMapAdd(classOf[Operator.Bool.NotEqual], operatorImplAsBinaryOperator("/="), e => "std_logic")


  expressionMapAdd(classOf[Operator.Bool.Not], operatorImplAsUnaryOperator("not"), e => "std_logic")
  expressionMapAdd(classOf[Operator.Bool.And], operatorImplAsBinaryOperator("and"), e => "std_logic")
  expressionMapAdd(classOf[Operator.Bool.Or], operatorImplAsBinaryOperator("or"), e => "std_logic")
  expressionMapAdd(classOf[Operator.Bool.Xor], operatorImplAsBinaryOperator("xor"), e => "std_logic")


//  //enum
//  expressionImplMap.put("e==e", enumEgualsImpl(true))
//  expressionImplMap.put("e!=e", enumEgualsImpl(false))
//
//  //cast
//  expressionImplMap.put("s->b", operatorImplAsFunction("std_logic_vector"))
//  expressionImplMap.put("u->b", operatorImplAsFunction("std_logic_vector"))
//  expressionImplMap.put("B->b", operatorImplAsFunction("pkg_toStdLogicVector"))
//  expressionImplMap.put("e->b", operatorImplAsEnumToBits)
//
//  expressionImplMap.put("b->s", operatorImplAsFunction("signed"))
//  expressionImplMap.put("u->s", operatorImplAsFunction("signed"))
//
//  expressionImplMap.put("b->u", operatorImplAsFunction("unsigned"))
//  expressionImplMap.put("s->u", operatorImplAsFunction("unsigned"))
//
//  expressionImplMap.put("b->e", operatorImplAsBitsToEnum)
//  expressionImplMap.put("e->e", operatorImplAsEnumToEnum)
//
//
//  //misc
//
//  expressionImplMap.put("resize(s,i)", resizeFunction("pkg_signed"))
//  expressionImplMap.put("resize(u,i)", resizeFunction("pkg_unsigned"))
//  expressionImplMap.put("resize(b,i)", resizeFunction("pkg_stdLogicVector"))
//
//  expressionImplMap.put("bAllByB", unaryAllBy("std_logic_vector"))
//  expressionImplMap.put("uAllByB", unaryAllBy("unsigned"))
//  expressionImplMap.put("sAllByB", unaryAllBy("signed"))
//
//
//  //Memo whenNode hardcode emitlogic
//  expressionImplMap.put("mux(B,B,B)", operatorImplAsFunction("pkg_mux"))
//  expressionImplMap.put("mux(B,b,b)", operatorImplAsFunction("pkg_mux"))
//  expressionImplMap.put("mux(B,u,u)", operatorImplAsFunction("pkg_mux"))
//  expressionImplMap.put("mux(B,s,s)", operatorImplAsFunction("pkg_mux"))
//  expressionImplMap.put("mux(B,e,e)", operatorImplAsFunction("pkg_mux"))
//
//  expressionImplMap.put("extract(b,i)", extractBoolFixed)
//  expressionImplMap.put("extract(u,i)", extractBoolFixed)
//  expressionImplMap.put("extract(s,i)", extractBoolFixed)
//
//  expressionImplMap.put("extract(b,u)", extractBoolFloating)
//  expressionImplMap.put("extract(u,u)", extractBoolFloating)
//  expressionImplMap.put("extract(s,u)", extractBoolFloating)
//
//  expressionImplMap.put("extract(b,i,i)", extractBitVectorFixed)
//  expressionImplMap.put("extract(u,i,i)", extractBitVectorFixed)
//  expressionImplMap.put("extract(s,i,i)", extractBitVectorFixed)
//
//  expressionImplMap.put("extract(b,u,w)", extractBitVectorFloating)
//  expressionImplMap.put("extract(u,u,w)", extractBitVectorFloating)
//  expressionImplMap.put("extract(s,u,w)", extractBitVectorFloating)
//
//
//
//  def extractBoolFixed(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBoolFixed]
//    s"pkg_extract(${emitLogic(that.getBitVector)},${that.getBitId})"
//  }
//
//  def extractBoolFloating(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBoolFloating]
//    s"pkg_extract(${emitLogic(that.getBitVector)},to_integer(${emitLogic(that.getBitId)}))"
//  }
//
//  def extractBitVectorFixed(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBitsVectorFixed]
//    s"pkg_extract(${emitLogic(that.getBitVector)},${that.getHi},${that.getLo})"
//  }
//
//  def extractBitVectorFloating(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBitsVectorFloating]
//    s"pkg_extract(${emitLogic(that.getBitVector)},${emitLogic(that.getOffset)},${that.getBitCount})"
//  }
//
//
  def opThatNeedBoolCastGen(a: String, b: String): List[String] = {
    ("==" :: "!=" :: "<" :: "<=" :: Nil).map(a + _ + b)
  }

  //TODO remove for something faster
  val opThatNeedBoolCast = mutable.Set[String]()
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("B", "B")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("b", "b")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("u", "u")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("s", "s")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("e", "e")
//
//
//  def emitLogic(node: Node): String = node match {
//    case baseType: BaseType => emitReference(baseType)
//    case node: Modifier => expressionImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)
//
//    case lit: BitsLiteral => s"pkg_stdLogicVector(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//    case lit: UIntLiteral => s"pkg_unsigned(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//    case lit: SIntLiteral => s"pkg_signed(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//
//    case lit: BitsAllToLiteral => lit.theConsumer match {
//      case _: Bits => s"pkg_stdLogicVector(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//      case _: UInt => s"pkg_unsigned(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//      case _: SInt => s"pkg_signed(${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'})"
//    }
//    case lit: BoolLiteral => s"pkg_toStdLogic(${lit.value})"
//    //  case lit: BoolLiteral => if(lit.value) "'1'" else "'0'" //Invalid VHDL when '1' = '1'
//    case lit: EnumLiteral[_] => emitEnumLiteral(lit.enum, lit.encoding)
//    case memRead: MemReadAsync => {
//      if(memRead.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memRead.getMem} because of its mixed width ports")
//      if (memRead.readUnderWrite == dontCare) SpinalWarning(s"memReadAsync with dontCare is as writeFirst into VHDL")
//      val symbolCount = memRead.getMem.getMemSymbolCount
//      if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//        s"${emitReference(memRead.getMem)}(to_integer(${emitReference(memRead.getAddress)}))"
//      else
//        (0 until symbolCount).reverse.map(i => (s"${emitReference(memRead.getMem)}_symbol$i(to_integer(${emitReference(memRead.getAddress)}))")).reduce(_ + " & " + _)
//    }
//    case whenNode: WhenNode => s"pkg_mux(${whenNode.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})" //Exeptional case with asyncrouns of literal
//    case dc : DontCareNodeEnum => {
//      if(dc.encoding.isNative)
//        return "pkg_enum." + dc.enum.elements.head.getName()
//      else
//        return s"(${'"'}${"-" * dc.encoding.getWidth(dc.enum)}${'"'})"
//    }
//    case dc: DontCareNode => {
//      dc.getBaseType match {
//        case to: Bool => s"'-'"
//        case to: BitVector => s"(${'"'}${"-" * dc.asInstanceOf[Widthable].getWidth}${'"'})"
//      }
//    }
//
//
//    case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
//  }
//
//  def emitDebug(component: Component, ret: StringBuilder, enumDebugSignals: ArrayBuffer[SpinalEnumCraft[_]]): Unit = {
//    for (signal <- enumDebugSignals) {
//      ret ++= s"  ${emitReference(signal)}_debug <= ${getEnumToDebugFuntion(toSpinalEnumCraft(signal).spinalEnum, signal.getEncoding)}(${emitReference(signal)});\n"
//    }
//  }
//
//  def emitSyncronous(component: Component, ret: StringBuilder): Unit = {
//    val syncNodes = component.getDelays
//
//    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[SyncNode]]()
//
//    for (syncNode <- syncNodes) {
//      clockDomainMap.getOrElseUpdate(syncNode.getClockDomain, new ArrayBuffer[SyncNode]()) += syncNode
//    }
//
//    for ((clockDomain, array) <- clockDomainMap.toList.sortWith(_._1.instanceCounter < _._1.instanceCounter)) {
//      val arrayWithReset = ArrayBuffer[SyncNode]()
//      val arrayWithoutReset = ArrayBuffer[SyncNode]()
//
//      for (syncNode <- array) {
//        if (syncNode.isUsingResetSignal || syncNode.isUsingSoftResetSignal)
//          arrayWithReset += syncNode
//        else
//          arrayWithoutReset += syncNode
//      }
//
//      emitClockDomain(true)
//      emitClockDomain(false)
//
//
//
//      def emitClockDomain(withReset: Boolean): Unit = {
//        val activeArray = if (withReset) arrayWithReset else arrayWithoutReset
//        if (activeArray.size == 0) return;
//        val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
//        val reset = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
//        val softReset = if (null == clockDomain.softReset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.softReset, throw new Exception("???")).asInstanceOf[Bool]
//        val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
//        val asyncReset = (null != reset) && clockDomain.config.resetKind == ASYNC
//        val syncReset = (null != reset) && clockDomain.config.resetKind == SYNC
//        var tabLevel = 1
//        def tabStr = "  " * tabLevel
//        def inc = {
//          tabLevel = tabLevel + 1
//        }
//        def dec = {
//          tabLevel = tabLevel - 1
//        }
//
//        val initialValues = ArrayBuffer[Node]()
//        val initialTasks = ArrayBuffer[AssignementLevelCmd]()
//        for (syncNode <- activeArray) syncNode match {
//          case reg: Reg => {
//            if (reg.hasInitialValue) {
//              initialTasks += AssignementLevelCmd(reg.getOutputByConsumers, reg.getInitialValue)
//              initialValues += reg.getInitialValue
//            }
//          }
//          case _ =>
//        }
//        val initialValueAssignement = new AssignementLevel(initialTasks)
//
//
//
//        if (asyncReset) {
//          val sensitivity = getSensitivity(initialValues, true)
//          ret ++= s"${tabStr}process(${emitReference(clock)},${emitReference(reset)}${sensitivity.foldLeft("")(_ + "," + emitReference(_))})\n"
//        } else {
//          ret ++= s"${tabStr}process(${emitReference(clock)})\n"
//        }
//
//        ret ++= s"${tabStr}begin\n"
//        inc
//        if (asyncReset) {
//          ret ++= s"${tabStr}if ${emitReference(reset)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\' then\n";
//          inc
//          emitRegsInitialValue(initialValueAssignement, tabStr)
//          dec
//          ret ++= s"${tabStr}elsif ${emitClockEdge(clock, clockDomain.config.clockEdge)}"
//          inc
//        } else {
//          ret ++= s"${tabStr}if ${emitClockEdge(clock, clockDomain.config.clockEdge)}"
//          inc
//        }
//        if (clockEnable != null) {
//          ret ++= s"${tabStr}if ${emitReference(clockEnable)} = \'${if (clockDomain.config.clockEnableActiveLevel == HIGH) 1 else 0}\' then\n"
//          inc
//        }
//
//        if (syncReset || softReset != null) {
//          var condList = ArrayBuffer[String]()
//          if(syncReset) condList += s"${emitReference(reset)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\'"
//          if(softReset != null) condList += s"${emitReference(softReset)} = \'${if (clockDomain.config.softResetActiveLevel == HIGH) 1 else 0}\'"
//
//          ret ++= s"${tabStr}if ${condList.reduce(_ + " or " + _)} then\n"
//          inc
//          emitRegsInitialValue(initialValueAssignement, tabStr)
//          dec
//          ret ++= s"${tabStr}else\n"
//          inc
//          emitRegsLogic(tabStr)
//          dec
//          ret ++= s"${tabStr}end if;\n"
//          dec
//        } else {
//          emitRegsLogic(tabStr)
//          dec
//        }
//
//        while (tabLevel != 1) {
//          ret ++= s"${tabStr}end if;\n"
//          dec
//        }
//        ret ++= s"${tabStr}end process;\n"
//        dec
//        ret ++= s"${tabStr}\n"
//
//
//        def emitRegsInitialValue(assignementLevel: AssignementLevel, tab: String): Unit = {
//          emitAssignementLevel(assignementLevel,ret, tab, "<=")
//        }
//
//
//        def emitRegsLogic(tab: String): Unit = {
//
//
//          val assignementTasks = ArrayBuffer[AssignementLevelCmd]()
//
//
//          for (syncNode <- activeArray) syncNode match {
//            case reg: Reg => {
//              val regSignal = reg.getOutputByConsumers
//              if (!regSignal.isIo || !regSignal.isInput) {
//                val in = reg.getDataInput
//                if (in != reg)
//                  assignementTasks += AssignementLevelCmd(regSignal, in)
//              }
//            }
//            case memWrite: MemWrite => {
//              if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.getMem} because of its mixed width ports")
//
//              if (memWrite.useWriteEnable) {
//                ret ++= s"${tab}if ${emitReference(memWrite.getEnable)} = '1' then\n"
//                emitWrite(tab + "  ")
//                ret ++= s"${tab}end if;\n"
//              } else {
//                emitWrite(tab)
//              }
//
//              def emitWrite(tab: String) = {
//                val symbolCount = memWrite.getMem.getMemSymbolCount
//                val bitPerSymbole = memWrite.getMem.getMemSymbolWidth()
//
//                if(memWrite.getMask == null) {
//                  if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                    ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
//                  else
//                    for(i <- 0 until symbolCount) {
//                      val range = s"(${(i + 1) * bitPerSymbole - 1} downto ${i * bitPerSymbole})"
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"
//                    }
//                }else{
//
//                  val maskCount = memWrite.getMask.getWidth
//                  for(i <- 0 until maskCount){
//                    val range = s"(${(i+1)*bitPerSymbole-1} downto ${i*bitPerSymbole})"
//                    ret ++= s"${tab}if ${emitReference(memWrite.getMask)}($i) = '1' then\n"
//                    if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)}))$range <= ${emitReference(memWrite.getData)}$range;\n"
//                    else
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"
//
//                    ret ++= s"${tab}end if;\n"
//                  }
//                }
//              }
//            }
//            case memReadSync: MemReadSync => {
//              if(memReadSync.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadSync.getMem} because of its mixed width ports")
//              if(memReadSync.readUnderWrite == writeFirst) SpinalError(s"Can't translate a memReadSync with writeFirst into VHDL $memReadSync")
//              if(memReadSync.readUnderWrite == dontCare) SpinalWarning(s"memReadSync with dontCare is as readFirst into VHDL $memReadSync")
//              if(memReadSync.useReadEnable) {
//                ret ++= s"${tab}if ${emitReference(memReadSync.getReadEnable)} = '1' then\n"
//                emitRead(tab + "  ")
//                ret ++= s"${tab}end if;\n"
//              } else {
//                emitRead(tab)
//              }
//              def emitRamRead() = {
//                val symbolCount = memReadSync.getMem.getMemSymbolCount
//                if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                  s"${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}))"
//                else
//                  (0 until symbolCount).reverse.map(i => (s"${emitReference(memReadSync.getMem)}_symbol$i(to_integer(${emitReference(memReadSync.getAddress)}))")).reduce(_ + " & " + _)
//              }
//              def emitRead(tab: String) = ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitRamRead()};\n"
//
//            }
//
//            case memWrite: MemReadWrite_writePart => {
//              val memReadSync = memWrite.readPart
//              if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.getMem} because of its mixed width ports")
//              if (memReadSync.readUnderWrite == writeFirst) SpinalError(s"Can't translate a MemWriteOrRead with writeFirst into VHDL $memReadSync")
//              if (memReadSync.readUnderWrite == dontCare) SpinalWarning(s"MemWriteOrRead with dontCare is as readFirst into VHDL $memReadSync")
//
//              val symbolCount = memWrite.getMem.getMemSymbolCount
//              ret ++= s"${tab}if ${emitReference(memWrite.getChipSelect)} = '1' then\n"
//              ret ++= s"${tab}  if ${emitReference(memWrite.getWriteEnable)} = '1' then\n"
//              emitWrite(tab + "    ")
//              ret ++= s"${tab}  end if;\n"
//              if (memReadSync.component.nodes.contains(memReadSync))
//                emitRead(tab + "  ")
//              ret ++= s"${tab}end if;\n"
//
//              def emitWrite(tab: String) = {
////                ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
//
//                val symbolCount = memWrite.getMem.getMemSymbolCount
//                val bitPerSymbole = memWrite.getMem.getMemSymbolWidth()
//
//                if(memWrite.getMask == null) {
//                  if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                    ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
//                  else
//                    for(i <- 0 until symbolCount) {
//                      val range = s"(${(i + 1) * bitPerSymbole - 1} downto ${i * bitPerSymbole})"
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"
//                    }
//                }else{
//
//                  val maskCount = memWrite.getMask.getWidth
//                  for(i <- 0 until maskCount){
//                    val range = s"(${(i+1)*bitPerSymbole-1} downto ${i*bitPerSymbole})"
//                    ret ++= s"${tab}if ${emitReference(memWrite.getMask)}($i) = '1' then\n"
//                    if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)}))$range <= ${emitReference(memWrite.getData)}$range;\n"
//                    else
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"
//
//                    ret ++= s"${tab}end if;\n"
//                  }
//                }
//              }
//              def emitRead(tab: String) = {
////                ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}));\n"
//                val symbolCount = memReadSync.getMem.getMemSymbolCount
//                ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${
//                  if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                    s"${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}))"
//                  else
//                    (0 until symbolCount).reverse.map(i => (s"${emitReference(memReadSync.getMem)}_symbol$i(to_integer(${emitReference(memReadSync.getAddress)}))")).reduce(_ + " & " + _)
//                };\n"
//
//
//              }
//
//            }
//            case memWriteRead_readPart: MemReadWrite_readPart => {
//
//            }
//            case assertNode : AssertNode => {
//              val cond = emitLogic(assertNode.cond)
//              require(assertNode.message.size == 0 || (assertNode.message.size == 1 && assertNode.message(0).isInstanceOf[String]))
//              val message = if(assertNode.message.size == 1) s"""report "${assertNode.message(0)}" """ else ""
//              val severity = "severity " +  (assertNode.severity match{
//                case `NOTE`     => "NOTE"
//                case `WARNING`  => "WARNING"
//                case `ERROR`    => "ERROR"
//                case `FAILURE`  => "FAILURE"
//              })
//              ret ++= s"${tab}assert $cond = '1' $message $severity;\n"
//            }
//          }
//          val rootContext = new AssignementLevel(assignementTasks)
//          emitAssignementLevel(rootContext,ret, tab, "<=")
//        }
//      }
//    }
//  }
//
//  def emitAssignement(to: Node, from: Node, ret: StringBuilder, tab: String, assignementKind: String): Unit = {
//    from match {
//      case from: AssignementNode => {
//        from match {
//          case assign: BitAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getBitId}) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: BitAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}(to_integer(${emitLogic(assign.getBitId)})) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getHi} downto ${assign.getLo}) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}(${assign.getBitCount.value - 1} + to_integer(${emitLogic(assign.getOffset)}) downto to_integer(${emitLogic(assign.getOffset)})) ${assignementKind} ${emitLogic(assign.getInput)};\n"
//        }
//      }
//      case man: MultipleAssignmentNode => {
//        //For some case with asyncronous partial assignement
//        man.onEachInput(assign => {
//          emitAssignement(to, assign, ret, tab, assignementKind)
//        })
//      }
//      case _ => ret ++= s"$tab${emitAssignedReference(to)} ${assignementKind} ${emitLogic(from)};\n"
//    }
//  }
//
//  def emitAssignementLevel(context : AssignementLevel,ret: mutable.StringBuilder, tab: String, assignementKind: String, isElseIf: Boolean = false,hiddenSensitivity : mutable.Set[Node] = null): Unit = {
//    val firstTab = if (isElseIf) "" else tab
//
//    context.content.foreach(_ match {
//      case whenTree: AssignementLevelWhen => {
//        def doTrue = whenTree.whenTrue.isNotEmpty
//        def doFalse = whenTree.whenFalse.isNotEmpty
//        val condLogic = emitLogic(whenTree.cond)
//        val condLogicCleaned = if (condLogic.startsWith("pkg_toStdLogic(")) condLogic.substring("pkg_toStdLogic(".length, condLogic.length - 1) else condLogic + " = '1'"
//
//        if (doTrue && !doFalse) {
//          ret ++= s"${firstTab}if $condLogicCleaned then\n"
//          emitAssignementLevel(whenTree.whenTrue, ret, tab + "  ", assignementKind)
//          ret ++= s"${tab}end if;\n"
//        } else {
//          ret ++= s"${firstTab}if $condLogicCleaned then\n"
//          emitAssignementLevel(whenTree.whenTrue, ret, tab + "  ", assignementKind)
//          val falseHead = if (whenTree.whenFalse.isOnlyAWhen) whenTree.whenFalse.content.head else null
//          if (falseHead != null && falseHead.asInstanceOf[AssignementLevelWhen].context.parentElseWhen == whenTree.context) {
//            ret ++= s"${tab}els"
//            emitAssignementLevel(whenTree.whenFalse, ret, tab, assignementKind, true)
//          } else {
//            ret ++= s"${tab}else\n"
//            emitAssignementLevel(whenTree.whenFalse, ret, tab + "  ", assignementKind)
//            ret ++= s"${tab}end if;\n"
//          }
//        }
//      }
//      case switchTree : AssignementLevelSwitch => {
//        if(hiddenSensitivity != null) hiddenSensitivity += switchTree.key
//        ret ++= s"${tab}case ${emitLogic(switchTree.key)} is\n"
//        switchTree.cases.foreach(c => {
//          val litString = c.const match {
//            case lit: BitsLiteral => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//            case lit: UIntLiteral => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//            case lit: SIntLiteral => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//
//            case lit: BitsAllToLiteral => lit.theConsumer match {
//              case _: Bits => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//              case _: UInt => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//              case _: SInt => s"${'\"'}${lit.getBitsStringOn(lit.getWidth)}${'\"'}"
//            }
//            case lit: BoolLiteral => s"${lit.value}"
//            //  case lit: BoolLiteral => if(lit.value) "'1'" else "'0'" //Invalid VHDL when '1' = '1'
//            case lit: EnumLiteral[_] => emitEnumLiteral(lit.enum, lit.encoding)
//          }
//          ret ++= s"${tab}  when $litString =>\n"
//          emitAssignementLevel(c.doThat,ret,tab + "    ",assignementKind)
//        })
//        ret ++= s"${tab}  when others =>\n"
//        if(switchTree.default != null){
//          emitAssignementLevel(switchTree.default.doThat,ret,tab + "    ",assignementKind)
//        }
//        ret ++= s"${tab}end case;\n"
//      }
//      case task : AssignementLevelSimple => emitAssignement(task.that, task.by, ret, tab, assignementKind)
//    })
//
//
//  }
//
//  def emitComponentInstances(component: Component, ret: StringBuilder): Unit = {
//    for (kind <- component.children) {
//      val isBB = kind.isInstanceOf[BlackBox]
//      val isBBUsingULogic = isBB && kind.asInstanceOf[BlackBox].isUsingULogic
//      val definitionString = if (isBB) kind.definitionName
//      else s"entity work.${
//        emitedComponentRef.getOrElse(kind, kind).definitionName
//      }"
//      ret ++= s"  ${
//        kind.getName()
//      } : $definitionString\n"
//
//
//      def addULogicCast(bt: BaseType, io: String, logic: String, dir: IODirection): String = {
//
//        if (isBBUsingULogic)
//          if (dir == in) {
//            bt match {
//              case _: Bool => return s"      $io => std_ulogic($logic),\n"
//              case _: Bits => return s"      $io => std_ulogic_vector($logic),\n"
//              case _ => return s"      $io => $logic,\n"
//            }
//          } else if (dir == spinal.core.out) {
//            bt match {
//              case _: Bool => return s"      std_logic($io) => $logic,\n"
//              case _: Bits => return s"      std_logic_vector($io) => $logic,\n"
//              case _ => return s"      $io => $logic,\n"
//            }
//          } else SpinalError("???")
//
//        else
//          return s"      $io => $logic,\n"
//      }
//
//      if (kind.isInstanceOf[BlackBox]) {
//        val bb = kind.asInstanceOf[BlackBox]
//        val genericFlat = bb.getGeneric.flatten
//
//        if (genericFlat.size != 0) {
//          ret ++= s"    generic map( \n"
//
//
//          for (e <- genericFlat) {
//            e match {
//              case baseType: BaseType => ret ++= addULogicCast(baseType, emitReference(baseType), emitLogic(baseType.getInput(0)), in)
//              case (name : String,s: String) => ret ++= s"      ${name} => ${"\""}${s}${"\""},\n"
//              case (name : String,i : Int) => ret ++= s"      ${name} => $i,\n"
//              case (name : String,d: Double) => ret ++= s"      ${name} => $d,\n"
//              case (name : String,b: Boolean) => ret ++= s"      ${name} => $b,\n"
//              case (name : String,t: TimeNumber) => {
//                val d = t.decompose
//                ret ++= s"      ${name} => ${d._1} ${d._2},\n"
//              }
//            }
//          }
//          ret.setCharAt(ret.size - 2, ' ')
//          ret ++= s"    )\n"
//        }
//      }
//      ret ++= s"    port map ( \n"
//      for (data <- kind.getOrdredNodeIo) {
//        if (data.isOutput) {
//          val bind = component.kindsOutputsToBindings.getOrElse(data, null)
//          if (bind != null) {
//            ret ++= addULogicCast(data, emitReference(data), emitReference(bind), data.dir)
//          }
//        }
//        else if (data.isInput)
//          ret ++= addULogicCast(data, emitReference(data), emitReference(data.getInput(0)), data.dir)
//      }
//      ret.setCharAt(ret.size - 2, ' ')
//
//      ret ++= s"    );"
//      ret ++= s"\n"
//    }
//  }
}
