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
import scala.util.Random


class ComponentEmitterVhdl(
  val c                              : Component,
  vhdlBase                           : VhdlBase,
  override val algoIdIncrementalBase : Int,
  override val mergeAsyncProcess     : Boolean,
  asyncResetCombSensitivity          : Boolean,
  anonymSignalPrefix                 : String,
  emitedComponentRef                 : java.util.concurrent.ConcurrentHashMap[Component,Component],
  pc                                 : PhaseContext,
  override val spinalConfig          : SpinalConfig
) extends ComponentEmitter{

  import vhdlBase._

  override def component = c

  val portMaps     = ArrayBuffer[String]()
  val declarations = new StringBuilder()
  val logics       = new StringBuilder()

  override def readedOutputWrapEnable = true

  def getTrace() = new ComponentEmitterTrace(declarations :: logics :: Nil, portMaps)


  def emitLibrary(ret: StringBuilder): Unit = {
    vhdlBase.emitLibrary(ret)
    val libs = mutable.HashSet[String]()

    for(child <- c.children)child match {
      case bb : BlackBox if bb.isBlackBox => libs ++= bb.librariesUsages
      case _ =>
    }
    for(lib <- libs){
      ret ++= s"library $lib;\n"
      ret ++= s"use $lib.all;\n"
    }
  }

  def result: String = {
    val ret = new StringBuilder()

    emitLibrary(ret)

    ret ++= commentTagsToString(component.definition, "--")

    ret ++= s"\nentity ${c.definitionName} is\n"

    if (portMaps.length > 0) {
      ret ++= s"  port("
      var first = true

      for (portMap <- portMaps) {
        if (first) {
          ret ++= s"\n    $portMap"
          first = false
        } else {
          ret ++= s";\n    $portMap"
        }
      }

      ret ++= "\n  );\n"
    }

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

  def emitEntity(): Unit = {
    component.getOrdredNodeIo.foreach(baseType =>
      if (!baseType.isSuffix)
        portMaps += s"${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)}${getBaseTypeSignalInitialisation(baseType)}"
    )
  }
  def emitLocation(that : AssignmentStatement) : String = if(that.locationString != null) " --" + that.locationString else ""

  override def wrapSubInput(io: BaseType): Unit = {
    if (referencesOverrides.contains(io))
      return
    var name: String = null
    if (!io.isSuffix) {
      name = component.localNamingScope.allocateName(io.component.getName() + "_" +  io.getName())
      declarations ++= s"  signal $name : ${emitDataType(io)};\n"
    } else {
      wrapSubInput(io.parent.asInstanceOf[BaseType])
      var parentName: String = ""
      referencesOverrides(io.parent) match {
        case s: String => parentName = s
        case n: Nameable => parentName = n.getNameElseThrow
        case _ => throw new Exception(s"Could not determine name of ${io}")
      }
      name = parentName + "." + io.getPartialName()
    }
    referencesOverrides(io) = name
  }

  def emitArchitecture(): Unit = {
    for(mem <- mems){
      var portId  = 0
      mem.foreachStatements(s => {
        s.foreachDrivingExpression{
          case e: BaseType =>
          case e           => expressionToWrap += e
        }

        val portName = anonymSignalPrefix + "_" + mem.getName() + "_port" + portId
        s match {
          case s: MemReadSync =>
            val name = component.localNamingScope.allocateName(portName)
            declarations ++= s"  signal $name : ${emitType(s)};\n"
            wrappedExpressionToName(s) = name
          case s: MemReadAsync =>
            val name = component.localNamingScope.allocateName(portName)
            declarations ++= s"  signal $name : ${emitType(s)};\n"
            wrappedExpressionToName(s) = name
          case s: MemReadWrite =>
            val name = component.localNamingScope.allocateName(portName)
            declarations ++= s"  signal $name : ${emitType(s)};\n"
            wrappedExpressionToName(s) = name
          case s: MemWrite =>
        }
      })
    }

    for(output <- outputsToBufferize){
      val name = component.localNamingScope.allocateName(output.getName() + "_read_buffer")
      declarations ++= s"  signal $name : ${emitDataType(output)}${getBaseTypeSignalInitialisation(output)};\n"
      logics ++= s"  ${emitReference(output, false)} <= $name;\n"
      referencesOverrides(output) = name
    }

    for((select, muxes) <- multiplexersPerSelect){
      expressionToWrap += select._1
      expressionToWrap ++= muxes
    }

    component.children.foreach(sub => {
      val structSignals = new mutable.ArrayBuffer[SpinalStruct]()
      sub.getAllIo.foreach(_io => {
        var io = _io
        if (io.isOutput) {
          // If output is a suffix'd type emit the parent exactly once
          if (io.isInstanceOf[SpinalStruct]) {
            structSignals += io.asInstanceOf[SpinalStruct]
          }
          if (io.isSuffix) {
            val structParent = io.parent.asInstanceOf[SpinalStruct]
            if (!structSignals.contains(structParent)) {
              structSignals += structParent
              io = structParent
            }
          }
          val name = component.localNamingScope.allocateName(sub.getNameElseThrow + "_" + io.getNameElseThrow)
          if (!io.isSuffix)
            declarations ++= s"  signal $name : ${emitDataType(io)};\n"
          referencesOverrides(io) = name
        }
      })
    })

    //Wrap expression which need it
    cutLongExpressions()
    expressionToWrap --= wrappedExpressionToName.keysIterator
    component.dslBody.walkStatements { s =>
      s.walkExpression { e =>
        if (!e.isInstanceOf[DeclarationStatement] && expressionToWrap.contains(e)) {
          val sName = s match {
            case s: AssignmentStatement => "_" + s.dlcParent.getName()
            case _: WhenStatement => "_when"
            case _: SwitchContext => "_switch"
            case s: MemPortStatement => "_" + s.dlcParent.getName() + "_port"
            case s: Nameable => "_" + s.getName()
            case _ => ""
          }

          val name = component.localNamingScope.allocateName(anonymSignalPrefix + sName)
          declarations ++= s"  signal $name : ${emitType(e)};\n"
          wrappedExpressionToName(e) = name
        }
      }
    }

    for(e <- expressionToWrap  if !e.isInstanceOf[DeclarationStatement] && !e.isInstanceOf[Multiplexer]){
      logics ++= s"  ${wrappedExpressionToName(e)} <= ${emitExpressionNoWrappeForFirstOne(e)};\n"
    }

    //Wrap inout
//    analogs.foreach(io => {
//      io.foreachStatements{
//        case AssignmentStatement(target, source: BaseType) =>
//          referencesOverrides(source) = emitAssignedExpression(target)
//        case _ =>
//      }
//    })

    //Flush all that mess out ^^
    emitBlackBoxComponents()
    emitAttributesDef()
    emitSignals()
    emitMems(mems)
    emitSubComponents(openSubIo)
    emitAnalogs()
    emitMuxes()

    processes.foreach(p => {
      if(p.leafStatements.nonEmpty) {
        p.leafStatements.head match {
          case AssignmentStatement(target: DeclarationStatement, _) if subComponentInputToNotBufferize.contains(target) =>
          case _ => emitAsynchronous(p)
        }
      } else {
        emitAsynchronous(p)
      }
    })

    syncGroups.valuesIterator.foreach(emitSynchronous(component, _))

    component.dslBody.walkStatements{
      case s: TreeStatement => s.algoIncrementale = algoIdIncrementalBase
      case s                =>
    }
  }

  def emitAnalogs(): Unit ={
    analogs.foreach(analog => {
      analog.foreachStatements{
        case AssignmentStatement(target, source: AnalogDriver) =>
          source.getTypeObject match {
            case `TypeBool` => logics ++= s"  ${emitAssignedExpression(target)} <= ${emitExpression(source.data)} when ${emitExpression(source.enable)} = '1' else 'Z';\n"
            case `TypeBits` | `TypeUInt` | `TypeSInt` => logics ++= s"  ${emitAssignedExpression(target)} <= ${emitExpression(source.data)} when ${emitExpression(source.enable)} = '1' else (others => 'Z');\n"
            case `TypeEnum` => SpinalError("???")
          }
        case s =>
      }
    })
  }

  def emitSubComponents(openSubIo: mutable.HashSet[BaseType]): Unit = {
    for(child <- component.children){
      emitAttributes(child.getName(), child.instanceAttributes, "label", declarations, "")
    }

    val analogDrivers = mutable.LinkedHashMap[BaseType, ArrayBuffer[AssignmentStatement]]()
    for(analog <- analogs) analog.foreachStatements{s =>
      s.walkDrivingExpressions{
        case e : BaseType => analogDrivers.getOrElseUpdate(e, ArrayBuffer[AssignmentStatement]()) += s
        case _ =>
      }
    }

    for (children <- component.children) {
      val isBB             = children.isInstanceOf[BlackBox] && children.asInstanceOf[BlackBox].isBlackBox
      val isBBUsingULogic        = isBB && children.asInstanceOf[BlackBox].isUsingULogic
      val isBBUsingNoNumericType = isBB && children.asInstanceOf[BlackBox].isUsingNoNumericType
      val definitionString = if (isBB) children.definitionName else s"entity work.${getOrDefault(emitedComponentRef, children, children).definitionName}"
      val postBb = new StringBuilder()
      logics ++= commentTagsToString(children, "  --")
      logics ++= s"  ${
        children.getName()
      } : $definitionString\n"


      def addCasting(bt: BaseType, io: String, logic: String, dir: IODirection): String = {

        def wrapIo(t : String, body : String => String): String ={
          val wrap = s"${children.getName()}_${logic}_bb_wrap"
          declarations ++= s"  signal $wrap : $t${if(!bt.isInstanceOf[Bool]) s"(${bt.getBitsWidth-1} downto 0)" else ""};\n"
          postBb ++= s"  ${body(wrap)};\n"
          s"      $io => $wrap,\n"
        }
        def wrapTo(t : String) = wrapIo(t, w => s"$w <= $t($logic)")
        def wrapFrom(t : String, t2 : String) = wrapIo(t, w => s"$logic <= $t2($w)")
        if (isBBUsingULogic || isBBUsingNoNumericType) {
          if (dir == in) {
            bt match {
              case _: Bool if isBBUsingULogic                            => return wrapTo("std_ulogic")
              case _: Bits if isBBUsingULogic                            => return wrapTo("std_ulogic_vector")
              case _: UInt if isBBUsingNoNumericType && !isBBUsingULogic => return wrapTo("std_logic_vector")
              case _: UInt if isBBUsingNoNumericType &&  isBBUsingULogic => return wrapTo("std_ulogic_vector")
              case _: SInt if isBBUsingNoNumericType && !isBBUsingULogic => return wrapTo("std_logic_vector")
              case _: SInt if isBBUsingNoNumericType &&  isBBUsingULogic => return wrapTo("std_ulogic_vector")
              case _                                                     => return s"      $io => $logic,\n"
            }
          } else if (dir == out) {
            bt match {
              case _: Bool if isBBUsingULogic => return wrapFrom("std_ulogic", "std_logic")
              case _: Bits if isBBUsingULogic => return wrapFrom("std_ulogic_vector", "std_logic_vector")
              case _: UInt if isBBUsingNoNumericType && !isBBUsingULogic => return wrapFrom("std_logic_vector", "unsigned")
              case _: UInt if isBBUsingNoNumericType &&  isBBUsingULogic => return wrapFrom("std_ulogic_vector", "unsigned")
              case _: SInt if isBBUsingNoNumericType && !isBBUsingULogic => return wrapFrom("std_logic_vector", "signed")
              case _: SInt if isBBUsingNoNumericType &&  isBBUsingULogic => return wrapFrom("std_ulogic_vector", "signed")
              case _                          => return s"      $io => $logic,\n"
            }
          }else{
            SpinalError("It is not possible to cast an inout")
          }

        }else {
          return s"      $io => $logic,\n"
        }
      }

      if (isBB) {
        val bb = children.asInstanceOf[BlackBox]
        val genericFlat = bb.genericElements

        if (genericFlat.nonEmpty) {
          logics ++= s"    generic map( \n"

          for (e <- genericFlat) {
            e match {
              case (name: String, bt: BaseType)     => logics ++= addCasting(bt, name, emitExpression(bt.getTag(classOf[GenericValue]).get.e), in)
              case (name: String, s: String)        => logics ++= s"      ${name} => ${"\""}${s}${"\""},\n"
              case (name: String, i: Int)           => logics ++= s"      ${name} => $i,\n"
              case (name: String, d: Double)        => logics ++= s"      ${name} => $d,\n"
              case (name: String, boolean: Boolean) => logics ++= s"      ${name} => $boolean,\n"
              case (name: String, t: TimeNumber)    =>
                val d = t.decompose
                logics ++= s"      ${name} => ${d._1} ${d._2},\n"
            }
          }

          logics.setCharAt(logics.size - 2, ' ')
          logics ++= s"    )\n"
        }
      }

      logics ++= s"    port map ( \n"

      for (data <- children.getOrdredNodeIo) {
        if (!data.isInstanceOf[SpinalStruct]) {
          val logic = if(openSubIo.contains(data)) "open" else emitReference(data, false)
          if(data.isInOut){
            val buf = new mutable.StringBuilder()
            analogDrivers.get(data) match {
              case Some(statements) => {
                case class Mapping(offset : Int, width : Int, dst : Expression)
                val mapping = statements.map{ s =>
                  val subio = s.source match {
                    case bt : BaseType => emitExpression(bt)
                    case e : BitVectorBitAccessFixed => s"${emitExpression(e.source)}(${e.bitId})"
                    case e : BitVectorRangedAccessFixed => s"${emitExpression(e.source)}(${e.hi} downto ${e.lo})"
                  }
                  logics ++= addCasting(data, subio, emitAssignedExpression(s.target), data.dir)
                }
              }
              case None =>
            }

          } else {
            logics ++= addCasting(data, emitReferenceNoOverrides(data), logic, data.dir)
          }
        }
      }

      logics.setCharAt(logics.size - 2, ' ')

      logics ++= s"    );"
      logics ++= s"\n"
      if(postBb.nonEmpty) {
        logics ++= postBb.toString()
        logics ++= s"\n"
      }
    }
  }

  def emitClockedProcess(emitRegsLogic        : (String, StringBuilder) => Unit,
                         emitRegsInitialValue : (String, StringBuilder) => Unit,
                         b                    : mutable.StringBuilder,
                         clockDomain          : ClockDomain,
                         withReset            : Boolean): Unit = {

    val clock       = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
    val reset       = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
    val softReset   = if (null == clockDomain.softReset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.softReset, throw new Exception("???")).asInstanceOf[Bool]
    val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]

    val asyncReset = (null != reset) && clockDomain.config.resetKind == ASYNC
    val syncReset  = (null != reset) && clockDomain.config.resetKind == SYNC
    var tabLevel   = 1

    def tabStr = "  " * tabLevel

    def inc = tabLevel = tabLevel + 1

    def dec = tabLevel = tabLevel - 1

    val initialStatlementsGeneration =  new StringBuilder()

    referenceSetStart()

    if (withReset) {
      val initSensitivity = asyncResetCombSensitivity && asyncReset
      if(!initSensitivity) referenceSetPause()
      emitRegsInitialValue("      ", initialStatlementsGeneration)
      if(!initSensitivity) referenceSetResume()
    }



    referenceSetAdd(emitReference(clock, false))

    if (asyncReset) {
      referenceSetAdd(emitReference(reset, false))
    }

    b ++= s"${tabStr}process(${referenceSetSorted.mkString(", ")})\n"
    b ++= s"${tabStr}begin\n"
    inc

    if (asyncReset) {
      b ++= s"${tabStr}if ${emitReference(reset, false)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\' then\n"
      inc
      b ++= initialStatlementsGeneration
      dec
      b ++= s"${tabStr}elsif ${emitClockEdge(emitReference(clock, false), clockDomain.config.clockEdge)}"
      inc
    } else {
      b ++= s"${tabStr}if ${emitClockEdge(emitReference(clock, false), clockDomain.config.clockEdge)}"
      inc
    }

    if (clockEnable != null) {
      b ++= s"${tabStr}if ${emitReference(clockEnable, false)} = \'${if (clockDomain.config.clockEnableActiveLevel == HIGH) 1 else 0}\' then\n"
      inc
    }

    if (syncReset || softReset != null) {
      var condList = ArrayBuffer[String]()
      if(syncReset) condList += s"${emitReference(reset, false)} = \'${if (clockDomain.config.resetActiveLevel == HIGH) 1 else 0}\'"
      if(softReset != null) condList += s"${emitReference(softReset, false)} = \'${if (clockDomain.config.softResetActiveLevel == HIGH) 1 else 0}\'"

      b ++= s"${tabStr}if ${condList.reduce(_ + " or " + _)} then\n"
      inc
      for(str <- initialStatlementsGeneration.toString.linesWithSeparators){
        b ++= "  "
        b ++= str
      }
      dec
      b ++= s"${tabStr}else\n"
      inc
      emitRegsLogic(tabStr,b)
      dec
      b ++= s"${tabStr}end if;\n"
      dec
    } else {
      emitRegsLogic(tabStr,b)
      dec
    }

    while(tabLevel != 1) {
      b ++= s"${tabStr}end if;\n"
      dec
    }

    b ++= s"${tabStr}end process;\n"
    dec
    b ++= s"${tabStr}\n"
  }

  def emitSynchronous(component: Component, group: SyncGroup): Unit = {
    import group._

    def withReset = hasInit

    def emitRegsInitialValue(tab: String, b: StringBuilder): Unit = {
      emitLeafStatements(group.initStatements, 0, group.scope, "<=", b , tab)
    }

    def emitRegsLogic(tab: String, b: StringBuilder): Unit = {
      emitLeafStatements(group.dataStatements, 0, group.scope, "<=", b , tab)
    }

    emitClockedProcess(
      emitRegsLogic        = emitRegsLogic,
      emitRegsInitialValue = emitRegsInitialValue,
      b                    = logics,
      clockDomain          = group.clockDomain,
      withReset            = withReset
    )
  }


  def emitMuxes(): Unit ={
    val tmp = new StringBuilder()
    for(((select, length), muxes) <- multiplexersPerSelect){
      referenceSetStart()
      tmp.clear()
      tmp ++= s"  begin\n"
      tmp ++= s"    case ${emitExpression(select)} is\n"
      for(i <- 0 until length){
        val key = Integer.toBinaryString(i)
        if(i != length-1)
          tmp ++= s"""      when "${"0" * (select.getWidth - key.length)}${key}" =>\n"""
        else
          tmp ++= s"      when others =>\n"

        for(mux <- muxes){
          tmp ++= s"        ${wrappedExpressionToName(mux)} <= ${emitExpression(mux.inputs(i))};\n"
        }
      }
      tmp ++= s"    end case;\n"
      tmp ++= s"  end process;\n\n"

      logics ++= s"  process(${referenceSetSorted.mkString(",")})\n"
      logics ++= tmp
    }
    referenceSetStop()
  }

  def emitAsynchronous(process: AsyncProcess): Unit = {
    process match {
      case _ if process.leafStatements.size == 1 && process.leafStatements.head.parentScope == process.nameableTargets.head.rootScopeStatement => process.leafStatements.head match {
        case s: AssignmentStatement =>
          logics ++= emitAssignment(s, "  ", "<=")
      }
      case _ =>
        val tmp = new StringBuilder

        referenceSetStart()

        emitLeafStatements(process.leafStatements, 0, process.scope, "<=", tmp, "    ")

        if (referenceSetSorted.nonEmpty) {
          logics ++= s"  process(${referenceSetSorted.mkString(",")})\n"
          logics ++= "  begin\n"
          logics ++= tmp.toString()
          logics ++= "  end process;\n\n"
        } else {
          //assert(process.nameableTargets.size == 1)
          for(node <- process.nameableTargets) node match {
            case node: BaseType =>
              val localDeclaration = new StringBuilder

              val funcName = "zz_" + emitReference(node, false).replace(".", "_")
              val varName = emitReference(node, false)

              localDeclaration ++= s"  function $funcName return ${emitDataType(node, false)} is\n"
              localDeclaration ++= s"    variable ${varName} : ${emitDataType(node, true)};\n"
              localDeclaration ++= s"  begin\n"
              val statements = ArrayBuffer[LeafStatement]()
              node.foreachStatements(s => statements += s.asInstanceOf[LeafStatement])
              emitLeafStatements(statements, 0, process.scope, ":=", localDeclaration, "    ")
              localDeclaration ++= s"    return ${varName};\n"
              localDeclaration ++= s"  end function;\n"

              localDeclaration.toString().split("\n").foreach(line => {
                if (line.contains(":=")) {
                  val parts = line.split(":=")
                  declarations ++= parts.head.replace(".", "_") + ":="
                  parts.tail.foreach {
                    declarations ++= _
                  }
                  declarations ++= "\n"
                } else {
                  declarations ++= line.replace(".", "_") + "\n"
                }
              })

              logics ++= s"  ${emitReference(node, false)} <= ${funcName};\n"
          }
        }
    }
  }

  def emitLeafStatements(statements: ArrayBuffer[LeafStatement], statementIndexInit: Int, scope: ScopeStatement, assignmentKind: String, b: StringBuilder, tab: String): Int = {
    var statementIndex = statementIndexInit
    var lastWhen: WhenStatement = null

    def closeSubs(): Unit = {
      if(lastWhen != null) {
        b ++= s"${tab}end if;\n"
        lastWhen = null
      }
    }

    while(statementIndex < statements.length){
      val leaf        = statements(statementIndex)
      val statement   = leaf
      val targetScope = statement.parentScope

      if(targetScope == scope){
        closeSubs()

        statement match {
          case assignment: AssignmentStatement  => b ++= emitAssignment(assignment,tab, assignmentKind)
          case assertStatement: AssertStatement =>
            val cond = emitExpression(assertStatement.cond)

            require(assertStatement.message.isEmpty || (assertStatement.message.size == 1 && assertStatement.message.head.isInstanceOf[String]))

            val message = if(assertStatement.message.size == 1) s"""report "${assertStatement.message(0)}" """ else ""

            val severity = "severity " +  (assertStatement.severity match{
              case `NOTE`     => "NOTE"
              case `WARNING`  => "WARNING"
              case `ERROR`    => "ERROR"
              case `FAILURE`  => "FAILURE"
            })
            b ++= s"${tab}assert $cond = '1' $message $severity;\n"
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

        if(treeStatement != lastWhen) {
          closeSubs()
        }

        treeStatement match {
          case treeStatement: WhenStatement =>
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
            lastWhen       = treeStatement
            statementIndex = emitLeafStatements(statements,statementIndex, scopePtr, assignmentKind,b, tab + "  ")
          case switchStatement: SwitchStatement =>
            val isPure = switchStatement.elements.foldLeft(true)((carry, element) => carry && !(element.keys.exists(!_.isInstanceOf[Literal])))
            //Generate the code

            def findSwitchScopeRec(scope: ScopeStatement): ScopeStatement = scope.parentStatement match {
              case null                      => null
              case s if s == switchStatement => scope
              case s                         => findSwitchScopeRec(s.parentScope)
            }

            def findSwitchScope(): ScopeStatement = {
              if(statementIndex < statements.length)
                findSwitchScopeRec(statements(statementIndex).parentScope)
              else
                null
            }

            var nextScope = findSwitchScope()

            //Generate the code
            if(isPure) {
              def emitIsCond(that: Expression): String = that match {
                case lit: BitVectorLiteral => '"' + lit.getBitsStringOnNoPoison(lit.getWidth) + '"'
                case lit: BoolLiteral      => if(lit.value) "'1'" else "'0'"
                case lit: EnumLiteral[_]   => emitEnumLiteral(lit.senum, lit.encoding)
              }

              b ++= s"${tab}case ${emitExpression(switchStatement.value)} is\n"
              switchStatement.elements.foreach(element =>  {
                val hasStuff = nextScope == element.scopeStatement
                if(hasStuff || switchStatement.defaultScope != null) {
                  b ++= s"${tab}  when ${element.keys.map(e => emitIsCond(e)).mkString(" | ")} =>\n"
                  if (hasStuff || switchStatement.defaultScope != null) {
                    statementIndex = emitLeafStatements(statements, statementIndex, element.scopeStatement, assignmentKind, b, tab + "    ")
                    nextScope = findSwitchScope()
                  }
                }
              })

              b ++= s"${tab}  when others =>\n"
              if (switchStatement.defaultScope != null) {
                if(nextScope == switchStatement.defaultScope) {
                  statementIndex = emitLeafStatements(statements, statementIndex, switchStatement.defaultScope, assignmentKind, b, tab + "    ")
                  nextScope      = findSwitchScope()
                }
              }

              b ++= s"${tab}end case;\n"
            } else {

              def emitIsCond(that: Expression): String = that match {
                case that: SwitchStatementKeyBool => s"(${emitExpression(that.cond)} = '1')"
                case that                         => s"(${emitExpression(switchStatement.value)} = ${emitExpression(that)})"
              }

              var index = 0

              switchStatement.elements.foreach(element => {
                b ++= s"${tab}${if(index == 0) "if" else "elsif"} ${element.keys.map(e => emitIsCond(e)).mkString(" or ")} then\n"
                if(nextScope == element.scopeStatement) {
                  statementIndex = emitLeafStatements(statements, statementIndex, element.scopeStatement, assignmentKind, b, tab + "    ")
                  nextScope      = findSwitchScope()
                }
                index += 1
              })

              if(switchStatement.defaultScope  != null){
                b ++= s"${tab}else\n"
                if(nextScope == switchStatement.defaultScope) {
                  statementIndex = emitLeafStatements(statements, statementIndex, switchStatement.defaultScope, assignmentKind, b, tab + "    ")
                  nextScope      = findSwitchScope()
                }
              }
              b ++= s"${tab}end if;\n"
            }
        }
      }
    }

    closeSubs()

    return statementIndex
  }

  def emitAssignment(assignment: AssignmentStatement, tab: String, assignmentKind: String): String = {
    assignment match {
      case _ =>
        if (!assignment.target.isInstanceOf[SpinalStruct])
          s"$tab${emitAssignedExpression(assignment.target)} ${assignmentKind} ${emitExpression(assignment.source)};${emitLocation(assignment)}\n"
        else
          ""
    }
  }

  def referenceSetStart(): Unit ={
    _referenceSetEnabled = true
    _referenceSet.clear()
  }

  def referenceSetStop(): Unit ={
    _referenceSetEnabled = false
    _referenceSet.clear()
  }

  def referenceSetPause(): Unit ={
    _referenceSetEnabled = false
  }

  def referenceSetResume(): Unit ={
    _referenceSetEnabled = true
  }

  def referenceSetAdd(str : String): Unit ={
    if(_referenceSetEnabled) {
      _referenceSet.add(str)
    }
  }

  def referenceSetSorted() = _referenceSet

  var _referenceSetEnabled = false
  val _referenceSet        = mutable.LinkedHashSet[String]()

  def emitReference(that: DeclarationStatement, sensitive: Boolean): String ={
    val name = referencesOverrides.getOrElse(that, that.getNameElseThrow) match {
      case x: String               => x
      case x: DeclarationStatement => emitReference(x,false)
      case x: Literal => emitExpression(x)
    }

    if(sensitive) referenceSetAdd(name)

    name
  }

  def emitReferenceNoOverrides(that: DeclarationStatement): String ={
    that.getNameElseThrow
  }

  def emitAssignedExpression(that : Expression): String = that match{
    case that: BaseType                 => emitReference(that, false)
    case that: BitAssignmentFixed       => s"${emitReference(that.out, false)}(${that.bitId})"
    case that: BitAssignmentFloating    => s"${emitReference(that.out, false)}(to_integer(${emitExpression(that.bitId)}))"
    case that: RangedAssignmentFixed    => s"${emitReference(that.out, false)}(${that.hi} downto ${that.lo})"
    case that: RangedAssignmentFloating => s"${emitReference(that.out, false)}(${that.bitCount - 1} + to_integer(${emitExpression(that.offset)}) downto to_integer(${emitExpression(that.offset)}))"
  }

  def emitExpression(that: Expression): String = {
    wrappedExpressionToName.get(that) match {
      case Some(name) =>
        referenceSetAdd(name)
        name
      case None => dispatchExpression(that)
    }
  }

  def emitExpressionNoWrappeForFirstOne(that: Expression): String = {
    dispatchExpression(that)
  }

  def emitAttributesDef(): Unit = {
    val map = mutable.Map[String, Attribute]()

    def walk(that : Any) = that match{
      case s: SpinalTagReady =>
        for (attribute <- s.instanceAttributes(Language.VHDL)) {
          val mAttribute = map.getOrElseUpdate(attribute.getName, attribute)
          if (!mAttribute.sameType(attribute)) SpinalError(s"There is some attributes with different nature (${attribute} and ${mAttribute} at\n${component}})")
        }
      case s =>
    }

    component.dslBody.walkStatements(walk)
    component.children.foreach(walk)

    for (attribute <- map.values) {
      val typeString = attribute match {
        case _: AttributeString  => "string"
        case _: AttributeInteger => "integer"
        case _: AttributeFlag    => "boolean"
      }

      declarations ++= s"  attribute ${attribute.getName} : $typeString;\n"
    }

    declarations ++= "\n"
  }


  def getBaseTypeSignalInitialisation(signal: BaseType): String = {
    if(signal.isReg){
      if(signal.clockDomain.config.resetKind == BOOT && signal.hasInit) {
        var initExpression: Literal = null
        var needFunc = false

        signal.foreachStatements {
          case s: InitAssignmentStatement =>
            assert(s.target == signal, s"Partial init not supported on $signal")
            if(initExpression != null)
              needFunc = true
            def findLiteral(that : Expression): Literal = that match{
              case that : Literal => that
              case that : BaseType => {
                if(Statement.isSomethingToFullStatement(that)){
                  findLiteral(that.head.source)
                }else{
                  SpinalError(s"Can't resolve the literal value of $that")
                }
              }
            }

            initExpression = s match {
              case s : Literal => s
              case _ => findLiteral(s.source)
            }
          case s =>
        }

        if(needFunc)
          ???
        else {
          //          assert(initStatement.parentScope == signal.parentScope)
          return " := " + emitExpressionNoWrappeForFirstOne(initExpression)
        }

      }else if (signal.hasTag(randomBoot)) {

        return signal match {
          case b: Bool       =>
            " := " + { if(pc.config.randBootFixValue) {"'0'"} else { if(Random.nextBoolean()) "'1'" else "'0'"} }
          case bv: BitVector =>
            val rand = (if(pc.config.randBootFixValue) {BigInt(0)} else { BigInt(bv.getBitsWidth, Random)}).toString(2)
            " := \"" + "0" * (bv.getWidth - rand.length) + rand + "\""
          case e: SpinalEnumCraft[_] =>
            val vec  = e.spinalEnum.elements.toVector
            val rand = if(pc.config.randBootFixValue) vec(0) else vec(Random.nextInt(vec.size))
            " := " + emitEnumLiteral(rand, e.getEncoding)
        }
      }
    }
    ""
  }

  var memBitsMaskKind : MemBitsMaskKind = MULTIPLE_RAM

  def emitSignals(): Unit = {
    component.dslBody.walkDeclarations {
      case signal: BaseType =>
        if (!signal.isIo && !signal.isSuffix) {
          declarations ++= s"  signal ${emitReference(signal, false)} : ${emitDataType(signal)}${getBaseTypeSignalInitialisation(signal)};\n"
        }
        emitAttributes(signal, signal.instanceAttributes(Language.VHDL), "signal", declarations)
      case mem: Mem[_] =>
    }
  }

  def emitMems(mems: ArrayBuffer[Mem[_]]): Unit = {
    for(mem <- mems) emitMem(mem)
  }

  def emitMem(mem: Mem[_]): Unit = {

    def emitDataType(mem: Mem[_], constrained: Boolean = true) =  s"${emitReference(mem, constrained)}_type"

    //ret ++= emitSignal(mem, mem);
    val symbolWidth = mem.getMemSymbolWidth()
    val symbolCount = mem.getMemSymbolCount

    val initAssignmentBuilder = for(i <- 0 until symbolCount) yield {
      val builder = new StringBuilder()
      val mask    = (BigInt(1) << symbolWidth) - 1

      if (mem.initialContent != null) {
        builder ++= " := ("

        var first = true
        for ((value, index) <- mem.initialContent.zipWithIndex) {
          if (!first)
            builder ++= ","
          else
            first = false

          if ((index & 15) == 0) {
            builder ++= "\n     "
          }

          val unfilledValue = ((value>>(i*symbolWidth)) & mask).toString(2)
          val filledValue   = "0" * (symbolWidth-unfilledValue.length) + unfilledValue

          builder ++= "\"" + filledValue + "\""
        }

        builder ++= ")"
      }else if(mem.hasTag(randomBoot)){
        val value = if(pc.config.randBootFixValue) {"'1'"} else { if(Random.nextBoolean()) "'1'" else "'0'"}
        builder ++= s" := (others => (others => $value))"
      }
      builder
    }


    if(memBitsMaskKind == MULTIPLE_RAM && symbolCount != 1) {
      //if(mem.initialContent != null) SpinalError("Memory with multiple symbol per line + initial contant are not suported currently")

      val mappings = ArrayBuffer[MemSymbolesMapping]()
      declarations ++= s"  type ${emitReference(mem,false)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${symbolWidth - 1} downto 0);\n"
      for(i <- 0 until symbolCount) {
        val postfix = "_symbol" + i
        val symbolName = s"${emitReference(mem,false)}$postfix"
        declarations ++= s"  signal $symbolName : ${emitDataType(mem)}${initAssignmentBuilder(i).toString()};\n"
        mappings += MemSymbolesMapping(symbolName, i*symbolWidth until (i+1)*symbolWidth)
        emitAttributes(mem,mem.instanceAttributes(Language.VHDL), "signal", declarations,postfix = postfix)
      }
      mem.addTag(MemSymbolesTag(mappings))
    }else{
      declarations ++= s"  type ${emitReference(mem,false)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${mem.getWidth - 1} downto 0);\n"
      declarations ++= s"  signal ${emitReference(mem,false)} : ${emitDataType(mem)}${initAssignmentBuilder.head.toString()};\n"
      emitAttributes(mem, mem.instanceAttributes(Language.VHDL), "signal", declarations)
    }


    def emitWrite(b: StringBuilder, mem: Mem[_], writeEnable: String, address: Expression, data: Expression, mask: Expression with WidthProvider, symbolCount: Int, bitPerSymbole: Int, tab: String): Unit = {
      if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1) {
        val ramAssign = s"$tab${emitReference(mem, false)}(to_integer(${emitExpression(address)})) <= ${emitExpression(data)};\n"

        if (writeEnable != null) {
          b ++= s"${tab}if ${writeEnable} then\n  "
          b ++= ramAssign
          b ++= s"${tab}end if;\n"
        } else {
          b ++= ramAssign
        }

      } else {

        def maskCount = mask.getWidth

        for(i <- 0 until symbolCount) {
          var conds = if(writeEnable != null) List(writeEnable) else Nil
          val range = s"(${(i + 1) * bitPerSymbole - 1} downto ${i * bitPerSymbole})"

          if(mask != null)
            conds =  s"${emitExpression(mask)}($i) = '1'" :: conds

          if(conds.nonEmpty)
            b ++= s"${tab}if ${conds.mkString(" and ")} then\n"

          if (memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
            b ++= s"$tab  ${emitReference(mem, false)}(to_integer(${emitExpression(address)}))$range <= ${emitExpression(data)}$range;\n"
          else
            b ++= s"$tab  ${emitReference(mem, false)}_symbol${i}(to_integer(${emitExpression(address)})) <= ${emitExpression(data)}$range;\n"

          if(conds.nonEmpty)
            b ++= s"${tab}end if;\n"
        }
      }
    }

    def emitRead(b: StringBuilder, mem: Mem[_], address: Expression, target: Expression, tab: String) = {
      val ramRead = {

        val symbolCount = mem.getMemSymbolCount

        if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
          b ++= s"$tab${emitExpression(target)} <= ${emitReference(mem, false)}(to_integer(${emitExpression(address)}));\n"
        else{
          val symboleReadDataNames = for(i <- 0 until symbolCount) yield {
            val symboleReadDataName = component.localNamingScope.allocateName(anonymSignalPrefix)
            declarations ++= s"  signal $symboleReadDataName : std_logic_vector(${mem.getMemSymbolWidth()-1} downto 0);\n"
            b ++= s"$tab$symboleReadDataName <= ${emitReference(mem,false)}_symbol$i(to_integer(${emitExpression(address)}));\n"
            symboleReadDataName
          }

          logics ++= s"  process (${symboleReadDataNames.mkString(", " )})\n"
          logics ++= s"  begin\n"
          logics ++= s"    ${emitExpression(target)} <= ${symboleReadDataNames.reverse.mkString(" & " )};\n"
          logics ++= s"  end process;\n"
        }
//          (0 until symbolCount).reverse.map(i => (s"${emitReference(mem, false)}_symbol$i(to_integer(${emitExpression(address)}))")).reduce(_ + " & " + _)
      }
    }

    def emitPort(port: MemPortStatement, tab: String, b: mutable.StringBuilder): Unit = port match {
      case memWrite: MemWrite =>
        if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.mem} because of its mixed width ports")
        emitWrite(b, memWrite.mem,  if (memWrite.writeEnable != null) emitExpression(memWrite.writeEnable) + " = '1'" else null.asInstanceOf[String], memWrite.address, memWrite.data, memWrite.mask, memWrite.mem.getMemSymbolCount, memWrite.mem.getMemSymbolWidth(), tab)
      case memReadSync: MemReadSync =>
        if(memReadSync.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadSync.mem} because of its mixed width ports")
        if(memReadSync.readUnderWrite == writeFirst) SpinalError(s"Can't translate a memReadSync with writeFirst into VHDL $memReadSync")
        if(memReadSync.readUnderWrite == dontCare) SpinalWarning(s"memReadSync with dontCare is as readFirst into VHDL $memReadSync")
        if(memReadSync.readEnable != null) {
          b ++= s"${tab}if ${emitExpression(memReadSync.readEnable)} = '1' then\n"
          emitRead(b, memReadSync.mem, memReadSync.address, memReadSync, tab + "  ")
          b ++= s"${tab}end if;\n"
        } else {
          emitRead(b, memReadSync.mem, memReadSync.address, memReadSync, tab)
        }
      case memReadWrite: MemReadWrite =>
        if(memReadWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadWrite.mem} because of its mixed width ports")
        //                    if (memReadWrite.readUnderWrite == writeFirst) SpinalError(s"Can't translate a MemWriteOrRead with writeFirst into VHDL $memReadWrite")
        //                    if (memReadWrite.readUnderWrite == dontCare) SpinalWarning(s"MemWriteOrRead with dontCare is as readFirst into VHDL $memReadWrite")

        val symbolCount = memReadWrite.mem.getMemSymbolCount
        emitWrite(b, memReadWrite.mem,s"${emitExpression(memReadWrite.chipSelect)} = '1' and ${emitExpression(memReadWrite.writeEnable)} = '1'", memReadWrite.address, memReadWrite.data, memReadWrite.mask, memReadWrite.mem.getMemSymbolCount, memReadWrite.mem.getMemSymbolWidth(),tab)
        b ++= s"${tab}if ${emitExpression(memReadWrite.chipSelect)} = '1' then\n"
        emitRead(b, memReadWrite.mem, memReadWrite.address, memReadWrite, tab + "  ")
        b ++= s"${tab}end if;\n"
    }

    val cdTasks = mutable.LinkedHashMap[ClockDomain, ArrayBuffer[MemPortStatement]]()


    val tmpBuilder = new StringBuilder()

    mem.foreachStatements{
      case memWrite: MemWrite      =>
        emitClockedProcess((tab, b) => {
          if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.mem} because of its mixed width ports")
          emitWrite(b, memWrite.mem,  if (memWrite.writeEnable != null) emitExpression(memWrite.writeEnable) + " = '1'" else null.asInstanceOf[String], memWrite.address, memWrite.data, memWrite.mask, memWrite.mem.getMemSymbolCount, memWrite.mem.getMemSymbolWidth(), tab)
        }, null, tmpBuilder, memWrite.clockDomain, false)
      case memReadWrite: MemReadWrite  =>
        if(memReadWrite.readUnderWrite != dontCare) SpinalError(s"memReadWrite can only be emited as dontCare into VHDL $memReadWrite")
        if(memReadWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadWrite.mem} because of its mixed width ports")
        emitClockedProcess((tab, b) => {
          val symbolCount = memReadWrite.mem.getMemSymbolCount()
          b ++= s"${tab}if ${emitExpression(memReadWrite.chipSelect)} = '1' then\n"
          emitRead(b, memReadWrite.mem, memReadWrite.address, memReadWrite, tab + "  ")
          b ++= s"${tab}end if;\n"
        }, null, tmpBuilder, memReadWrite.clockDomain, false)

        emitClockedProcess((tab, b) => {
          val symbolCount = memReadWrite.mem.getMemSymbolCount()
          emitWrite(b, memReadWrite.mem,s"${emitExpression(memReadWrite.chipSelect)} = '1' and ${emitExpression(memReadWrite.writeEnable)} = '1'", memReadWrite.address, memReadWrite.data, memReadWrite.mask, memReadWrite.mem.getMemSymbolCount, memReadWrite.mem.getMemSymbolWidth(),tab)
        }, null, tmpBuilder, memReadWrite.clockDomain, false)

      case memReadSync: MemReadSync   =>
        if(memReadSync.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadSync.mem} because of its mixed width ports")
        if(memReadSync.readUnderWrite == writeFirst) SpinalError(s"memReadSync with writeFirst is as dontCare into VHDL $memReadSync")
        if(memReadSync.readUnderWrite == readFirst) SpinalError(s"memReadSync with readFirst is as dontCare into VHDL $memReadSync")
        emitClockedProcess((tab, b) => {
          if(memReadSync.readEnable != null) {
            b ++= s"${tab}if ${emitExpression(memReadSync.readEnable)} = '1' then\n"
            emitRead(b, memReadSync.mem, memReadSync.address, memReadSync, tab + "  ")
            b ++= s"${tab}end if;\n"
          } else {
            emitRead(b, memReadSync.mem, memReadSync.address, memReadSync, tab)
          }
        }, null, tmpBuilder, memReadSync.clockDomain, false)
      case port: MemReadAsync  =>
        if(port.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${port.mem} because of its mixed width ports")
        if (port.readUnderWrite != writeFirst) SpinalWarning(s"memReadAsync can only be write first into VHDL")
        val symbolCount = port.mem.getMemSymbolCount
        if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
          tmpBuilder ++= s"  ${emitExpression(port)} <= ${emitReference(port.mem, false)}(to_integer(${emitExpression(port.address)}));\n"
        else
          (0 until symbolCount).foreach(i => tmpBuilder  ++= s"  ${emitExpression(port)}(${(i + 1) * symbolWidth - 1} downto ${i * symbolWidth}) <= ${emitReference(port.mem, false)}_symbol$i(to_integer(${emitExpression(port.address)}));\n")
    }


    for((cd, ports) <- cdTasks){
      for(port <- ports){
        def syncLogic(tab: String, b: StringBuilder): Unit = port match{
          case port: MemWrite     => emitPort(port, tab, b)
          case port: MemReadSync  => if(port.readUnderWrite != dontCare) emitPort(port, tab, b)
          case port: MemReadWrite => emitPort(port, tab, b)
        }
        emitClockedProcess(syncLogic, null, tmpBuilder, cd, false)
      }
    }

    logics ++= tmpBuilder
  }
  def emitAttributes(node: DeclarationStatement, attributes: Iterable[Attribute], vhdlType: String, ret: StringBuilder, postfix: String = ""): Unit = {
    emitAttributes(emitReference(node, false), attributes,vhdlType,ret,postfix)
  }

  def emitAttributes(node: String, attributes: Iterable[Attribute], vhdlType: String, ret: StringBuilder, postfix: String): Unit = {
    for (attribute <- attributes){
      val value = attribute match {
        case attribute: AttributeString  => "\"" + attribute.value + "\""
        case attribute: AttributeInteger => attribute.value.toString
        case attribute: AttributeFlag    => "true"
      }

      ret ++= s"  attribute ${attribute.getName} of $node$postfix : $vhdlType is $value;\n"
    }
  }

  def emitBlackBoxComponents(): Unit = {
    val emited = mutable.Set[String]()

    for (c <- component.children) c match {
      case blackBox: BlackBox if blackBox.isBlackBox =>
        if (!emited.contains(blackBox.definitionName)) {
          emited += blackBox.definitionName
          emitBlackBoxComponent(blackBox)
        }
      case _ =>
    }
  }

  def blackBoxReplaceTypeRegardingTag(b: BlackBox, str: String): String = {
    var str_tmp = str

    if(b.isUsingNoNumericType){
      str_tmp = str_tmp.replace("unsigned", "std_logic_vector")
      str_tmp = str_tmp.replace("signed",   "std_logic_vector")
    }
    if (b.isUsingULogic) {
      str_tmp = str_tmp.replace("std_logic", "std_ulogic")
    }

    return str_tmp
  }

  def emitBlackBoxComponent(component: BlackBox): Unit = {

    declarations ++= s"\n  component ${component.definitionName} is\n"
    val genericFlat = component.genericElements
    if (genericFlat.size != 0) {
      declarations ++= s"    generic( \n"
      //
      for (e <- genericFlat) {
        e match {
          case (name: String, bt: BaseType)     => declarations ++= s"      $name : ${emitDataType(bt, true)} ${(if(component.isDefaultGenericValue) s":= ${emitExpression(bt.head.source)}" else "")};\n"
          case (name: String, s: String)        => declarations ++= s"      $name : string ${(if(component.isDefaultGenericValue) s":= ${"\""}${s}${"\""}" else "")};\n"
          case (name: String, i: Int)           => declarations ++= s"      $name : integer ${(if(component.isDefaultGenericValue) s":= $i" else "")};\n"
          case (name: String, d: Double)        => declarations ++= s"      $name : real ${(if(component.isDefaultGenericValue) s":= $d" else "")};\n"
          case (name: String, boolean: Boolean) => declarations ++= s"      $name : boolean ${(if(component.isDefaultGenericValue) s":= $boolean" else "")};\n"
          case (name: String, t: TimeNumber)    => declarations ++= s"      $name : time ${(if(component.isDefaultGenericValue) s":= ${t.decompose._1} ${t.decompose._2}" else "")};\n"
        }
      }

      declarations.setCharAt(declarations.size - 2, ' ')
      declarations ++= s"    );\n"
    }

    declarations ++= s"    port( \n"

    component.getOrdredNodeIo.foreach {
      case baseType: BaseType =>
        if (baseType.isIo && !baseType.isSuffix) {
          declarations ++= s"      ${baseType.getName()} : ${emitDirection(baseType)} ${blackBoxReplaceTypeRegardingTag(component, emitDataType(baseType, false))};\n"
        }
      case _ =>
    }

    declarations.setCharAt(declarations.size - 2, ' ')
    declarations ++= s"    );\n"
    declarations ++= s"  end component;\n"
    declarations ++= s"  \n"
  }


  def refImpl(e: BaseType): String = emitReference(e, true)

  def operatorImplAsBinaryOperator(vhd: String)(e: BinaryOperator): String = {
    s"(${emitExpression(e.left)} $vhd ${emitExpression(e.right)})"
  }

  def operatorImplAsBinaryOperatorStdCast(vhd: String)(e: BinaryOperator): String = {
    s"pkg_toStdLogic(${emitExpression(e.left)} $vhd ${emitExpression(e.right)})"
  }

  def boolLiteralImpl(e: BoolLiteral): String = s"pkg_toStdLogic(${e.value})"

  def moduloImpl(e: Operator.BitVector.Mod): String = {
    s"resize(${emitExpression(e.left)} mod ${emitExpression(e.right)},${e.getWidth})"
  }

  def operatorImplAsUnaryOperator(vhd: String)(e: UnaryOperator): String = {
    s"($vhd ${emitExpression(e.source)})"
  }

  def opImplAsCast(vhd: String)(e: Cast): String = {
    s"$vhd(${emitExpression(e.input)})"
  }

  def binaryOperatorImplAsFunction(vhd: String)(e: BinaryOperator): String = {
    s"$vhd(${emitExpression(e.left)},${emitExpression(e.right)})"
  }

  def unaryOperatorImplAsFunction(vhd: String)(e: UnaryOperator): String = {
    s"$vhd(${emitExpression(e.source)})"
  }

  def muxImplAsFunction(vhd: String)(e: BinaryMultiplexer): String = {
    s"$vhd(${emitExpression(e.cond)},${emitExpression(e.whenTrue)},${emitExpression(e.whenFalse)})"
  }

  def shiftRightByIntImpl(e: Operator.BitVector.ShiftRightByInt): String = {
    s"pkg_shiftRight(${emitExpression(e.source)},${e.shift})"
  }

  def shiftLeftByIntImpl(e: Operator.BitVector.ShiftLeftByInt): String = {
    s"pkg_shiftLeft(${emitExpression(e.source)},${e.shift})"
  }

  def shiftRightByIntFixedWidthImpl(e: Operator.BitVector.ShiftRightByIntFixedWidth): String = {
    s"shift_right(${emitExpression(e.source)},${e.shift})"
  }

  def shiftLeftByIntFixedWidthImpl(e: Operator.BitVector.ShiftLeftByIntFixedWidth): String = {
    s"shift_left(${emitExpression(e.source)},${e.shift})"
  }

  def shiftRightBitsByIntFixedWidthImpl(e: Operator.BitVector.ShiftRightByIntFixedWidth): String = {
    s"std_logic_vector(shift_right(unsigned(${emitExpression(e.source)}),${e.shift}))"
  }

  def shiftLeftBitsByIntFixedWidthImpl(e: Operator.BitVector.ShiftLeftByIntFixedWidth): String = {
    s"std_logic_vector(shift_left(unsigned(${emitExpression(e.source)}),${e.shift}))"
  }


  def shiftLeftByUIntFixedWidthImpl(e: Operator.BitVector.ShiftLeftByUIntFixedWidth): String = {
    s"shift_left(${emitExpression(e.left)},to_integer(${emitExpression(e.right)}))"
  }

  def shiftLeftBitsByUIntFixedWidthImpl(e: Operator.BitVector.ShiftLeftByUIntFixedWidth): String = {
    s"std_logic_vector(shift_left(unsigned(${emitExpression(e.left)}),to_integer(${emitExpression(e.right)})))"
  }

  def shiftSIntLeftByUInt(e: Operator.SInt.ShiftLeftByUInt): String = {
    s"pkg_shiftLeft(${emitExpression(e.left)}, ${emitExpression(e.right)}, ${e.getWidth})"
  }

  def resizeFunction(vhdlFunc: String)(e: Resize): String = {
    s"pkg_resize(${emitExpression(e.input)},${e.size})"
  }

  def emitBitsLiteral(e: BitsLiteral): String = {
    s"pkg_stdLogicVector(${'\"'}${e.getBitsStringOn(e.getWidth, if(spinalConfig.dontCareGenAsZero) '0' else 'X')}${'\"'})"
  }

  def emitUIntLiteral(e: UIntLiteral): String = {
    s"pkg_unsigned(${'\"'}${e.getBitsStringOn(e.getWidth, if(spinalConfig.dontCareGenAsZero) '0' else 'X')}${'\"'})"
  }

  def emitSIntLiteral(e : SIntLiteral): String = {
    s"pkg_signed(${'\"'}${e.getBitsStringOn(e.getWidth, if(spinalConfig.dontCareGenAsZero) '0' else 'X')}${'\"'})"
  }

  def emitEnumLiteralWrap(e: EnumLiteral[_  <: SpinalEnum]): String = {
    emitEnumLiteral(e.senum, e.encoding)
  }

  def enumEgualsImpl(eguals: Boolean)(e: BinaryOperator with EnumEncoded): String = {
    val enumDef  = e.getDefinition
    val encoding = e.getEncoding
    encoding match {
      //  case `binaryOneHot` => s"pkg_toStdLogic((${emitExpression(binOp.left)} and ${emitExpression(binOp.right)}) ${if (eguals) "/=" else "="} ${'"' + "0" * encoding.getWidth(enumDef) + '"'})"
      case _ => s"pkg_toStdLogic(${emitExpression(e.left)} ${if (eguals) "=" else "/="} ${emitExpression(e.right)})"
    }
  }

  def operatorImplAsBitsToEnum(e: CastBitsToEnum): String = {
    val enumDef  = e.getDefinition
    val encoding = e.encoding

    if (!encoding.isNative) {
      emitExpression(e.input)
    } else {
      s"pkg_to${enumDef.getName()}_${encoding.getName()}(${emitExpression(e.input)})"
    }
  }

  def operatorImplAsEnumToBits(e: CastEnumToBits): String = {
    val enumDef  = e.input.getDefinition
    val encoding = e.input.getEncoding

    if (!encoding.isNative) {
      emitExpression(e.input)
    } else {
      s"pkg_toStdLogicVector_${encoding.getName()}(${emitExpression(e.input)})"
    }
  }

  def operatorImplAsEnumToEnum(e: CastEnumToEnum): String = {
    val enumDefSrc  = e.input.getDefinition
    val encodingSrc = e.input.getEncoding
    val enumDefDst  = e.getDefinition
    val encodingDst = e.getEncoding

    if (encodingDst.isNative && encodingSrc.isNative)
      emitExpression(e.input)
    else {
      s"${getReEncodingFuntion(enumDefDst, encodingSrc,encodingDst)}(${emitExpression(e.input)})"
    }
  }

  def emitEnumPoison(e: Expression): String = {
    val dc = e.asInstanceOf[EnumPoison]
    if(dc.encoding.isNative)
      dc.senum.elements.head.getName()
    else
      s"(${'"'}${(if(spinalConfig.dontCareGenAsZero) "0" else "X") * dc.encoding.getWidth(dc.senum)}${'"'})"
  }

  def accessBoolFixed(e: BitVectorBitAccessFixed): String = {
    s"pkg_extract(${emitExpression(e.source)},${e.bitId})"
  }

  def accessBoolFloating(e: BitVectorBitAccessFloating): String = {
    s"pkg_extract(${emitExpression(e.source)},to_integer(${emitExpression(e.bitId)}))"
  }

  def accessBitVectorFixed(e: BitVectorRangedAccessFixed): String = {
    s"pkg_extract(${emitExpression(e.source)},${e.hi},${e.lo})"
  }

  def accessBitVectorFloating(e: BitVectorRangedAccessFloating): String = {
    s"pkg_extract(${emitExpression(e.source)},${emitExpression(e.offset)},${e.size})"
  }

  def dispatchExpression(e: Expression) :  String = e match {
    case  e: BaseType                                => refImpl(e)

    case  e: BoolLiteral                             => boolLiteralImpl(e)
    case  e: BitsLiteral                             => emitBitsLiteral(e)
    case  e: UIntLiteral                             => emitUIntLiteral(e)
    case  e: SIntLiteral                             => emitSIntLiteral(e)
    case  e: EnumLiteral[_]                          => emitEnumLiteralWrap(e)

    case  e: BoolPoison                              => (if(spinalConfig.dontCareGenAsZero) "'0'" else "'X'")
    case  e: EnumPoison                              => emitEnumPoison(e)

    //unsigned
    case  e: Operator.UInt.Add                       => operatorImplAsBinaryOperator("+")(e)
    case  e: Operator.UInt.Sub                       => operatorImplAsBinaryOperator("-")(e)
    case  e: Operator.UInt.Mul                       => operatorImplAsBinaryOperator("*")(e)
    case  e: Operator.UInt.Div                       => operatorImplAsBinaryOperator("/")(e)
    case  e: Operator.UInt.Mod                       => moduloImpl(e)

    case  e: Operator.UInt.Or                        => operatorImplAsBinaryOperator("or")(e)
    case  e: Operator.UInt.And                       => operatorImplAsBinaryOperator("and")(e)
    case  e: Operator.UInt.Xor                       => operatorImplAsBinaryOperator("xor")(e)
    case  e: Operator.UInt.Not                       => unaryOperatorImplAsFunction("pkg_not")(e) //workaround cadence incisive 15.20

    case  e: Operator.UInt.Equal                     => operatorImplAsBinaryOperatorStdCast("=")(e)
    case  e: Operator.UInt.NotEqual                  => operatorImplAsBinaryOperatorStdCast("/=")(e)
    case  e: Operator.UInt.Smaller                   =>  operatorImplAsBinaryOperatorStdCast("<")(e)
    case  e: Operator.UInt.SmallerOrEqual            => operatorImplAsBinaryOperatorStdCast("<=")(e)

    case  e: Operator.UInt.ShiftRightByInt           => shiftRightByIntImpl(e)
    case  e: Operator.UInt.ShiftLeftByInt            => shiftLeftByIntImpl(e)
    case  e: Operator.UInt.ShiftRightByUInt          => binaryOperatorImplAsFunction("pkg_shiftRight")(e)
    case  e: Operator.UInt.ShiftLeftByUInt           => binaryOperatorImplAsFunction("pkg_shiftLeft")(e)
    case  e: Operator.UInt.ShiftRightByIntFixedWidth =>  shiftRightByIntFixedWidthImpl(e)
    case  e: Operator.UInt.ShiftLeftByIntFixedWidth  =>  shiftLeftByIntFixedWidthImpl(e)
    case  e: Operator.UInt.ShiftLeftByUIntFixedWidth =>  shiftLeftByUIntFixedWidthImpl(e)

    //signed
    case  e: Operator.SInt.Add                       => operatorImplAsBinaryOperator("+")(e)
    case  e: Operator.SInt.Sub                       => operatorImplAsBinaryOperator("-")(e)
    case  e: Operator.SInt.Mul                       => operatorImplAsBinaryOperator("*")(e)
    case  e: Operator.SInt.Div                       => operatorImplAsBinaryOperator("/")(e)
    case  e: Operator.SInt.Mod                       => moduloImpl(e)

    case  e: Operator.SInt.Or                        => operatorImplAsBinaryOperator("or")(e)
    case  e: Operator.SInt.And                       => operatorImplAsBinaryOperator("and")(e)
    case  e: Operator.SInt.Xor                       => operatorImplAsBinaryOperator("xor")(e)
    case  e: Operator.SInt.Not                       => unaryOperatorImplAsFunction("pkg_not")(e) //workaround cadence incisive 15.20
    case  e: Operator.SInt.Minus                     => operatorImplAsUnaryOperator("-")(e)

    case  e: Operator.SInt.Equal                     => operatorImplAsBinaryOperatorStdCast("=")(e)
    case  e: Operator.SInt.NotEqual                  => operatorImplAsBinaryOperatorStdCast("/=")(e)
    case  e: Operator.SInt.Smaller                   => operatorImplAsBinaryOperatorStdCast("<")(e)
    case  e: Operator.SInt.SmallerOrEqual            => operatorImplAsBinaryOperatorStdCast("<=")(e)


    case  e: Operator.SInt.ShiftRightByInt           => shiftRightByIntImpl(e)
    case  e: Operator.SInt.ShiftLeftByInt            => shiftLeftByIntImpl(e)
    case  e: Operator.SInt.ShiftRightByUInt          => binaryOperatorImplAsFunction("pkg_shiftRight")(e)
    case  e: Operator.SInt.ShiftLeftByUInt           => shiftSIntLeftByUInt(e)
    case  e: Operator.SInt.ShiftRightByIntFixedWidth => shiftRightByIntFixedWidthImpl(e)
    case  e: Operator.SInt.ShiftLeftByIntFixedWidth  => shiftLeftByIntFixedWidthImpl(e)
    case  e: Operator.SInt.ShiftLeftByUIntFixedWidth => shiftLeftByUIntFixedWidthImpl(e)

    //bits
    case  e: Operator.Bits.Cat                       => binaryOperatorImplAsFunction("pkg_cat")(e)

    case  e: Operator.Bits.Or                        => operatorImplAsBinaryOperator("or")(e)
    case  e: Operator.Bits.And                       => operatorImplAsBinaryOperator("and")(e)
    case  e: Operator.Bits.Xor                       => operatorImplAsBinaryOperator("xor")(e)
    case  e: Operator.Bits.Not                       => unaryOperatorImplAsFunction("pkg_not")(e) //workaround cadence incisive 15.20

    case  e: Operator.Bits.Equal                     => operatorImplAsBinaryOperatorStdCast("=")(e)
    case  e: Operator.Bits.NotEqual                  => operatorImplAsBinaryOperatorStdCast("/=")(e)

    case  e: Operator.Bits.ShiftRightByInt           => shiftRightByIntImpl(e)
    case  e: Operator.Bits.ShiftLeftByInt            => shiftLeftByIntImpl(e)
    case  e: Operator.Bits.ShiftRightByUInt          => binaryOperatorImplAsFunction("pkg_shiftRight")(e)
    case  e: Operator.Bits.ShiftLeftByUInt           => binaryOperatorImplAsFunction("pkg_shiftLeft")(e)
    case  e: Operator.Bits.ShiftRightByIntFixedWidth => shiftRightBitsByIntFixedWidthImpl(e)
    case  e: Operator.Bits.ShiftLeftByIntFixedWidth  => shiftLeftBitsByIntFixedWidthImpl(e)
    case  e: Operator.Bits.ShiftLeftByUIntFixedWidth => shiftLeftBitsByUIntFixedWidthImpl(e)

    //bool
    case  e: Operator.Bool.Equal                     => operatorImplAsBinaryOperatorStdCast("=")(e)
    case  e: Operator.Bool.NotEqual                  => operatorImplAsBinaryOperatorStdCast("/=")(e)

    case  e: Operator.Bool.Not                       => operatorImplAsUnaryOperator("not")(e)
    case  e: Operator.Bool.And                       => operatorImplAsBinaryOperator("and")(e)
    case  e: Operator.Bool.Or                        => operatorImplAsBinaryOperator("or")(e)
    case  e: Operator.Bool.Xor                       => operatorImplAsBinaryOperator("xor")(e)

    //senum
    case  e: Operator.Enum.Equal                     => enumEgualsImpl(true)(e)
    case  e: Operator.Enum.NotEqual                  => enumEgualsImpl(false)(e)

    //cast
    case  e: CastSIntToBits                          => opImplAsCast("std_logic_vector")(e)
    case  e: CastUIntToBits                          => opImplAsCast("std_logic_vector")(e)
    case  e: CastBoolToBits                          => opImplAsCast("pkg_toStdLogicVector")(e)
    case  e: CastEnumToBits                          => operatorImplAsEnumToBits(e)

    case  e: CastBitsToSInt                          => opImplAsCast("signed")(e)
    case  e: CastUIntToSInt                          => opImplAsCast("signed")(e)

    case  e: CastBitsToUInt                          => opImplAsCast("unsigned")(e)
    case  e: CastSIntToUInt                          => opImplAsCast("unsigned")(e)

    case  e: CastBitsToEnum                          => operatorImplAsBitsToEnum(e)
    case  e: CastEnumToEnum                          => operatorImplAsEnumToEnum(e)

    //misc
    case  e: ResizeSInt                              => resizeFunction("pkg_signed")(e)
    case  e: ResizeUInt                              => resizeFunction("pkg_unsigned")(e)
    case  e: ResizeBits                              => resizeFunction("pkg_stdLogicVector")(e)

    case  e: BinaryMultiplexer                       => muxImplAsFunction("pkg_mux")(e)

    case  e: BitVectorBitAccessFixed                 => accessBoolFixed(e)
    case  e: BitVectorBitAccessFloating              => accessBoolFloating(e)
    case  e: BitVectorRangedAccessFixed              => accessBitVectorFixed(e)
    case  e: BitVectorRangedAccessFloating           => accessBitVectorFloating(e)
  }

  elaborate()
  emitEntity()
  emitArchitecture()
}
