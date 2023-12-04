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

package spinal.core

import spinal.core.fiber._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._
import spinal.idslplugin.PostInitCallback


object Component {

  /** Push a new component on the stack */
  def push(c: Component) = DslScopeStack.set(if(c != null) c.dslBody else null)
//
//  /**  Remove component of the stack if it is the same as c */
//  def pop(c: Component): Unit = {
//    val globalData = if(c != null) c.globalData else GlobalData.get
//    DslScopeStack.pop()
//  }

  /** Get the current component on the stack */
  def current: Component = DslScopeStack.get match {
    case null  => null
    case scope => scope.component
  }

  def toplevel : Component = {
    var ptr = Component.current
    while(ptr.parent != null) ptr = ptr.parent
    ptr
  }
}


/**
  * Abstract class used to create a new Component
  *
  * @example {{{
  *         class MyAndGate extends Component {
  *           val io = new Bundle{
  *             val a,b = in Bool()
  *             val res = out Bool()
  *           }
  *           io.res := io.a & io.b
  *         }
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/components_hierarchy Component Documentation]]
  */
abstract class Component extends NameableByComponent with ContextUser with ScalaLocated with PostInitCallback with Stackable with OwnableRef with SpinalTagReady with OverridedEqualsHashCode with ValCallbackRec {
  if(parentScope == null) {
    if(globalData.config.singleTopLevel) {
      if (globalData.toplevel != null) SpinalError(s"MULTIPLE TOPLEVEL : SpinalHDL only allows a single toplevel Component.\n- ${globalData.toplevel.getClass}\n- ${this.getClass}")
      assert(globalData.phaseContext.topLevel == null, "???")
    }
    globalData.toplevel = this
    globalData.phaseContext.topLevel = this
  }
  val dslBody = new ScopeStatement(null)

  dslBody.component = this

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  val definition = new SpinalTagReady {
    def addComment(str : String) = this.addTag(new CommentTag(str))
  }

  def addComment(str : String) = this.addTag(new CommentTag(str))

  var withVitalOutputs = false
  def asFormalDut() : this.type = {
    withVitalOutputs = true
    this
  }


  var withHierarchyAutoPull = false
  def withAutoPull(): Unit ={
    withHierarchyAutoPull = true
  }

  var isFormalTester = false
  def setFormalTester(): Unit ={
    isFormalTester = true
  }

  def isLogicLess = dslBody.isEmpty && children.isEmpty

  /** Contains all in/out signals of the component */
  private[core] val ioSet = mutable.LinkedHashSet[BaseType]()

  /** Class used to create a task that must be executed after the creation of the component */
  case class PrePopTask(task : () => Unit){
    val context = ScopeProperty.capture()
  }

  /** Array of PrePopTask */
  private[core] var prePopTasks = mutable.ArrayBuffer[PrePopTask]()
  /** enable/disable "io_" prefix in front of the in/out signals in the RTL */
  private[core] var ioPrefixEnable = true
  /** Used to store arbitrary object related to the component */
  val userCache = mutable.Map[Any, Any]()

  /** Definition Name (name of the entity (VHDL) or module (Verilog))*/
  var definitionName: String = null
  var definitionNameNoMerge = false

  /** Hierarchy level of the component */
  private[core] val level : Int = if(parent == null) 0 else parent.level + 1
  /** Contains an array of all children Component */
  val children = ArrayBuffer[Component]()
  def walkComponents(body : Component => Unit) : Unit = {
    body(this)
    children.foreach(_.walkComponents(body))
  }

  var traceEnabled = true

  def traceDisable(recursive : Boolean = true) : this.type = {
    traceEnabled = false
    if(recursive) this.children.foreach(_.traceDisable(true))
    this
  }

  def traceEnable(recursive : Boolean = true) : this.type = {
    traceEnabled = true
    if(recursive) this.children.foreach(_.traceEnable(true))
    this
  }

  var pulledDataCache = mutable.Map[Data, Data]()

  /** Get the parent component (null if there is no parent)*/
  def parent: Component = if(parentScope != null) parentScope.component else null
  /** Get the current clock domain (null if there is no clock domain already set )*/
  val clockDomain = ClockDomain.currentHandle

  var withoutReservedKeywords = false
  def withoutKeywords(): Unit ={
    withoutReservedKeywords = true
  }
  def withKeywords(): Unit ={
    withoutReservedKeywords = false
  }

  // Check if it is a top level component or a children of another component
  if (parent != null) {
    parent.children += this
  } else {
    setName("toplevel", Nameable.DATAMODEL_WEAK)
  }

  // Push component on the stack
  Component.push(this)

  def prePop(): Unit ={
    while(prePopTasks.nonEmpty){
      val prePopTasksToDo = prePopTasks
      prePopTasks = mutable.ArrayBuffer[PrePopTask]()

      val ctx = ScopeProperty.captureNoClone()
      for(t <- prePopTasksToDo){
        t.context.restoreCloned()
        t.task()
      }
      ctx.restore()
    }
  }

  def postInitCallback(): this.type = {
    prePop()
    ClockDomain.push(clockDomain)
    DslScopeStack.set(parentScope)
    this
  }

  /** Add a new prePopTask */
  def addPrePopTask(task: () => Unit) = prePopTasks += PrePopTask(task)
//  def addPrePopTask(task: () => Unit) = Engine.get.onCompletion += task
  def afterElaboration(body : => Unit) = addPrePopTask(() => body)

  /** Set the definition name of the component */
  def setDefinitionName(name: String, noMerge : Boolean = true): this.type = {
    definitionName = name
    definitionNameNoMerge = noMerge
    this
  }

  /** No "io_" prefix in front of the in/out signals */
  def noIoPrefix(): this.type = {
    val io = reflectIo
    if(io != null) {
      io.setName("")
    }
    ioPrefixEnable = false
    this
  }

  /** Check if this component is the top level */
  private[core] def isTopLevel: Boolean = parent == null

  /** Return a list of all parents of the components */
  def parents(of: Component = this, list: List[Component] = Nil): List[Component] = {
    if (of.parent == null) return list
    parents(of.parent, of.parent :: list)
  }

  /**
    * Get the IO definition of the component
    * @example {{{ val io = new Bundle { ... } }}}
    */
  private[core] def reflectIo: Data = {
    try {
      val m = this.getClass.getMethod("io")
      m.invoke(this).asInstanceOf[Data]
    } catch {
      case _: Throwable => null
    }
  }


  override def valCallbackRec(ref: Any, name: String): Unit = {
    if(name == "io"){
      ref match {
        case io: Nameable =>
          if (ioPrefixEnable)
            io.setName("io", Nameable.DATAMODEL_WEAK)
          else
            io.setName("", Nameable.DATAMODEL_WEAK)
          OwnableRef.proposal(io,this)
        case _ =>
      }
    } else {
      ref match {
        case component: Component if component.parent == this =>
          OwnableRef.proposal(ref, this)
          component.setName(name, Nameable.DATAMODEL_WEAK)

        case nameable: Nameable =>
          if (!nameable.isInstanceOf[ContextUser]) {
            nameable.setName(name, Nameable.DATAMODEL_WEAK)
            OwnableRef.proposal(ref, this)
          } else if (nameable.asInstanceOf[ContextUser].component == this) {
            nameable.setName(name, Nameable.DATAMODEL_WEAK)
            OwnableRef.proposal(ref, this)
          } else {
            for (kind <- children) {
              //Allow to name a component by his io reference into the parent component
              if (kind.reflectIo == nameable) {
                kind.setName(name, Nameable.DATAMODEL_WEAK)
                OwnableRef.proposal(kind, this)
              }
            }
          }
        case _ =>
      }
    }
  }

  /**
    * Name allocation
    */
  var localNamingScope : NamingScope = null
  private[core] def allocateNames(globalScope: NamingScope): Unit = {

    localNamingScope = if(withoutReservedKeywords) new NamingScope(globalScope.duplicationPostfix) else globalScope.newChild()
    val anonymPrefix = if(globalData.phaseContext.config.anonymSignalUniqueness) globalData.anonymSignalPrefix + "_" + this.definitionName else globalData.anonymSignalPrefix
    localNamingScope.allocateName(anonymPrefix)

    for (child <- children) {
      OwnableRef.proposal(child, this)
      if (child.isUnnamed) {
        var name = classNameOf(child)
        name = Character.toLowerCase(name.charAt(0)) + (if (name.length() > 1) name.substring(1) else "")
        child.unsetName().setName(name, Nameable.DATAMODEL_WEAK)
      }
      child.setName(localNamingScope.allocateName(child.getName()), Nameable.DATAMODEL_STRONG)
    }

    dslBody.walkStatements{
      case nameable : Nameable =>
        if (nameable.isUnnamed || nameable.getName() == "") {
          nameable.unsetName().setName(anonymPrefix, Nameable.DATAMODEL_WEAK)
        }
        if (nameable.isWeak)
          nameable.setName(localNamingScope.allocateName(nameable.getName()), Nameable.DATAMODEL_STRONG)
        else
          localNamingScope.iWantIt(nameable.getName(), s"Reserved name ${nameable.getName()} is not free for ${nameable.toString()} defined at \n${nameable.getScalaLocationLong}")
      case _ =>
    }
  }

  /** Get a set of all IO available in the component */
  def getAllIo: mutable.Set[BaseType] = ioSet

  /** Sort all IO regarding instanceCounter */
  def getOrdredNodeIo = getAllIo.toList.sortWith(_.instanceCounter < _.instanceCounter)

  private[core] def userParentCalledDef(): Unit = {}

  private[core] def isInBlackBoxTree: Boolean = if (parent == null) false else parent.isInBlackBoxTree

  private[core] override def getComponent(): Component = parent

  override def getDisplayName(): String = if (isNamed) super.getDisplayName() else "[" + getClass.getSimpleName + "]"

  /**
    * Return the path of the parent
    *
    * @example{{{ toplevel/[myComponent1] // Current component is myComponent2 }}}
    */
  def getParentsPath(sep: String = "/"): String = if (parent == null) "" else parents().map(_.getDisplayName()).reduce(_ + sep + _)

  /**
    * Return the path of the component
    *
    * @example{{{ toplevel/[myComponent1]/[myComponent2] // Current component is myComponent2 }}}
    */
  def getPath(sep: String = "/"): String = (if (parent == null) "" else getParentsPath(sep) + sep) + this.getDisplayName()
  def getRtlPath(separator : String = "/") : String = {
    if(parent == null) "" else (parents().tail :+this) .mkString(separator)
  }

  def getGroupedIO(ioBundleBypass: Boolean): Seq[Data] = {
    val ret      = mutable.Set[Data]()
    val ioBundle = if (ioBundleBypass) reflectIo else null

    def getRootParent(that: Data): Data = if (that.parent == null || that.parent == ioBundle) that else getRootParent(that.parent)

    for (e <- getOrdredNodeIo) {
      ret += getRootParent(e)
    }

    ret.toSeq.sortBy(_.instanceCounter)
  }

  override def postPushEvent(): Unit = {}
  override def prePopEvent(): Unit = {}

  /** Rework the component */
  val scopeProperties = ScopeProperty.capture()
  def rework[T](gen: => T) : T = {
    val previousTasks = this.prePopTasks
    this.prePopTasks = mutable.ArrayBuffer[PrePopTask]()
    val ctx = ScopeProperty.captureNoClone()

    scopeProperties.restoreCloned()
    val ret = gen
    prePop()
    ctx.restore()

    prePopTasks = previousTasks
    ret
  }

  def reflectBaseType(name : String): BaseType = {
    this.dslBody.walkStatements{
      case bt : BaseType if bt.getName() == name => return bt
      case _ =>
    }
    null
  }

  def onBody[T](body : => T) : T = {
    val ctx = DslScopeStack.set(dslBody)
    val ret = body
    ctx.restore()
    ret
  }

  /**
    * Empty Component, remove logic in component and assign zero on output port as stub
    * @example {{{ val dut = (new MyComponent).stub() }}}
    */
  def stub(): this.type = this.rework{
    // step0: walk and fix clock (clock, reset port need keep)
    PhasePullClockDomains.recursive(this)
    // step1: First remove all we don't want
    this.children.clear()
    this.dslBody.foreachStatements{
      case bt : BaseType if !bt.isDirectionLess =>
      case s => s.removeStatement()
    }
    // step2: remove output and assign zero
    // this step can't merge into step1
    this.dslBody.foreachStatements{
      case bt : BaseType if bt.isOutput | bt.isInOut =>
        bt.removeAssignments()
        bt := bt.getZero
      case s =>
    }
    this
  }
}
