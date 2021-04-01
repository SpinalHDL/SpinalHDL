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

import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._

object DataAssign
object InitAssign
class VarAssignementTag(val from : Data) extends SpinalTag{
  var id = 0
}

trait DataPrimitives[T <: Data]{

  private[spinal] def _data : T

  /** Comparison between two data */
  def ===(that: T): Bool = _data isEquals that
  def =/=(that: T): Bool = _data isNotEquals that

  /** Assign a data to this */
  def := (that: T): Unit = _data assignFrom that

  /** Use as \= to have the same behavioral as VHDL variable */
  def \(that: T): T = {

    val globalData = GlobalData.get

    DslScopeStack.push(_data.parentScope)

    val swapContext = _data.parentScope.swap()
    val ret = cloneOf(that)

    ret := _data

    swapContext.appendBack()
    DslScopeStack.pop()

    ret.allowOverride
    ret := that

    (this, ret) match {
      case (from: Data with Nameable, to: Data with Nameable) => {
        val t = from.getTag(classOf[VarAssignementTag]) match {
          case Some(t) => t
          case None => new VarAssignementTag(from)
        }
        t.id += 1
        to.setCompositeName(t.from,t.id.toString)

        from.removeTag(t)
        ret.addTag(t)
      }
      case _ =>
    }

    ret
  }

  def copyDirectionOf(that : T): Unit ={
    _data.copyDirectionOfImpl(that)
  }

  /** Auto connection between two data */
  def <>(that: T): Unit = _data autoConnect that

  /** Set initial value to a data */
  def init(that: T): T = {
    _data.initFrom(that)
    _data
  }

  /** Set a default value to a data */
  def default(that: => T): T = {
    assert(_data.dir != inout)

    val c = if (_data.dir == in) {
      Component.current.parent
    } else {
      Component.current
    }

    if(c != null) {
      Component.push(c)
      _data.defaultImpl(that)
      Component.pop(c)
    }
    _data
  }
}


/**
  * Should not extends AnyVal, Because it create kind of strange call stack move that make error reporting miss accurate
  */
class DataPimper[T <: Data](val _data: T) extends DataPrimitives[T]{
}


object Data {

  /** Creates a signal path through the component hierarchy to finalComponent to read the srcData signal
    *
    * @param srcData Data that you want to read
    * @param finalComponent Location where you want to read the srcData signal
    * @param useCache If multiple doPull are done on the same signal, allow to reuse the previously created paths
    * @param propagateName The signals created through the hierarchy will get the same name as srcData
    * @tparam T Type of the srcData
    * @return Readable signal in the finalComponent which is driven by srcData
    */
  def doPull[T <: Data](srcData: T, finalComponent: Component, useCache: Boolean = false, propagateName: Boolean = false): T = {

    val startComponent = srcData.component

    if (useCache) {
      val finalComponentCacheState = finalComponent.pulledDataCache.getOrElse(srcData, null)
      if (finalComponentCacheState != null)
        return finalComponentCacheState.asInstanceOf[srcData.type]
    }

    if (startComponent == finalComponent || (startComponent != null && finalComponent == startComponent.parent && srcData.isIo)) {
      finalComponent.pulledDataCache.put(srcData, srcData)
      return srcData
    }

    //Find commonComponent and fill the risePath
    val risePath = ArrayBuffer[Component]()

    //Find the first common parent component between finalComponent and srcData.component
    val commonComponent = {
      var srcPtr = srcData.component
      var dstPtr = finalComponent
      if(dstPtr == null) {
        null
      } else if(srcPtr == null) {
        while (dstPtr != null) {
          risePath += dstPtr
          dstPtr = dstPtr.parent
        }
        null
      }else{
        while (srcPtr.level > dstPtr.level) srcPtr = srcPtr.parent
        while (srcPtr.level < dstPtr.level){
          risePath += dstPtr
          dstPtr = dstPtr.parent
        }
        while (srcPtr != dstPtr) {
          srcPtr  = srcPtr.parent
          risePath += dstPtr
          dstPtr  = dstPtr.parent
        }
        srcPtr
      }
    }

    def push(c: Component, scope: ScopeStatement): Unit = {
      DslScopeStack.push(scope)
      ClockDomain.push(c.clockDomain)
    }

    def pop(c: Component): Unit = {
      assert(Component.current == c)
      DslScopeStack.pop()
      ClockDomainStack.pop()
    }

    var currentData: T = srcData
    var currentComponent: Component = srcData.component

    //Build the path from srcData to the commonComponent (falling path)
    while(currentComponent != commonComponent){
      if(useCache && currentComponent.parent.pulledDataCache.contains(srcData)){
        currentData = currentComponent.parent.pulledDataCache(srcData).asInstanceOf[T]
        currentComponent = currentComponent.parent
      } else {
        if (currentData.component == currentComponent && currentData.isIo) {
          //nothing to do
        } else {
          push(currentComponent, currentComponent.dslBody)
          val copy = cloneOf(srcData).asOutput()
          if (propagateName)
            copy.setPartialName(srcData, "", weak=true)
          copy := currentData
          pop(currentComponent)
          currentData = copy
        }
        currentComponent = currentComponent.parent
        if (useCache)
          currentComponent.pulledDataCache.put(srcData, currentData)
      }
    }

    //Build the path from commonComponent to the targetComponent (rising path)
    for(riseTo <- risePath.reverseIterator){
      if(useCache && riseTo.pulledDataCache.contains(srcData)){
        currentComponent = riseTo
        currentData = riseTo.pulledDataCache(srcData).asInstanceOf[T]
      }else {
        push(riseTo, riseTo.dslBody)
        val copy = cloneOf(srcData).asInput()
        if (propagateName)
          copy.setPartialName(srcData, "", weak=true)
        pop(riseTo)
        if (currentComponent != null) {
          push(currentComponent, riseTo.parentScope)
          copy := currentData
          pop(currentComponent)
        } else {
          copy.addTag(new ExternalDriverTag(currentData))
        }
        currentData = copy

        currentComponent = riseTo
        if (useCache)
          currentComponent.pulledDataCache.put(srcData, currentData)
      }
    }

    if (useCache)
      currentComponent.pulledDataCache.put(srcData, currentData)
    currentData
  }
}


trait InComponent{
  def getComponent() : Component
  /** Get current component with all parents */
  def getComponents(): Seq[Component] = {
    val component = getComponent()
    if(component == null) Nil else component.parents() ++ Seq(component)
  }
}

trait Data extends ContextUser with NameableByComponent with Assignable with SpinalTagReady with GlobalDataUser with ScalaLocated with OwnableRef with OverridedEqualsHashCode with InComponent{

  private[core] var dir: IODirection = null
  private[core] def isIo = dir != null
  private[core] def isSuffix = parent != null && parent.isInstanceOf[Suffixable]

  var parent: Data = null
  def getRootParent: Data = if(parent == null) this else parent.getRootParent

  /** Set a data as input */
  def asInput(): this.type = {
    if(this.component != Component.current) {
      LocatedPendingError(s"You should not set $this as input outside its own component." )
    }else {
      dir = in
    }
    this
  }

  /** Set a data as output */
  def asOutput(): this.type = {
    if(this.component != Component.current) {
      LocatedPendingError(s"You should not set $this as output outside its own component." )
    }else {
      dir = out
    }
    this
  }

  /** set a data as inout */
  def asInOut(): this.type = {
    if(this.component != Component.current) {
      LocatedPendingError(s"You should not set $this as output outside its own component." )
    }else {
      dir = inout
    }
    this
  }

  def copyDirectionOfImpl(that : Data): this.type ={
    if(this.component != Component.current) {
      LocatedPendingError(s"You should not set $this as output outside its own component." )
    }else {
      dir = that.dir
    }
    this
  }

  /** remove the direction (in,out,inout) to a data*/
  def setAsDirectionLess(): this.type = {
    dir = null
    this
  }

  @deprecated("use setAsDirectionLess instead","???")
  def asDirectionLess(): this.type = setAsDirectionLess()

  /** Set baseType to reg */
  def setAsReg(): this.type
  /** Set baseType to Combinatorial */
  def setAsComb(): this.type

  def purify() : this.type = {
    setAsDirectionLess()
    setAsComb()
    removeAssignments()
  }

  def dirString(): String = dir match {
    case `in`    => "in"
    case `out`   => "out"
    case `inout` => "inout"
    case null    => ""
  }

  def isOutput: Boolean = dir == out
  def isInput:  Boolean = dir == in
  def isInOut:  Boolean = dir == inout
  def getDirection = dir

  def isOutputOrInOut: Boolean = dir == out || dir == inout
  def isInputOrInOut:  Boolean = dir ==  in || dir == inout
  def isDirectionLess: Boolean = dir == null

  /** flip the direction of the data */
  def flip(): this.type  = {
    dir match {
      case `in`    => dir = out
      case `out`   => dir = in
      case `inout` =>
      case _       => LocatedPendingError(s"Can't flip a data that is direction less ($this)")
    }
    this
  }

  final def assignFrom(that: AnyRef, target: AnyRef = this) = compositAssignFrom(that, target, DataAssign)

  final def initFrom(that: AnyRef, target: AnyRef = this) = (that, target) match {
    case (init: Data, target: Data) if ! target.isReg =>
      for ((e, initElement) <- (target.flatten, init.flatten).zipped) {
        def recursiveSearch(bt: BaseType): Unit = {
          if (bt.isReg)
            bt.init (initElement)
          else if(Statement.isFullToFullStatement(bt))
            recursiveSearch(bt.head.source.asInstanceOf[BaseType])
          else
            LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
        }
        recursiveSearch(e)
      }
    case _ => compositAssignFrom(that,target,InitAssign)
  }

  def asData = this.asInstanceOf[Data]

  /** Create a data set to 0*/
  def getZero: this.type

  def flatten: Seq[BaseType]
  def flattenLocalName: Seq[String]
  def flattenForeach(body : BaseType => Unit) : Unit = flatten.foreach(body(_))
  /** Pull a signal to the top level (use for debugging) */
  def pull(): this.type = Data.doPull(this, Component.current, useCache = false, propagateName = false)

  /** Concatenation between two data */
  def ##(right: Data): Bits = this.asBits ## right.asBits

  /** Cast data to Bits */
  def asBits: Bits

  def assignFromBits(bits: Bits): Unit
  def assignFromBits(bits: Bits, hi: Int, low: Int): Unit
  def assignFromBits(bits: Bits, offset: Int, bitCount: BitCount): Unit = this.assignFromBits(bits, offset + bitCount.value - 1, offset)

  def as[T <: Data](dataType: HardType[T]) : T = {
    val ret = dataType()
    ret.assignFromBits(this.asBits)
    ret
  }

  def assignDontCare(): this.type = {
    flatten.foreach(_.assignDontCare())
    this
  }

  def removeAssignments(): this.type = {
    flattenForeach(_.removeAssignments())
    this
  }

  private[core] def isEquals(that: Any): Bool
  private[core] def isNotEquals(that: Any): Bool

  /** Resized data regarding target */
  def resized: this.type = {
    val ret = cloneOf(this)
    ret.assignFrom(this)
    ret.addTag(tagAutoResize)
    return ret.asInstanceOf[this.type]
  }

  /** Allow a data to be overrided */
  def allowOverride: this.type = {
    addTag(allowAssignmentOverride)
  }

  def allowDirectionLessIo: this.type = {
    addTag(allowDirectionLessIoTag)
  }

  def allowPartialyAssigned : this.type = {
    addTag(AllowPartialyAssignedTag)
  }

  def allowUnsetRegToAvoidLatch: this.type = {
    addTag(unsetRegIfNoAssignementTag)
  }

  def noCombLoopCheck : this.type = {
    addTag(spinal.core.noCombinatorialLoopCheck)
  }

  def noBackendCombMerge : this.type = {
    addTag(spinal.core.noBackendCombMerge)
  }

  private[core] def autoConnect(that: Data): Unit// = (this.flatten, that.flatten).zipped.foreach(_ autoConnect _)

  private[core] def autoConnectBaseImpl(that: Data): Unit = {

    def getTrueIoBaseType(that: Data): Data = that.getRealSource.asInstanceOf[Data]

    val thisTrue = getTrueIoBaseType(this)
    val thatTrue = getTrueIoBaseType(that)

    val c = Component.current

    if(thisTrue.component != c && thisTrue.component.parent != c){
      LocatedPendingError(s"HIERARCHY VIOLATION, $thisTrue can't be used in $c at")
    }else if(thatTrue.component != c && thatTrue.component.parent != c){
      LocatedPendingError(s"HIERARCHY VIOLATION, $thatTrue can't be used in $c at")
    } else {
      def dirSolve(that: Data): IODirection = {
        if(that.component == c)
          that.dir
        else
          that.dir match {
            case `in`    => out
            case `out`   => in
            case `inout` => inout
            case null    => null
          }
      }

      val thisDir = dirSolve(thisTrue)
      val thatDir = dirSolve(thatTrue)

      (thisDir,thatDir) match {
        case (`out`,`in`)                         => this := that
        case (`out`,null)                         => this := that
        case (`in`,`out`)                         => that := this
        case (`in`,null)                          => that := this
        case (null,`in`)                          => this := that
        case (null,`out`)                         => that := this
        case _ if this.isAnalog && that.isAnalog  => this := that
        case _                                    => LocatedPendingError(s"DIRECTION MISMATCH, impossible to infer the connection direction between $this and $that ")
      }
    }
  }

  /** Return the width of the data */
  def getBitsWidth: Int

  def keep(): this.type = {
//    flatten.foreach(t => t.component.additionalNodesRoot += t);
    dontSimplifyIt()
    this
  }

  def dontSimplifyIt(): this.type = {
    flatten.foreach(_.dontSimplifyIt())
    this
  }

  def allowSimplifyIt(): this.type = {
    flatten.foreach(_.allowSimplifyIt())
    this
  }

  override def addAttribute(attribute: Attribute): this.type = {
    flatten.foreach(_.addAttribute(attribute))
    this
  }

  def isReg:    Boolean = flatten.forall(_.isReg)
  def isComb:   Boolean = flatten.forall(_.isComb)
  def isAnalog: Boolean = flatten.forall(_.isAnalog)

  def setAsAnalog(): this.type = {flatten.foreach(_.setAsAnalog()); this}

  override def getRealSourceNoRec: Any = this

  private[core] def defaultImpl(init: Data): this.type = {
    val regInit = clone()
    regInit := init

    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      e.addTag(new DefaultTag(initElement))
    }

    this
  }

  /**
    * Useful for register that doesn't need a reset value in RTL,
    * but need a random value for simulation (avoid x-propagation)
    */
  def randBoot(): this.type = {
    if(!globalData.phaseContext.config.noRandBoot) flatten.foreach(_.addTag(spinal.core.randomBoot))
    this
  }

  def allowPruning() : this.type = {
    flatten.foreach(_.addTag(unusedTag))
    this
  }

  override def getComponent(): Component = component

  override def clone: Data = {
    try {
      val clazz            = this.getClass
      val constructor      = clazz.getConstructors.head
      val constrParamCount = constructor.getParameterTypes.length

      //No param =>
      if (constrParamCount == 0) return constructor.newInstance().asInstanceOf[this.type]

      def cleanCopy[T <: Data](that: T): T = {
        that.purify()
        that
      }

      def constructorParamsAreVal: this.type = {
        val outer         = clazz.getFields.find(_.getName == "$outer")
        val constructor   = clazz.getDeclaredConstructors.head
        val argumentCount = constructor.getParameterTypes.length - (if (outer.isDefined) 1 else 0)
        val fields        = clazz.getDeclaredFields

        val arguments = (0 until argumentCount) map { i =>
          val fieldName = fields(i).getName
          val getter    = clazz.getMethod(fieldName)
          val arg       = getter.invoke(this)

          if(arg.isInstanceOf[Data]){
            cloneOf(arg.asInstanceOf[Data])
          } else{
            arg
          }
        }

        if (outer.isEmpty)
          return cleanCopy(constructor.newInstance(arguments: _*).asInstanceOf[this.type])
        else {
          val args = (outer.get.get(this) :: Nil) ++ arguments
          return cleanCopy(constructor.newInstance(args: _*).asInstanceOf[this.type])
        }
      }

      //Case class =>
      if (ScalaUniverse.isCaseClass(this)) {
        return cleanCopy(constructorParamsAreVal)
      }

      //Inner class with no user parameters
      if (constrParamCount == 1) {
        var outerField = clazz.getFields.find(_.getName == "$outer")

        if(outerField.isEmpty) outerField = clazz.getDeclaredFields.find(_.getName == "$outer")

        if(outerField.isDefined){
          val outer = outerField.get
          outer.setAccessible(true)
          return cleanCopy(constructor.newInstance(outer.get(this)).asInstanceOf[this.type])
        }

        val c  = clazz.getMethod("getComponent").invoke(this).asInstanceOf[Component]
        val pt = constructor.getParameterTypes.apply(0)

        if(c.getClass.isAssignableFrom(pt)){
          val copy =  constructor.newInstance(c).asInstanceOf[this.type]
          if(copy.isInstanceOf[Bundle])
            copy.asInstanceOf[Bundle].hardtype = (HardType(constructor.newInstance(c).asInstanceOf[this.type]))
          return cleanCopy(copy)
        }
      }

      needCloneImpl()

    } catch {
      case npe: java.lang.reflect.InvocationTargetException if npe.getCause.isInstanceOf[java.lang.NullPointerException] =>
        needCloneImpl()
      case e: java.lang.Exception =>
        needCloneImpl()
    }

    def needCloneImpl(): this.type = {
      SpinalError(
        s"""
           |*** Spinal can't clone ${this.getClass} datatype
                                                     |*** You have two way to solve that :
                                                     |*** In place to declare a "class Bundle(args){}", create a "case class Bundle(args){}"
                                                     |*** Or override by your self the bundle clone function
                                                     |*** The error is """.stripMargin + this.getScalaLocationLong)
      null
    }
    null
  }


  def toIo(): this.type ={
    val subIo = this
    val topIo = cloneOf(subIo)//.setPartialName(h, "", true)
    topIo.copyDirectionOf(subIo)
    for((s,t) <- (subIo.flatten, topIo.flatten).zipped if s.isAnalog) t.setAsAnalog()
    topIo <> subIo
    topIo.asInstanceOf[this.type]
  }

  /** Generate this if condition is true */
  def genIf(cond: Boolean): this.type = if(cond) this else null

  private [core] def formalPast(delay : Int) : this.type = {
    val ret = cloneOf(this)
    for((to, from) <- (ret.flatten, this.flatten).zipped){
      to := from.formalPast(delay)
    }
    ret.asInstanceOf[this.type]
  }

}

trait DataWrapper extends Data{
  override def asBits: Bits = ???
  override def flatten: Seq[BaseType] = ???
  override def getBitsWidth: Int = ???
  override private[core] def isEquals(that: Any): Bool = ???
  override private[core] def autoConnect(that: Data): Unit = ???
  override def assignFromBits(bits: Bits): Unit = ???
  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = ???
  override def getZero: DataWrapper.this.type = ???
  override private[core] def isNotEquals(that: Any): Bool = ???
  override def flattenLocalName: Seq[String] = ???
  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = ???
  override def setAsReg(): DataWrapper.this.type = ???
  override def setAsComb(): DataWrapper.this.type = ???
}

