/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import scala.collection.mutable.ArrayBuffer
//import scala.reflect.runtime.{universe => ru}


object Data {

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
    var commonComponent ={
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
        while (srcPtr.level < dstPtr.level) {
          risePath += dstPtr
          dstPtr = dstPtr.parent
        }
        while (srcPtr != dstPtr) {
          srcPtr = srcPtr.parent
          risePath += dstPtr
          dstPtr = dstPtr.parent
        }
        srcPtr
      }
    }


    def push(c : Component, scope : ScopeStatement) : Unit = {
      c.globalData.context.push(DslContext(c.clockDomain, c, scope))
    }

    def pop(c : Component) : Unit = {
      assert(c.globalData.context.head.component == c)
      c.globalData.context.pop()
    }

    var currentData : T = srcData
    var currentComponent : Component = srcData.component

    //Fall path
    while(currentComponent != commonComponent){
      if(useCache && currentComponent.parent.pulledDataCache.contains(srcData)){
        currentComponent = currentComponent.parent
        currentData = currentComponent.parent.pulledDataCache(srcData).asInstanceOf[T]
      } else {
        if (currentData.component == currentComponent && currentData.isIo) {
          //nothing to do
        } else {
          push(currentComponent, currentComponent.dslBody)
          val copy = cloneOf(srcData).asOutput()
          if (propagateName)
            copy.setCompositeName(srcData,weak=true)
          copy := currentData
          pop(currentComponent)
          currentData = copy
        }
        currentComponent = currentComponent.parent
        if (useCache)
          currentComponent.pulledDataCache.put(srcData, currentData)
      }
    }

    //Rise path
    for(riseTo <- risePath.reverseIterator){
      if(useCache && riseTo.pulledDataCache.contains(srcData)){
        currentComponent = riseTo
        currentData = riseTo.pulledDataCache(srcData).asInstanceOf[T]
      }else {
        push(riseTo, riseTo.dslBody)
        val copy = cloneOf(srcData).asInput()
        if (propagateName)
          copy.setCompositeName(srcData,weak=true)
        pop(riseTo)
        if (currentComponent != null) {
          push(currentComponent, currentComponent.dslBody)
          copy := currentData
          pop(currentComponent)
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

trait DataPrimitives[T <: Data]{
  private[spinal] def _data : T

  def ===(that: T): Bool = _data.isEquals(that)
  def =/=(that: T): Bool = _data.isNotEquals(that)



  def := (that: T): Unit = {
    _data assignFrom(that)
  }


//    def := [T2 <: T](that: T2): Unit = pimpIt assignFrom(that)

  //Use as \= to have the same behavioral than VHDL variable
  def \(that: T) : T = {
    val globalData = GlobalData.get
    globalData.context.push(DslContext(null, _data.component, _data.parentScope))
    val lastStatement = _data.parentScope.last
    val manageLastStatement = lastStatement.isInstanceOf[TreeStatement]
    if(manageLastStatement) lastStatement.removeStatementFromScope()
    val ret = cloneOf(that)
    ret := _data
    if(manageLastStatement) _data.parentScope.append(lastStatement)
    globalData.context.pop()
    ret.allowOverride
    ret := that
    ret
  }

  def <>(that: T): Unit = _data autoConnect that
  def init(that: T): T = {
    _data.initFrom(that)
    _data
  }
  def default(that : => T) : T ={
    val c = if(_data.dir == in)
      Component.current.parent
    else
      Component.current

    if(c != null) {
      Component.push(c)
      _data.defaultImpl(that)
      Component.pop(c)
    }
    _data
  }

}



//Should not extends AnyVal, Because it create kind of strange call stack move that make error reporting miss accurate
class DataPimper[T <: Data](val _data: T) extends DataPrimitives[T]{

}

object DataAssign
object InitAssign

trait Data extends ContextUser with NameableByComponent with Assignable with SpinalTagReady with GlobalDataUser with ScalaLocated with OwnableRef {
  private[core] var dir: IODirection = null
  private[core] def isIo = dir != null

  var parent : Data = null
  def getRootParent : Data = if(parent == null) this else parent.getRootParent

  def asInput(): this.type = {
    if(this.component != Component.current) {
      val location = ScalaLocated.long
      PendingError(s"You should not set $this as input outside it's own component.\n$location" )
    }else {
      dir = in
    }
    this
  }
  def asOutput(): this.type = {
    if(this.component != Component.current) {
      val location = ScalaLocated.long
      PendingError(s"You should not set $this as output outside it's own component.\n$location" )
    }else {
      dir = out
    }
    this
  }

  def asDirectionLess() : this.type = {
    dir = null
    this
  }

 // def assignDontCare() : Unit = ???

  def isOutput: Boolean = dir == out
  def isInput: Boolean = dir == in
  def isDirectionLess: Boolean = dir == null
  def flip(): this.type  = {
    dir match {
      case `in` => dir = out
      case `out` => dir = in
      case _ => SpinalError(s"Can't flip a data that is direction less $this")
    }
    this
  }


  final def assignFrom(that : AnyRef, target : AnyRef = this) = compositAssignFrom(that,target,DataAssign)
  final def initFrom(that : AnyRef, target : AnyRef = this) = (that, target) match {
    case (init : Data, target : Data) if ! target.isReg => {
      for ((e, initElement) <- (target.flatten, init.flatten).zipped) {
        def recursiveSearch(bt: BaseType): Unit = {
          if (bt.isReg)
            bt.init (initElement)
          else if(bt.hasOnlyOneStatement && Statement.isFullToFullStatement(bt.head))
            recursiveSearch(bt.head.source.asInstanceOf[BaseType])
          else
            LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
        }
        recursiveSearch(e)
      }
    }
    case _ => compositAssignFrom(that,target,InitAssign)
  }

  def asData = this.asInstanceOf[Data]
  def getZero: this.type

  def flatten: Seq[BaseType]
  def flattenLocalName: Seq[String]

  def pull(): this.type = Data.doPull(this, Component.current, false, false)

  //  def :-(that: => SSelf): this.type = {
  //    val task = () => {
  //      this := that
  //    }
  //    component.postCreationTask += task
  //
  //    this
  //  }

  def ##(right: Data): Bits = this.asBits ## right.asBits

  def asBits: Bits
  def assignFromBits(bits: Bits): Unit
  def assignFromBits(bits: Bits,hi : Int,low : Int): Unit
  def assignFromBits(bits: Bits,offset: Int, bitCount: BitCount): Unit = this.assignFromBits(bits,offset + bitCount.value -1,offset)
  def assignDontCare() : this.type = {
    flatten.foreach(_.assignDontCare())
    this
  }

  private[core] def isEquals(that: Any): Bool
  private[core] def isNotEquals(that: Any): Bool

  def resized : this.type ={
    val ret = cloneOf(this)
    ret.assignFrom(this)
    ret.addTag(tagAutoResize)
    return ret.asInstanceOf[this.type]
  }
  def allowOverride : this.type ={
    addTag(allowAssignmentOverride)
  }


  private[core] def autoConnect(that: Data): Unit// = (this.flatten, that.flatten).zipped.foreach(_ autoConnect _)

  private[core] def autoConnectBaseImpl(that: Data): Unit = {

    def error(message : String) = {
      val locationString = ScalaLocated.long
      globalData.pendingErrors += (() => (message + "\n" + this + "\n" + that + "\n" + locationString))
    }
    def getTrueIoBaseType(that : Data) : Data = that.getRealSource.asInstanceOf[Data]


    val thisTrue = getTrueIoBaseType(this)
    val thatTrue = getTrueIoBaseType(that)

    if (thisTrue.component == thatTrue.component) {
      if (thisTrue.component == Component.current) {
        sameFromInside
      } else if (thisTrue.component.parent == Component.current) {
        sameFromOutside
      } else error("You cant autoconnect from here")
    } else if (thisTrue.component.parent == thatTrue.component.parent) {
      childAndChild
    } else if (thisTrue.component == thatTrue.component.parent) {
      parentAndChild(this, that)
    } else if (thisTrue.component.parent == thatTrue.component) {
      parentAndChild(that, this)
    } else error("Don't know how autoconnect")



    def sameFromOutside: Unit = {
      if (thisTrue.isOutput && thatTrue.isInput) {
        that := this
      } else if (thisTrue.isInput && thatTrue.isOutput) {
        this := that
      } else error("Bad input output specification for autoconnect")
    }
    def sameFromInside: Unit = {
      (thisTrue.dir,thatTrue.dir) match {
        case (`out`,`in`) => this := that
        case (`out`,null) => this := that
        case (null,`in`) => this := that
        case (`in`,`out`) => that := this
        case (`in`,null) => that := this
        case (null,`out`) => that := this
        case _ =>  error("Bad input output specification for autoconnect")
      }
    }

    def childAndChild: Unit = {
      if (thisTrue.isOutput && thatTrue.isInput) {
        that := this
      } else if (thisTrue.isInput && thatTrue.isOutput) {
        this := that
      } else error("Bad input output specification for autoconnect")
    }

    def parentAndChild(p: Data, k: Data): Unit = {
      if (getTrueIoBaseType(k).isOutput) {
        p := k
      } else if (getTrueIoBaseType(k).isInput) {
        k := p
      } else error("Bad input output specification for autoconnect")
    }
  }

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

  def isReg: Boolean = flatten.foldLeft(true)(_ && _.isReg)
  def isComb: Boolean = flatten.foldLeft(true)(_ && _.isComb)

  override def getRealSourceNoRec: Any = this

  /*private[core] */
//  private[core] def initImpl(init: Data): this.type = {
//    for ((e, initElement) <- (this.flatten, init.flatten).zipped) {
//      def recursiveSearch(ptr: Node): Unit = {
//        //if (ptr.component != init.component) SpinalError(s"Try to set initial value of a data that is not in current component ($this)")
//        ptr match {
//          case bt: BaseType => {
//            if (bt.isReg)
//              bt.input.asInstanceOf[Reg].setInitialValue(initElement)
//            else
//              recursiveSearch(bt.input)
//          }
//          case _ => LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
//        }
//      }
//
//      //maybe need to restor commented ?
//      //if (initElement.getInput(0) != null /* && initElement.getInput(0).getInput(0) != null*/ ) {
//      recursiveSearch(e)
//     // }
//    }
//    this
//  }



  private[core] def defaultImpl(init: Data): this.type = {
    val regInit = clone()
    regInit := init
    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      e.addTag(new DefaultTag(initElement))
    }
    this
  }

  /*private[core] */
  //  def next(next: SSelf): this.type = {
  //    if (!isReg) SpinalError(s"Try to set next value of a data that is not a register ($this)")
  //    this := next
  //    this
  //  }

  //Usefull for register that doesn't need a reset value in RTL, but need a randome value for simulation (avoid x-propagation)
  def randBoot(): this.type = {
    flatten.foreach(_.addTag(spinal.core.randomBoot))
    this
  }

//  @deprecated("use allowPruning instead")
//  def unused = allowPruning()

  def allowPruning() = {
    flatten.foreach(_.addTag(unusedTag))
  }


  override def getComponent(): Component = component

  override def clone: Data = {
    try {
      val clazz = this.getClass
      val constructor = clazz.getConstructors.head
      val constrParamCount = constructor.getParameterTypes.size
      //No param =>
      if (constrParamCount == 0) return constructor.newInstance().asInstanceOf[this.type]

      def cleanCopy[T <: Data](that : T) : T = {
        that.asDirectionLess()
//        that.flatten.foreach(_.input = null)
        that
      }

      def constructorParamsAreVal: this.type = {
        val outer = clazz.getFields.find(_.getName == "$outer")
        val constructor = clazz.getDeclaredConstructors.head
        val argumentCount = constructor.getParameterTypes.size - (if (outer.isDefined) 1 else 0)
        val fields = clazz.getDeclaredFields
        val arguments = (0 until argumentCount) map { i =>
          val fieldName = fields(i).getName
          val getter = clazz.getMethod(fieldName)
          val arg = getter.invoke(this)
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
        if(!outerField.isDefined) outerField = clazz.getDeclaredFields.find(_.getName == "$outer")
        if(outerField.isDefined){
          val outer = outerField.get
          outer.setAccessible(true)
          return cleanCopy(constructor.newInstance(outer.get(this)).asInstanceOf[this.type])
        }
        val c = clazz.getMethod("getComponent").invoke(this).asInstanceOf[Component]
        val pt = constructor.getParameterTypes.apply(0)
        if(c.getClass.isAssignableFrom(pt)){
          val copy =  constructor.newInstance(c).asInstanceOf[this.type]
//          if(copy.isInstanceOf[Bundle]) //TODO IR !!
//            copy.asInstanceOf[Bundle].cloneFunc = (() => constructor.newInstance(c).asInstanceOf[this.type])
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
                                                     |*** The error is """.stripMargin + this.getScalaLocationLong);
      null
    }
    null
  }
  def getComponents() : Seq[Component] = if(component == null) Nil else (component.parents() ++ Seq(component))

  def genIf(cond : Boolean) : this.type = if(cond) this else null

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
}

//
//abstract class CustomData extends Data{
//  override def getBitsWidth: Int = flatten.map(_.getBitsWidth).reduce(_ + _)
//  override def assignFromBits(bits: Bits): Unit = {
//    var offset = 0
//    for(e <- flatten){
//      e.assignFromBits(bits(e.getBitsWidth + offset-1,offset))
//      offset += e.getBitsWidth
//    }
//  }
//  override def toBits: Bits = Cat(flatten.reverse.map(_.toBits))
//  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
//    assert(!conservative)
//    assert(that.getClass == this.getClass)
//    val t = that.asInstanceOf[CustomData]
//    (this.flatten,t.flatten).zipped.map(_.assignFromImpl(_,conservative))
//  }
//}
