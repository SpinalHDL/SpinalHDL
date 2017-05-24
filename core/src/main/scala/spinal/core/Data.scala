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


    //val targetPath = finalComponent.parents()
    var finalData : T = null.asInstanceOf[T]
    val srcPath = null +: srcData.getComponents()
    var currentData : T = null.asInstanceOf[T]
    var currentComponent = finalComponent

    //Rise path
    if(!srcPath.contains(currentComponent)){
      Component.push(finalComponent)
      finalData = cloneOf(srcData)
      if (propagateName)
        finalData.setCompositeName(srcData,true)
      finalData.asInput()
      if (useCache) currentComponent.pulledDataCache.put(srcData, finalData)
      Component.pop(finalComponent)

      currentComponent = currentComponent.parent
      currentData = finalData

      while(!srcPath.contains(currentComponent)){
        if (useCache && currentComponent.pulledDataCache.contains(srcData)) {
          val cachedNode = currentComponent.pulledDataCache.get(srcData).get
          Component.push(currentComponent)
          currentData.assignFrom(cachedNode,false)
          Component.pop(currentComponent)
          return finalData
        }else{
          Component.push(currentComponent)
          val copy = cloneOf(srcData)
          if (propagateName)
            copy.setCompositeName(srcData,true)
          copy.asInput()
          currentData.assignFrom(copy,false)
          Component.pop(currentComponent)
          if (useCache) currentComponent.pulledDataCache.put(srcData, copy)

          currentData = copy
          currentComponent = currentComponent.parent
        }
      }
    }

    //fall path
    var fallPath = srcPath.drop(srcPath.indexOf(currentComponent) + 1)
    if(!fallPath.isEmpty && srcData.isOutput) fallPath = fallPath.dropRight(1)
    while(!fallPath.isEmpty){
      if (useCache && currentComponent.pulledDataCache.contains(srcData)) {
        val cachedNode = currentComponent.pulledDataCache.get(srcData).get
        Component.push(currentComponent)
        currentData.assignFrom(cachedNode,false)
        Component.pop(currentComponent)
        return finalData
      }else{
        Component.push(fallPath.head)
        val copy = cloneOf(srcData)
        if (propagateName)
          copy.setCompositeName(srcData,true)
        copy.asOutput()
        Component.pop(fallPath.head)

        if(currentData != null) {
          Component.push(currentComponent)
          currentData.assignFrom(copy, false)
          Component.pop(currentComponent)
        }else{
          finalData = copy
        }
        if (useCache) currentComponent.pulledDataCache.put(srcData, copy)

        currentData = copy
      }

      currentComponent = fallPath.head
      fallPath = fallPath.tail
    }

    srcData.dir match{
      case `in`=> {
        Component.push(srcData.component)
        currentData := srcData
        Component.pop(srcData.component)
      }
      case _ => {
        Component.push(currentData.component)
        currentData := srcData
        Component.pop(currentData.component)
      }
    }

    finalData
  }
}

trait DataPrimitives[T <: Data]{
  private[spinal] def _data : T

  def ===(that: T): Bool = _data.isEquals(that)
  def =/=(that: T): Bool = _data.isNotEquals(that)
  @deprecated("Use =/= instead")
  def !==(that: T): Bool = this =/= that



  def := (that: T): Unit = {
    _data assignFrom(that, false)
  }


  //  def := [T2 <: T](that: T2): Unit = pimpIt assignFrom(that, false)

  //Use as \= to have the same behavioral than VHDL variable
  def \(that: T) : T = {
    val ret = cloneOf(that)
    ret := _data
    ret.flatten.foreach(_.conditionalAssignScope = _data.conditionalAssignScope)
    ret.globalData.overridingAssignementWarnings = false
    ret := that
    ret.globalData.overridingAssignementWarnings = true
    ret
  }

  def <>(that: T): Unit = _data autoConnect that
  def init(that: T): T = _data.initImpl(that)
  def default(that : => T) : T ={
    val c = if(_data.dir == in)
      Component.current.parent
    else
      Component.current

    Component.push(c)
    _data.defaultImpl(that)
    Component.pop(c)
    _data
  }

  def muxList[T <: Data](mappings: Seq[(Any, T)]): T = {
    SpinalMap.list(_data,mappings)
  }

  def muxList[T <: Data](defaultValue: T, mappings: Seq[(Any, T)]): T = {
    SpinalMap.list(_data, mappings :+ (spinal.core.default , defaultValue) )
  }

  def mux[T <: Data](mappings: (Any, T)*): T = {
    SpinalMap.list(_data,mappings)
  }

}



//Should not extends AnyVal, Because it create kind of strange call stack move that make error reporting miss accurate
class DataPimper[T <: Data](val _data: T) extends DataPrimitives[T]{

}


trait Data extends ContextUser with NameableByComponent with Assignable  with SpinalTagReady with GlobalDataUser with ScalaLocated with OwnableRef {
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
    ret.assignFrom(this,false)
    ret.addTag(tagAutoResize)
    return ret.asInstanceOf[this.type]
  }

  private[core] def autoConnect(that: Data): Unit// = (this.flatten, that.flatten).zipped.foreach(_ autoConnect _)

  private[core] def autoConnectBaseImpl(that: Data): Unit = {

    def error(message : String) = {
      val locationString = ScalaLocated.long
      globalData.pendingErrors += (() => (message + "\n" + this + "\n" + that + "\n" + locationString))
    }
    def getTrueIoBaseType(that : Data) : Data = that match {
      case that : BaseType => that.input match{
          case input : Extract => getTrueIoBaseType(input.getBitVector.asInstanceOf[BaseType])
          case _ => that
      }
      case _ => that
    }


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
    flatten.foreach(t => t.component.additionalNodesRoot += t);
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


  /*private[core] */
  private[core] def initImpl(init: Data): this.type = {
    for ((e, initElement) <- (this.flatten, init.flatten).zipped) {
      def recursiveSearch(ptr: Node): Unit = {
        //if (ptr.component != init.component) SpinalError(s"Try to set initial value of a data that is not in current component ($this)")
        ptr match {
          case bt: BaseType => {
            if (bt.isReg)
              bt.input.asInstanceOf[Reg].setInitialValue(initElement)
            else
              recursiveSearch(bt.input)
          }
          case _ => LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
        }
      }

      //maybe need to restor commented ?
      //if (initElement.getInput(0) != null /* && initElement.getInput(0).getInput(0) != null*/ ) {
      recursiveSearch(e)
     // }
    }
    this
  }

//  private[core] def initImpl(init: Data): this.type = {
//    if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
//    for ((e, initElement) <- (this.flatten, init.flatten).zipped) {
//      e match {
//        case bt: BaseType => bt.getInput(0).asInstanceOf[Reg].setInitialValue(initElement)
//        case _ => SpinalError(s"???")
//      }
//    }
//    this
//  }

  private[core] def defaultImpl(init: Data): this.type = {
    // if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      e.defaultValue = initElement
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

  @deprecated("use allowPruning instead")
  def unused = allowPruning()

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
        that.flatten.foreach(_.input = null)
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
          if(copy.isInstanceOf[Bundle])
            copy.asInstanceOf[Bundle].cloneFunc = (() => constructor.newInstance(c).asInstanceOf[this.type])
          return cleanCopy(copy)
        }
        //        val a = c.areaClassSet.get(pt)
        //        if(a.isDefined && a.get != null){
        //          return constructor.newInstance(a.get).asInstanceOf[this.type]
        //        }
      }


      //      if (clazz.getAnnotations.find(_.isInstanceOf[valClone]).isDefined) {
      //        return constructorParamsAreVal
      //      }

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
  override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = ???
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