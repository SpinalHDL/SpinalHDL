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



object Data {
  implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //implicit def autoCast[T <: Data](that: T#SSelf): T = that.asInstanceOf[T]
  //  implicit def autoCast[T2 <: Bool](that: Bool): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //  implicit def autoCast[T2 <: Bits](that: Bits): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //  implicit def autoCast[T2 <: UInt](that: UInt): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //  implicit def autoCast[T2 <: SInt](that: SInt): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //  implicit def autoCast[T2 <: Bundle](that: Bundle): T2#SSelf = that.asInstanceOf[T2#SSelf]
  //  implicit def autoCast[T2 <: Vec](that: Vec): T2#SSelf = that.asInstanceOf[T2#SSelf]

  def doPull[T <: Data](srcData: T, finalComponent: Component, useCache: Boolean = false, propagateName: Boolean = false): T = {
    val startComponent = srcData.component
    if (useCache) {
      val finalComponentCacheState = finalComponent.pulledDataCache.getOrElse(srcData, null)
      if (finalComponentCacheState != null) return finalComponentCacheState.asInstanceOf[srcData.type]
    }

    if (startComponent == finalComponent || (startComponent != null && finalComponent == startComponent.parent && srcData.isIo)) {
      finalComponent.pulledDataCache.put(srcData, srcData)
      return srcData
    }

    val startComponentStack = if (startComponent == null) List[Component](null) else startComponent.parents() :+ startComponent
    var componentPtr = finalComponent
    var nextData: srcData.type = null.asInstanceOf[srcData.type]
    var ret: srcData.type = null.asInstanceOf[srcData.type]

    //pull from final component to lower component (fall)
    while (!startComponentStack.contains(componentPtr)) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData := cacheState
          Component.pop(componentPtr)
          return ret
        }
      }
      Component.push(componentPtr)
      val from = srcData.clone()
      if (propagateName)
        from.setCompositeName(srcData)
      from.asInput
      from.isIo = true
      if (nextData != null) nextData := from else ret = from
      Component.pop(componentPtr)

      if (useCache) componentPtr.pulledDataCache.put(srcData, from)
      nextData = from
      componentPtr = componentPtr.parent
    }

    val lowerComponent = componentPtr
    var risePath = if (lowerComponent == null) List[Component]() else startComponentStack.takeRight(startComponentStack.size - 1 - componentPtr.parents().size)
    //pull from lower component to start component (rise)
    while (!risePath.isEmpty) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData.:=(cacheState)
          Component.pop(componentPtr)
          return ret
        }
      }
      val fromComponent = risePath.head
      if (fromComponent != startComponent || srcData.isIo == false) {
        Component.push(fromComponent)
        val from = srcData.clone()
        if (propagateName)
          from.setCompositeName(srcData)
        from.isIo = true
        from.asOutput
        Component.pop(fromComponent)
        Component.push(componentPtr)
        if (nextData != null) nextData := from else ret = from
        Component.pop(componentPtr)

        if (useCache) componentPtr.pulledDataCache.put(srcData, from)
        nextData = from
        componentPtr = fromComponent
      }
      risePath = risePath.tail
    }

    Component.push(nextData.component)
    nextData := srcData
    Component.pop(nextData.component)

    ret
  }
}

trait Data extends ContextUser with Nameable with Assignable with AttributeReady with SpinalTagReady with GlobalDataUser with ScalaLocated {
  type SSelf <: Data

  var dir: IODirection = null
  var isIo = false

  def asInput: this.type = {
    dir = in;
    this
  }
  def asOutput: this.type = {
    dir = out;
    this
  }

  def isOutputDir: Boolean = dir == out
  def isInputDir: Boolean = dir == in

  def isOutput: Boolean = dir == out && isIo
  def isInput: Boolean = dir == in && isIo
  def isDirectionLess: Boolean = dir == null || !isIo
  def flip: this.type = {
    for ((n, e) <- flatten) {
      e.dir match {
        case `in` => e.dir = out
        case `out` => e.dir = in
        case _ => SpinalError(s"Can't flip a data that is direction less $e")
      }
    }
    this
  }


  val parentData = globalData.dataStack.head()

  def getZero: this.type = {
    val ret = clone()
    ret.flatten.foreach(t => {
      t._2 := t._2.getZero
    })

    ret
  }

  def flatten: ArrayBuffer[(String, BaseType)]

  def pull: this.type = Data.doPull(this, Component.current, false, false)

  //Use as \= to have the same behavioral than VHDL variable
  def \(that: SSelf) = {
    val ret = that.clone()
    ret := this
    ret.whenScope = this.whenScope
    ret := that
    ret
  }

  def :=(that: SSelf): Unit = this assignFrom(that, false)
  def <>(that: SSelf): Unit = this autoConnect that
  def ===(that: SSelf): Bool = isEguals(that)
  def !==(that: SSelf): Bool = !isEguals(that)
  //  def :-(that: => SSelf): this.type = {
  //    val task = () => {
  //      this := that
  //    }
  //    component.postCreationTask += task
  //
  //    this
  //  }

  def ##(right: Data): Bits = this.toBits ## right.toBits

  def toBits: Bits
  def assignFromBits(bits: Bits): Unit



  def isEguals(that: Data): Bool = (this.flatten, that.flatten).zipped.map((a, b) => a._2.isEguals(b._2)).reduceLeft(_ || _)
  def autoConnect(that: Data): Unit = (this.flatten, that.flatten).zipped.foreach(_._2 autoConnect _._2)


  def getBitsWidth: Int

  def keep: this.type = {
    flatten.foreach(t => t._2.component.additionalNodesRoot += t._2);
    dontSimplifyIt
    this
  }

  def dontSimplifyIt: this.type = {
    flatten.foreach(_._2.dontSimplifyIt)
    this
  }
  def allowSimplifyIt: this.type = {
    flatten.foreach(_._2.allowSimplifyIt)
    this
  }

  override def add(attribute: Attribute): Unit = {
    flatten.foreach(_._2.add(attribute))
  }

  def isReg: Boolean = flatten.foldLeft(true)(_ && _._2.isReg)

  /*private[core] */
  def init(init: SSelf): this.type = {
    // if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for (((eName, e), (y, initElement)) <- (this.flatten, regInit.flatten).zipped) {
      def recursiveSearch(ptr: Node): Unit = {
        if (ptr.component != init.component) SpinalError(s"Try to set initial value of a data that is not in current component ($this)")
        ptr match {
          case bt: BaseType => {
            if (bt.isReg)
              bt.inputs(0).asInstanceOf[Reg].setInitialValue(initElement)
            else
              recursiveSearch(bt)
          }
          case _ => SpinalError(s"Try to set initial value of a data that is not a register ($this)")
        }
      }

      //maybe need to restor commented ?
      if (initElement.inputs(0) != null /* && initElement.inputs(0).inputs(0) != null*/ ) {
        recursiveSearch(e)
      }
    }
    this
  }

  /*private[core] */
  def next(next: SSelf): this.type = {
    if (!isReg) SpinalError(s"Try to set next value of a data that is not a register ($this)")
    this := next
    this
  }


  def randBoot(): this.type = {
    flatten.foreach(_._2.addTag(spinal.core.randomBoot))
    this
  }


  override def clone(): this.type = {
    try {
      val clazz = this.getClass
      val constructor = clazz.getConstructors.head
      val constrParamCount = constructor.getParameterTypes.size
      //No param =>
      if(constrParamCount == 0) return constructor.newInstance().asInstanceOf[this.type]

      def constructorParamsAreVal: this.type ={
        val outer = clazz.getFields.find(_.getName == "$outer")
        val constructor = clazz.getDeclaredConstructors.head
        val argumentCount = constructor.getParameterTypes.size - (if (outer.isDefined) 1 else 0)
        val fields = clazz.getDeclaredFields
        val arguments = (0 until argumentCount) map { i =>
          val fieldName = fields(i).getName
          val getter = clazz.getMethod(fieldName)
          getter.invoke(this)
        }
        if (outer.isEmpty)
          return constructor.newInstance(arguments: _*).asInstanceOf[this.type]
        else {
          val args = (outer.get.get(this) :: Nil) ++ arguments
          return constructor.newInstance(args : _*).asInstanceOf[this.type]
        }
      }
      //Case class =>
      if (ScalaUniverse.isCaseClass(this)) {
        return constructorParamsAreVal
      }

      //Inner class with no user parameters
      if(constrParamCount == 1) {
        val outer = clazz.getFields.find(_.getName == "$outer")
        if(outer.isDefined) {
          return constructor.newInstance(outer.get.get(this)).asInstanceOf[this.type]
        }
      }

      if(clazz.getAnnotations.find(_.isInstanceOf[valParams]).isDefined){
        return constructorParamsAreVal
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
        """
          |*** Spinal can't clone one of your datatype
          |*** You have two way to solve that :
          |*** In place to declare a "class Bundle(args){}", create a "case class Bundle(args){}"
          |*** Or override by your self the bundle clone function
          |*** The error is """.stripMargin + this.getScalaLocationString);
      null
    }
    null
  }

}
