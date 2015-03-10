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

package spinal


import scala.collection.mutable.ArrayBuffer




object Data {
  implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
 // implicit def autoCast[T <: Data](that: Null): T = that.asInstanceOf[T]


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

    val startComponentStack = if(startComponent == null) List[Component](null) else startComponent.parents() :+ startComponent
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
    var risePath = if(lowerComponent == null)  List[Component]() else  startComponentStack.takeRight(startComponentStack.size - 1 - componentPtr.parents().size)
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

trait Data extends ContextUser with Nameable with Assignable with AttributeReady with SpinalTagReady with GlobalDataUser {
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
  def flip : this.type = {
    for((n,e) <- flatten){
      e.dir match {
        case `in` => e.dir = out
        case `out` => e.dir = in
        case _ => SpinalError(s"Can't flip a data that is direction less $e")
      }
    }
    this
  }

  def getZero : this.type = {
    val ret = clone()
    ret.flatten.foreach(t => {
      t._2 := t._2.getZero
    })

    ret
  }

  def flatten: ArrayBuffer[(String, BaseType)]

  def pull: this.type = Data.doPull(this, Component.current, false, false)


  def :=(that: SSelf): Unit = this assignFrom (that,false)
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
  def fromBits(bits: Bits): Unit

  def isEguals(that: Data): Bool = (this.flatten, that.flatten).zipped.map((a, b) => a._2.isEguals(b._2)).reduceLeft(_ || _)
  def autoConnect(that: Data): Unit = (this.flatten, that.flatten).zipped.foreach(_._2 autoConnect _._2)


  def getBitsWidth: Int

  def keep: this.type = {
    flatten.foreach(t => t._2.component.additionalNodesRoot += t._2);
    dontSimplifyIt
    this
  }

  def dontSimplifyIt : this.type = {
    flatten.foreach(_._2.dontSimplifyIt)
    this
  }
  def allowSimplifyIt : this.type = {
    flatten.foreach(_._2.allowSimplifyIt)
    this
  }

  override def add(attribute: Attribute): Unit = {
    flatten.foreach(_._2.add(attribute))
  }

  def setRegInit(init: Data): Unit = {
    if (!flatten.foldLeft(true)(_ && _._2.isReg)) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for (((eName, e), (y, initElement)) <- (this.flatten, regInit.flatten).zipped) {
      if (initElement.inputs(0) != null && initElement.inputs(0).inputs(0) != null) {
        e.inputs(0).asInstanceOf[Reg].setInitialValue(initElement)
      }
    }
  }


  override def clone(): this.type = {
    try {
      val constructor = this.getClass.getConstructors.head
      constructor.getParameterTypes.size match {
        case 0 => return constructor.newInstance().asInstanceOf[this.type]
        case 1 => {
          val paramtype = constructor.getParameterTypes.head
          if (classOf[Bundle].isAssignableFrom(paramtype) || classOf[Component].isAssignableFrom(paramtype)) {
            return constructor.newInstance(null).asInstanceOf[this.type]
          }
          needCloneImpl()
        }
        case _ => needCloneImpl()
      }
    } catch {
      case npe: java.lang.reflect.InvocationTargetException if npe.getCause.isInstanceOf[java.lang.NullPointerException] =>
        needCloneImpl()
      case e: java.lang.Exception =>
        needCloneImpl()
    }

    def needCloneImpl(): this.type = {
      throw new Exception(s"Can't clone this data, you must implit it by yourself");
      null
    }
    null
  }

}
