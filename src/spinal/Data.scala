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

/**
 * Created by PIC18F on 21.08.2014.
 */


object Data {


  def doPull[T <: Data](srcData: T, finalComponent: Component, useCache: Boolean = false, propagateName: Boolean = false): T = {
    val startComponent = srcData.component
    if (useCache) {
      val finalComponentCacheState = finalComponent.pulledDataCache.getOrElse(srcData, null)
      if (finalComponentCacheState != null) return finalComponentCacheState.asInstanceOf[srcData.type]
    }

    if (startComponent == finalComponent || (finalComponent == startComponent.parent && srcData.isIo)) {
      finalComponent.pulledDataCache.put(srcData, srcData)
      return srcData
    }

    val startComponentStack = startComponent.parents() :+ startComponent
    var componentPtr = finalComponent
    var nextData: srcData.type = null.asInstanceOf[srcData.type]
    var ret: srcData.type = null.asInstanceOf[srcData.type]

    //pull from final component to lower component (fall)
    while (!startComponentStack.contains(componentPtr)) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData.assignFrom(cacheState)
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
      if (nextData != null) nextData.assignFrom(from) else ret = from
      Component.pop(componentPtr)

      if (useCache) componentPtr.pulledDataCache.put(srcData, from)
      nextData = from
      componentPtr = componentPtr.parent
    }

    val lowerComponent = componentPtr
    var risePath = startComponentStack.takeRight(startComponentStack.size - 1 - componentPtr.parents().size)
    //pull from lower component to start component (rise)
    while (componentPtr != startComponent) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData.assignFrom(cacheState)
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
        if (nextData != null) nextData.assignFrom(from) else ret = from
        Component.pop(componentPtr)

        if (useCache) componentPtr.pulledDataCache.put(srcData, from)
        nextData = from
        componentPtr = fromComponent
      }
      risePath = risePath.tail
    }

    Component.push(nextData.component)
    nextData.assignFrom(srcData)
    Component.pop(nextData.component)

    ret
  }
}


trait Data extends ComponentLocated with Nameable with Assignable{
  var dir: IODirection = null
  var isIo = false

  def asInput: this.type = {
    dir = in
    this
  }

  def asOutput: this.type = {
    dir = out
    this
  }



  /*def :=(that :this.type) : Unit = {
    this assignFrom that
  }*/

  def isOutput: Boolean = dir == out && isIo
  def isInput: Boolean = dir == in && isIo
  def isDirectionLess: Boolean = dir == null || !isIo

  def flatten: ArrayBuffer[(String, BaseType)]

  def pull: this.type = Data.doPull(this, Component.current, false, false)

  def ##(right: Data): Bits = {
    this.toBits ## right.toBits
  }


  def toBits: Bits
  def getBitsWidth : Int

  def keep : this.type = {
    flatten.foreach(t => t._2.component.additionalNodesRoot += t._2)
    this
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
