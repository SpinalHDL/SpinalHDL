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

/**
  * Created by PIC18F on 08.01.2015.
  */

object Bundle {

}

class Bundle extends MultiData with Nameable with OverridedEqualsHashCode {
  var cloneFunc: () => Object = null


  override def clone: this.type = {
    if (cloneFunc != null) {
      val ret = cloneFunc().asInstanceOf[this.type].asDirectionLess
      ret.cloneFunc = cloneFunc
      return ret
    }
    super.clone
  }

  def assignAllByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null) SpinalError("Bundle assignement is not complete at " + ScalaLocated.long)
      element match {
        case b: Bundle => b.assignAllByName(other.asInstanceOf[Bundle])
        case _ => element := other
      }
    }
  }

  def assignSomeByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other != null) {
        element match {
          case b: Bundle => b.assignSomeByName(other.asInstanceOf[Bundle])
          case _ => element := other
        }
      }
    }
  }

  private[core] override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    assert(!conservative)
    that match {
      case that: Bundle => {
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("Bundles must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        for ((name, element) <- elements) {
          val other = that.find(name)
          if (other == null) SpinalError("Bundle assignment is not complete at " + ScalaLocated.long)
          element := other
        }
      }
      case _ => throw new Exception("Undefined assignment")
    }
  }

  private var elementsCache: ArrayBuffer[(String, Data)] = null


  def elements = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      Misc.reflect(this, (name, obj) => {
        obj match {
          case data: Data => {
            if (!rejectOlder || this.isOlderThan(data)) {
              //To avoid bundle argument
              elementsCache += Tuple2(name, data)
              Ownable.set(data,this)
              data.parent = this
            }
          }
          case _ =>
        }
      })
      elementsCache = elementsCache.sortWith(_._2.instanceCounter < _._2.instanceCounter)
    }
    elementsCache
  }

  private[core] def rejectOlder = true


  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}"
}

class BundleCase extends Bundle {
  private[core] override def rejectOlder = false
}
