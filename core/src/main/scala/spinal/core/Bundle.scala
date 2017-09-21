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
  if(component != null) component.addPrePopTask(() => {
    elements.foreach{case (n,e) => {
      OwnableRef.proposal(e,this)
      e.setPartialName(n,true)
    }}
  })
  override def clone: Bundle = {
    if (cloneFunc != null) {
      val ret = cloneFunc().asInstanceOf[this.type].asDirectionLess
      ret.cloneFunc = cloneFunc
      return ret
    }
    super.clone.asInstanceOf[Bundle]
  }

  def assignAllByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null)
        PendingError(s"Bundle assignement is not complete. Missing $name\n " + ScalaLocated.long)
      else element match {
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

  private[core] override def assignFromImpl(that: AnyRef, target : AnyRef, kind : AnyRef): Unit = {
    that match {
      case that: Bundle => {
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("Bundles must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        for ((name, element) <- elements) {
          val other = that.find(name)
          if (other == null) {
            val trace = ScalaLocated.long
            PendingError(s"Bundle assignement is not complete. $this need '$name' but $that doesn't provide it.\n$trace ")
          }
          else
            element.compositAssignFrom(other,element,kind)
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

  def getTypeString = getClass.getSimpleName
  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getTypeString}"
}

class BundleCase extends Bundle {
  private[core] override def rejectOlder = false
}
