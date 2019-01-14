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


/**
  * The Bundle is a composite type that defines a group of named signals (of any SpinalHDL basic type) under a single name.
  * The Bundle can be used to model data structures, buses and interfaces.
  *
  * @example {{{
  *     val cmd = new Bundle{
  *       val init   = in Bool
  *       val start  = in Bool
  *       val result = out Bits(32 bits)
  *     }
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Bundle Bundle Documentation]]
  */
class Bundle extends MultiData with Nameable with OverridedEqualsHashCode {

  var hardtype: HardType[_] = null

  globalData.currentComponent match {
    case null =>
    case component =>
      component.addPrePopTask(() => {
        elements.foreach { case (n, e) =>
          OwnableRef.proposal(e, this)
          e.setPartialName(n, weak = true)
        }
      })
  }

  override def clone: Bundle = {
    if (hardtype != null) {
      val ret = hardtype().asInstanceOf[this.type]
      ret.hardtype = hardtype
      return ret
    }
    super.clone.asInstanceOf[Bundle]
  }

  /** Assign the bundle with an other bundle by name */
  def assignAllByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null)
        PendingError(s"Bundle assignment is not complete. Missing $name\n " + ScalaLocated.long)
      else element match {
        case b: Bundle => b.assignAllByName(other.asInstanceOf[Bundle])
        case _         => element := other
      }
    }
  }

  /** Assign all possible signal fo the bundle with an other bundle by name */
  def assignSomeByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other != null) {
        element match {
          case b: Bundle => b.assignSomeByName(other.asInstanceOf[Bundle])
          case _         => element := other
        }
      }
    }
  }

  private[core] override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case that: Bundle =>
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("Bundles must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        for ((name, element) <- elements) {
          val other = that.find(name)
          if (other == null) {
            val trace = ScalaLocated.long
            PendingError(s"Bundle assignment is not complete. $this need '$name' but $that doesn't provide it.\n$trace ")
          }
          else
            element.compositAssignFrom(other,element,kind)
        }
      case _ => throw new Exception("Undefined assignment")
    }
  }

  private var elementsCache: ArrayBuffer[(String, Data)] = null

  /** Return all element of the bundle */
  def elements: ArrayBuffer[(String, Data)] = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      Misc.reflect(this, (name, obj) => {
        obj match {
          case data: Data =>
            if (!rejectOlder || this.isOlderThan(data)) {
              //To avoid bundle argument
              elementsCache += (name -> data)
              data.parent = this
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

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : $getTypeString"
}

class BundleCase extends Bundle {
  private[core] override def rejectOlder = false
}
