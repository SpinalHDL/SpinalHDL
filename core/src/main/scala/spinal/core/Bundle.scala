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
import spinal.idslplugin.{Location, ValCallback}

import scala.collection.mutable


/**
  * The Bundle is a composite type that defines a group of named signals (of any SpinalHDL basic type) under a single name.
  * The Bundle can be used to model data structures, buses and interfaces.
  *
  * @example {{{
  *     val cmd = new Bundle{
  *       val init   = in Bool()
  *       val start  = in Bool()
  *       val result = out Bits(32 bits)
  *     }
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Bundle Bundle Documentation]]
  */



trait ValCallbackRec extends ValCallback{

//  final override def valCallback(fieldRef: Any, name: String): Unit = {
//    val refs = mutable.Set[Any]()
//    valCallbackOn(fieldRef,name, refs)
//  }
  def valCallbackOn(ref: Any, name: String, refs :  mutable.Set[Any]): Unit = {
    if (ref != null && !refs.contains(ref)) {
      refs += ref
      ref match {
        case range : Range =>
        case vec: Vec[_]   =>
        case seq: Seq[_]   =>
          for ((e, i) <- seq.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case seq: Array[_] =>
          for ((e, i) <- seq.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case seq: Set[_]   =>
          for ((e, i) <- seq.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case seq: mutable.LinkedHashSet[_]   =>
          for ((e, i) <- seq.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case seq: mutable.LinkedHashMap[_, _]   =>
          for ((e, i) <- seq.zipWithIndex) {
            valCallbackOn(e._2, name + "_" + i, refs)
          }
        case prod : Tuple2[_,_] if !name.contains("$")=> //$ check to avoid trigerring on val (x,y)
          for ((e, i) <- prod.productIterator.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case prod : Tuple3[_,_,_] if !name.contains("$") =>
          for ((e, i) <- prod.productIterator.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case prod : Tuple4[_,_,_,_] if !name.contains("$") =>
          for ((e, i) <- prod.productIterator.zipWithIndex) {
            valCallbackOn(e, name + "_" + i, refs)
          }
        case Some(x) => valCallbackOn(x, name, refs)
        case _             =>
      }

      valCallbackRec(ref, name)
    }
  }

  def valCallbackRec(ref: Any, name: String): Unit

  override def valCallback[T](ref: T, name: String): T = {
    val refs = mutable.Set[Any]()
    valCallbackOn(ref, name, refs)
    ref
  }
}

class Bundle extends MultiData with Nameable with ValCallbackRec {

  var hardtype: HardType[_] = null

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
        LocatedPendingError(s"Bundle assignment is not complete. Missing $name")
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

  def bundleAssign(that : Bundle)(f : (Data, Data) => Unit): Unit ={
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null) {
        LocatedPendingError(s"Bundle assignment is not complete. $this need '$name' but $that doesn't provide it.")
      }
      else {
        f(element, other)
      }
    }
  }

  protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case that: Bundle =>
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("Bundles must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        bundleAssign(that)((to, from) => to.compositAssignFrom(from,to,kind))
      case _ => throw new Exception("Undefined assignment")
    }
  }

  var elementsCache = ArrayBuffer[(String, Data)]()

  override def valCallbackRec(ref: Any, name: String): Unit = ref match {
    case ref : Data => {
      elementsCache += name -> ref
      ref.parent = this
      if(OwnableRef.proposal(ref, this)) ref.setPartialName(name, Nameable.DATAMODEL_WEAK)
    }
    case ref =>
  }


  override def elements: ArrayBuffer[(String, Data)] = elementsCache

  private[core] def rejectOlder = true

  def getTypeString = getClass.getSimpleName

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : $getTypeString"
}

class BundleCase extends Bundle {
  private[core] override def rejectOlder = false
}

trait IConnectable[T <: IConnectable[T]] {
  def connectFrom(that: T): T
  def <<(that: T): T = connectFrom(that)
  def >>(into: T): T = {
    into << this.asInstanceOf[T]
    into
  }
}