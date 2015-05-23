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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Vec extends VecFactory{

}

class VecFactory{
  def apply[T <: Data](elements : Iterable[T]) : Vec[T] = {
    val vecType = elements.reduce((a,b) => {
      if (a.getClass.isAssignableFrom(b.getClass)) a
      else if (b.getClass.isAssignableFrom(a.getClass)) b
      else throw new Exception("can't mux that")
    }).clone()

    val vec = new Vec(vecType,elements.toVector)
    vec
  }

  def apply[T <: Data](size : Int,gen : => T) : Vec[T] = fill(size)(gen)
  def apply[T <: Data](size : Int,gen :(Int) => T) : Vec[T] = tabulate(size)(gen)

  def tabulate[T <: Data](size : Int)(gen : (Int)=> T) : Vec[T] ={
    this((0 until size).map(gen(_)))
  }

  def fill[T <: Data](size : Int)(gen : => T) : Vec[T] ={
    tabulate(size)(_ => gen)
  }
}


object SeqMux {
  def apply[T <: Data](elements: Seq[T], address: UInt): T = {
    val addressBools = address.toBools
    val addressWidth = address.getWidth
    def stage(elements: Seq[T], level: Int): T = {
      elements.size match {
        case 0 => throw new Exception("Can't mux a Vec of size zero")
        case 1 => elements(0)
        case _ => {
          val split = elements.grouped((elements.size + 1) / 2).toList
          Mux(addressBools(addressWidth-level-1), stage(split(1), level + 1), stage(split(0), level + 1))
        }
      }
    }
    stage(elements, 0)
  }
}

class VecAccessAssign[T <: BaseType](enables: Seq[Bool], tos: Seq[T]) extends Assignable {
  override def assignFromImpl(that: AnyRef,conservative : Boolean): Unit = {
    assert(!conservative)
    for ((enable, to) <- (enables, tos).zipped) {
      when(enable) {
        to := that.asInstanceOf[T]
      }
    }
  }
}

class Vec[T <: Data](_dataType: T,val vec : Vector[T]) extends MultiData with collection.IndexedSeq[T]{

  def dataType = cloneOf(_dataType)


  override def equals(that : Any) : Boolean = that match{
    case that : Vec[_] => instanceCounter == that.instanceCounter
    case _ => false
  }
  override def hashCode(): Int = instanceCounter

  val accessMap = mutable.Map[(Component,UInt), T]()
  var vecTransposedCache: ArrayBuffer[ArrayBuffer[BaseType]] = null

  def vecTransposed: ArrayBuffer[ArrayBuffer[BaseType]] = {
    if (vecTransposedCache == null) {
      vecTransposedCache = new ArrayBuffer[ArrayBuffer[BaseType]]()
      val size = dataType.flatten.size
      for (i <- 0 until size)
        vecTransposedCache += ArrayBuffer[BaseType]()

      for (vecElement <- vec) {
        for ((e, i) <- vecElement.flatten.zipWithIndex) {
          vecTransposedCache(i) += e;
        }
      }
    }
    vecTransposedCache
  }

  override def length: Int = vec.size


  def apply(idx: Int): T = {
    if(idx < 0 || idx >= vec.size) SpinalError(s"Static Vec($idx) is outside the range (${vec.size - 1} downto 0) of ${this}")
    vec(idx)
  }




  def apply(address: UInt): T = {
    access(address)
  }


  def access(address: UInt): T = {
    val key = (Component.current,address)
    if (accessMap.contains(key)) return accessMap(key)


    val ret = SeqMux(vec, address)
    val enables = (U(1) << address).toBools
    for ((accessE, to) <- (ret.flatten, vecTransposed).zipped) {
      accessE.compositeAssign = new VecAccessAssign(enables,to)
    }

    accessMap += (key -> ret)
    ret
  }

  override def assignFromImpl(that: AnyRef,conservative : Boolean): Unit = {
    assert(!conservative)
    that match {
      case that: Vec[T] => {
        if (that.vec.size != this.vec.size) throw new Exception("Can't assign Vec with a different size")
        for ((to, from) <- (this.vec, that.vec).zipped) {
          to.:=(from)
        }
      }
      case _ => throw new Exception("Undefined assignement")
    }
  }


  private var elementsCache: ArrayBuffer[(String, Data)] = null

  def elements = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      var i = vec.size -1
      while(i >= 0) {
        elementsCache += Tuple2(i.toString, vec(i))
        i = i - 1
      }
    }
    elementsCache
  }

  override def clone(): this.type = {
    new Vec[T](dataType,vec.map(_.clone())).asInstanceOf[this.type]
  }
}
