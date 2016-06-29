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

//object Vec extends VecFactory{
//
//}

trait VecFactory {
  def Vec[T <: Data](elements: TraversableOnce[T]): Vec[T] = {
    val vector = elements.toVector
    val vecType = vector.reduce((a, b) => {
      if (a.getClass.isAssignableFrom(b.getClass)) a
      else if (b.getClass.isAssignableFrom(a.getClass)) b
      else throw new Exception("can't mux that")
    })

    val vec = new Vec(vecType, vector)
    vec
  }

  def Vec[T <: Data](gen: => T, size: Int): Vec[T] = fill(size)(gen)

  def Vec[T <: Data](gen: Vec[T], size: Int): Vec[Vec[T]] = fill(size)(gen.clone)

  def Vec[T <: Data](gen: (Int) => T, size: Int): Vec[T] = tabulate(size)(gen)

  //def apply[T <: Data](gen : => Vec[T],size : Int) : Vec[Vec[T]] = fill(size)(gen)

  @deprecated //swap data and size
  def Vec[T <: Data](size: Int, gen: => T): Vec[T] = fill(size)(gen)

  @deprecated //swap data and size
  def Vec[T <: Data](size: Int, gen: (Int) => T): Vec[T] = tabulate(size)(gen)

  def Vec[T <: Data](firstElement: T, followingElements: T*): Vec[T] = Vec(List(firstElement) ++ followingElements)

  def tabulate[T <: Data](size: Int)(gen: (Int) => T): Vec[T] = {
    Vec((0 until size).map(gen(_)))
  }

  def fill[T <: Data](size: Int)(gen: => T): Vec[T] = {
    tabulate(size)(_ => gen)
  }
}


object SeqMux {
  def apply[T <: Data](elements: Seq[T], _address: UInt): T = {
    var address = _address
    val bitNeeded = log2Up(elements.size)
    if(bitNeeded < address.getWidth){
      if(address.hasTag(tagAutoResize)){
        address = _address.resize(bitNeeded)
      }else {
        SpinalError(s"To many bit to address the vector (${address.getWidth} in place of ${bitNeeded})\n at\n${ScalaLocated.long}")
      }
    }


    if (elements.size == 1) {
      val ret = elements.head.clone()
      ret := elements.head
      return ret
    }

    val addressBools = address.asBools
    val addressWidth = address.getWidth
    def stage(elements: Seq[T], level: Int): T = {
      elements.size match {
        case 0 => throw new Exception("Can't mux a Vec of size zero")
        case 1 => elements(0)
        case _ => {
          val muxs = (0 until elements.length/2).map(i => Mux(address(level), elements(2*i + 1), elements(2*i)))
          stage(muxs ++ elements.slice(elements.length/2*2, elements.length), level + 1)
        }
      }
    }
    stage(elements, 0)
  }
}

class VecAccessAssign[T <: BaseType](enables: Seq[Bool], tos: Seq[T]) extends Assignable {
  override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    for ((enable, to) <- (enables, tos).zipped) {
      when(enable) {
        val thatSafe = that match {
          case that: AssignementNode => that.clone(to)
          case _ => that
        }
        to.assignFrom(thatSafe, conservative)
      }
    }
  }
}

class Vec[T <: Data](_dataType: T, val vec: Vector[T]) extends MultiData with collection.IndexedSeq[T] {

  def dataType = cloneOf(_dataType)

  def range = 0 until vec.length

  override def equals(that: Any): Boolean = that match {
    case that: Vec[_] => instanceCounter == that.instanceCounter
    case _ => false
  }

  override def hashCode(): Int = instanceCounter

  private[core] val accessMap = mutable.Map[(Component, UInt), T]()
  private[core] val readMap = mutable.Map[(Component, UInt), T]()
  private[core] var vecTransposedCache: ArrayBuffer[ArrayBuffer[BaseType]] = null

  private[core] def vecTransposed: ArrayBuffer[ArrayBuffer[BaseType]] = {
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
    if (idx < 0 || idx >= vec.size) SpinalError(s"Static Vec($idx) is outside the range (${vec.size - 1} downto 0) of ${this}")
    vec(idx)
  }


  def apply(address: UInt): T = {
    access(address)
  }

  def read(address: UInt): T = {
    val key = (Component.current, address)
    if (readMap.contains(key)) return accessMap(key)


    val ret = SeqMux(vec.take(Math.min(vec.length, 1 << address.getWidth)), address)

    readMap += (key -> ret)
    ret
  }

  def access(address: UInt): T = {
    val key = (Component.current, address)
    if (accessMap.contains(key)) return accessMap(key)


    val ret = SeqMux(vec.take(Math.min(vec.length, 1 << address.getWidth)), address)
    val enables = (U(1) << address).asBools
    for ((accessE, to) <- (ret.flatten, vecTransposed).zipped) {
      accessE.compositeAssign = new VecAccessAssign(enables, to)
    }

    accessMap += (key -> ret)
    ret
  }

  //TODO sub element composite assignement, as well for indexed access (std)
  def oneHotAccess(oneHot: Bits): T = {
    if(elements.size == oneHot.getWidth){
      SpinalError(s"To many bit to address the vector (${oneHot.getWidth} in place of ${elements.size})\n at\n${ScalaLocated.long}")
    }
    val ret = dataType.clone
    ret := ret.getZero
    for ((e, idx) <- vec.zipWithIndex) {
      when(oneHot(idx)) {
        ret := e
      }
    }
    ret.compositeAssign = new Assignable {
      override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
        assert(!conservative)
        for ((e, idx) <- vec.zipWithIndex) {
          when(oneHot(idx)) {
            e := that.asInstanceOf[T]
          }
        }
      }
    }

    ret
  }

  private[core] override def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    assert(!conservative)
    that match {
      case that: Vec[T] => {
        if (that.vec.size != this.vec.size) throw new Exception("Can't assign Vec with a different size")
        for ((to, from) <- (this.vec, that.vec).zipped) {
          to.:=(from)
        }
      }
      case _ => throw new Exception("Undefined assignment")
    }
  }


  private var elementsCache: ArrayBuffer[(String, Data)] = null

  def elements = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      //      var i = vec.size -1
      //      while(i >= 0) {
      //        elementsCache += Tuple2(i.toString, vec(i))
      //        i = i - 1
      //      }
      for ((e, i) <- vec.zipWithIndex) {
        elementsCache += Tuple2(i.toString, e)
        Ownable.set(e,this)
      }
    }
    elementsCache
  }

  override def clone: this.type = {
    new Vec[T](dataType, vec.map(_.clone())).asInstanceOf[this.type]
  }
}

