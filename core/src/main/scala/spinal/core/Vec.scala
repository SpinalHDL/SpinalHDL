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

import spinal.core.internals.{BitAssignmentFixed, BitAssignmentFloating, BitVectorAssignmentExpression, RangedAssignmentFixed, RangedAssignmentFloating}
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

/**
  * Vec factory
  */
trait VecFactory {

  def Vec[T <: Data](elements: TraversableOnce[T], dataType : HardType[T] = null): Vec[T] = {
    val vector = elements.toVector

    if(vector.nonEmpty) {
      new Vec(dataType, vector)
    }else{
      new Vec[T](null.asInstanceOf[T], vector)
    }
  }

  def Vec[T <: Data](gen: => T, size: Int): Vec[T] = Vec.fill(size)(gen)
  def Vec[T <: Data](gen: HardType[T], size: Int): Vec[T] = Vec.fill(size)(gen())

//  def Vec[T <: Data](gen: Vec[T], size: Int): Vec[Vec[T]] = fill(size)(cloneOf(gen))

//  def Vec[T <: Data](gen: (Int) => T, size: Int): Vec[T] = tabulate(size)(gen)

  //def apply[T <: Data](gen : => Vec[T],size : Int) : Vec[Vec[T]] = fill(size)(gen)


  def Vec[T <: Data](firstElement: T, followingElements: T*): Vec[T] = Vec(List(firstElement) ++ followingElements)

  class VecBuilder{
    def tabulate[T <: Data](size: Int)(gen: (Int) => T): Vec[T] = {
      Vec((0 until size).map(gen(_)))
    }

    def fill[T <: Data](size: Int)(dataType: => T): Vec[T] = {
      Vec((0 until size).map(_ => dataType), HardType(dataType))
    }
  }
  val Vec = new VecBuilder()
}



class VecAccessAssign[T <: Data](enables: Seq[Bool], tos: Seq[BaseType], vec: Vec[T]) extends Assignable {

  override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    for ((enable, to) <- (enables, tos).zipped) {
      when(enable) {
        val thatSafe = that /*match {
          case that: AssignmentNode => that.clone(to)
          case _ => that
        }*/
        target match {
          case a : BitVectorAssignmentExpression => to.compositAssignFrom(thatSafe, a.copyWithTarget(to.asInstanceOf[BitVector]), kind)
          case bt : BaseType => to.compositAssignFrom(thatSafe, to, kind)
        }
      }
    }
  }

  override def getRealSourceNoRec: Any = vec
}


/**
  * The Vec is a composite type that defines a group of indexed signals (of any SpinalHDL basic type) under a single name
  *
  * @example {{{
  *     val myVecOfSInt = Vec(SInt(8 bits), 2)
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Vector Vec Documentation]]
  */
class Vec[T <: Data](var _dataType : HardType[T], val vec: Vector[T]) extends MultiData with collection.IndexedSeq[T] {

  def dataType = {
    if(_dataType == null){
      val data = vec.reduce((a, b) => {
        if (a.getClass.isAssignableFrom(b.getClass)) a
        else if (b.getClass.isAssignableFrom(a.getClass)) b
        else throw new Exception("can't mux that")
      })
      _dataType = data.getMuxType(vec)
    }
    _dataType
  }



  for(i <- elements.indices){
    val e = elements(i)._2
    if(OwnableRef.proposal(e, this)) e.setPartialName(i.toString, Nameable.DATAMODEL_WEAK)
  }

  def range = vec.indices

  override def length: Int = vec.size

  override def equals(that: Any): Boolean = that match {
    case that: Vec[_] => instanceCounter == that.instanceCounter
    case _            => false
  }

  override def hashCode(): Int = instanceCounter

  private[core] val accessMap = mutable.Map[(Component, UInt), T]()
  private[core] val readMap   = mutable.Map[(Component, UInt), T]()
  private[core] var vecTransposedCache: ArrayBuffer[ArrayBuffer[BaseType]] = null

  private[core] def vecTransposed: ArrayBuffer[ArrayBuffer[BaseType]] = {
    if (vecTransposedCache == null) {
      vecTransposedCache = new ArrayBuffer[ArrayBuffer[BaseType]]()
      val size = dataType().flatten.size

      for (i <- 0 until size)
        vecTransposedCache += ArrayBuffer[BaseType]()

      for (vecElement <- vec) {
        for ((e, i) <- vecElement.flatten.zipWithIndex) {
          vecTransposedCache(i) += e
        }
      }
    }
    vecTransposedCache
  }

  /** Access an element of the vector by an Int index */
  override def apply(idx: Int): T = {
    if (idx < 0 || idx >= vec.size) SpinalError(s"Static Vec($idx) is outside the range (${vec.size - 1} downto 0) of ${this}")
    vec(idx)
  }

  /** Access an element of the vector by an UInt index */
  def apply(address: UInt): T = access(address)

  private def readEmu(address : UInt): T = {
    if(elements.size == 0){
      throw new Exception("Can't mux a Vec of size zero")
    }
    if (elements.size == 1) {
      val ret = cloneOf(vec.head)
      ret := vec.head
      return ret
    }
    //    val ret = SeqMux(vec.take(Math.min(vec.length, 1 << address.getWidth)), address)
    var finalAddress   = address
    val bitNeeded = log2Up(elements.size)

    if(bitNeeded < finalAddress.getWidth){
      if(finalAddress.hasTag(tagAutoResize)){
        finalAddress = address.resize(bitNeeded)
      }else {
        SpinalError(s"Too many bit to address the vector (${finalAddress.getWidth} in place of $bitNeeded)\n at\n${ScalaLocated.long}")
      }
    }


    val ret = dataType()
    def rec(ret : Data, elements : Traversable[Data]): Unit ={
      ret match {
        case ret : MultiData =>{
          val iRet = ret.elements.iterator
          val iIn = elements.map(_.toMuxInput[Data](ret).asInstanceOf[MultiData].elements.iterator)
          val continue = true
          while(iRet.nonEmpty && continue){
            val dst = iRet.next()
            val srcs = iIn.map(_.next())
            assert(srcs.forall(_._1 == dst._1), "Doesn't match ???")
            rec(dst._2, srcs.map(_._2))
          }
        }
        case ret : BaseType => {
          val ab = ArrayBuffer[BaseType]()
          ab ++= elements.map(_.toMuxInput(ret))
          ret.assignFrom(ret.newMultiplexer(finalAddress, ab))
        }
      }
    }
    rec(ret, vec)
    ret
  }

  private def fixAddress(address : UInt) : UInt = if(widthOf(address) != log2Up(length)){
    if(address.hasTag(tagAutoResize)){
      address.resize(log2Up(length))
    }else{
      LocatedPendingError(s"Vec address width mismatch.\n- Vec : $this\n- Address width : ${widthOf(address)}\n")
      address
    }
  }else{
    address
  }

  def read(address: UInt): T = {
    val key = (Component.current, address)
    if (readMap.contains(key)) return readMap(key)
    val trueAddress = fixAddress(address)
    val ret = readEmu(trueAddress)

    readMap += (key -> ret)
    ret
  }

  def access(address: UInt): T = {
    val key = (Component.current, address)
    if (accessMap.contains(key)) return accessMap(key)
    val trueAddress = fixAddress(address)

    val ret     = readEmu(trueAddress)
    val enables = (U(1) << trueAddress).asBools

    for ((accessE, to) <- (ret.flatten, vecTransposed).zipped) {
      accessE.compositeAssign = new VecAccessAssign[T](enables, to, this)
    }

    accessMap += (key -> ret)
    ret
  }

  //TODO sub element composite assignment, as well for indexed access (std)
  /** Access an element of the vector by a oneHot value */
  def oneHotAccess(oneHot: Bits): T = {

    if(elements.size != oneHot.getWidth){
      SpinalError(s"Invalid length of oneHot selection vector (${oneHot.getWidth}), not matching length of Vec (${elements.size})\n at\n${ScalaLocated.long}")
    }

    val ret = cloneOf(dataType)
    ret := ret.getZero

    for ((e, idx) <- vec.zipWithIndex) {
      when(oneHot(idx)) {
        ret := e
      }
    }

    ret.compositeAssign = new Assignable {
      override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
        for ((e, idx) <- vec.zipWithIndex) {
          when(oneHot(idx)) {
            e.compositAssignFrom(that, target,kind)
          }
        }
      }
      override def getRealSourceNoRec: Any = Vec.this
    }

    ret
  }

  protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case that: Vec[T] =>
        if (that.vec.size != this.vec.size) throw new Exception("Can't assign Vec with a different size")
        for ((to, from) <- (this.vec, that.vec).zipped) {
          to.compositAssignFrom(from, to, kind)
        }
      case _            => throw new Exception("Undefined assignment")
    }
  }

  private var elementsCache: ArrayBuffer[(String, Data)] = null

  override def elements = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      for ((e, i) <- vec.zipWithIndex) {
        elementsCache += Tuple2(i.toString, e)
      }
    }
    elementsCache
  }

  override def clone: this.type = new Vec[T](dataType, vec.map(cloneOf(_))).asInstanceOf[this.type]

  override def toString() = s"${getDisplayName()} : Vec of $length elements"
}

class VecBitwisePimper[T <: Data with BitwiseOp[T]](pimped : Vec[T]) extends BitwiseOp[Vec[T]] {
  override def |(other: Vec[T]): Vec[T] = map2with(_ | _)(other)
  override def &(other: Vec[T]): Vec[T] = map2with(_ & _)(other)
  override def ^(other: Vec[T]): Vec[T] = map2with(_ ^ _)(other)
  override def unary_~ : Vec[T] = Vec(pimped.map(~ _))

  private def map2with(f: (T, T) => T)(other: Vec[T]): Vec[T] = {
    if (pimped.length != other.length)
      SpinalError(s"Cannot apply a bitwize opration on vectors with different size (${pimped.length} vs ${other.length})")
    Vec((pimped, other).zipped.map(f))
  }
}
