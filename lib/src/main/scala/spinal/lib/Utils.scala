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

package spinal.lib

import spinal.core._


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object OHToUInt {
  def apply(bitVector: BitVector): UInt = apply(bitVector.toBools)

  def apply(bools: collection.IndexedSeq[Bool]): UInt = {
    val boolsSize = bools.size
    if (boolsSize < 2) return u(0)

    val retBitCount = log2Up(bools.size)
    val ret = Vec(retBitCount, Bool())

    for (retBitId <- 0 until retBitCount) {
      var bit: Bool = null
      for (boolsBitId <- 0 until boolsSize if ((boolsBitId >> retBitId) & 1) != 0) {
        if (bit != null)
          bit = bit | bools(boolsBitId)
        else
          bit = bools(boolsBitId)
      }
      ret(retBitId) := bit.dontSimplifyIt
    }

    ret.toBits.toUInt
  }
}

object toGray{
  def apply(uint : UInt): Bits ={
    Bits((uint >> u(1)) ^ uint)
  }
}

object fromGray{
  def apply(gray : Bits): UInt ={
    val ret = UInt(widthOf(gray) bit)
    for(i <- 0 until widthOf(gray)-1){
      ret(i) := gray(i) ^ ret(i+1)
    }
    ret.msb := gray.msb
    ret
  }
}



//object Flag{
//  def apply() = new Flag
//
//  implicit def implicitValue(f: Flag) = f.value
//}
//class Flag extends Area{
//  val value = Bool(false)
//  def set = value := Bool(true)
//}

object CounterFreeRun {
  def apply(stateCount: Int) : Counter  = {
    new Counter(stateCount,true)
  }
}

object Counter {
  def apply(stateCount: Int) : Counter = new Counter(stateCount)
  def apply(stateCount: Int,inc : Bool) : Counter  = {
    val counter = Counter(stateCount)
    when(inc) {
      counter ++;
    }
    counter
  }
  implicit def implicitValue(c: Counter) = c.value
}

class Counter(val stateCount: Int,freeRun : Boolean = false) extends Area {
  val inc = Bool(freeRun)
  def ++(): UInt = {
    inc := Bool(true)
    valueNext
  }

  val valueNext = UInt(log2Up(stateCount) bit)
  val value = RegNext(valueNext, u(0))

  if (isPow2(stateCount))
    valueNext := value + inc.toUInt
  else {
    when(inc) {
      when(value === u(stateCount - 1)) {
        valueNext := u(0)
      } otherwise {
        valueNext := value + u(1)
      }
    }
  }
}




object MajorityVote {
  def apply(that: BitVector): Bool = apply(that.toBools)
  def apply(that: collection.IndexedSeq[Bool]): Bool = {
    val size = that.size
    val trigger = that.size / 2 + 1
    var ret = Bool(false)
    for (i <- BigInt(0) until (BigInt(1) << size)) {
      if (i.bitCount == trigger) {
        val bits = ArrayBuffer[Bool]()
        for (bitId <- 0 until i.bitLength) {
          if (i.testBit(bitId)) bits += that(bitId)
        }
        ret = ret | bits.reduceLeft(_ & _)
      }
    }
    ret
  }
}

object SpinalMap {
  def apply[Key <: Data, Value <: Data](elems: Tuple2[() => Key, () => Value]*): SpinalMap[Key, Value] = {
    new SpinalMap(elems)
  }
}

class SpinalMap[Key <: Data, Value <: Data](pairs: Iterable[(() => Key, () => Value)]) {
  def apply(key: Key): Value = {
    val ret: Value = pairs.head._2()

    for ((k, v) <- pairs.tail) {
      when(k() === key) {
        ret := v()
      }
    }

    ret
  }
}


object latencyAnalysis {
  //Don't care about clock domain
  def apply(paths: Node*): Integer = {
    var stack = 0;
    for (i <- (0 to paths.size - 2)) {
      stack = stack + impl(paths(i), paths(i + 1))
    }
    stack
  }

  def impl(from: Node, to: Node): Integer = {
    val walked = mutable.Set[Node]()
    var pendingStack = mutable.ArrayBuffer[Node](to)
    var depth = 0;

    while (pendingStack.size != 0) {
      val iterOn = pendingStack
      pendingStack = new mutable.ArrayBuffer[Node](10000)
      for (start <- iterOn) {
        if (walk(start)) return depth;
      }
      depth = depth + 1
    }

    def walk(that: Node, depth: Integer = 0): Boolean = {
      if (that == null) return false
      if (walked.contains(that)) return false
      walked += that
      if (that == from)
        return true
      that match {
        case delay: SyncNode => {
          for (input <- delay.getAsynchronousInputs) {
            if (walk(input)) return true
          }
          pendingStack ++= delay.getSynchronousInputs
        }
        case _ => {
          for (input <- that.inputs) {
            if (walk(input)) return true
          }
        }
      }
      false
    }

    SpinalError("latencyAnalysis don't find any path")
    -1
  }
}



trait DataCarrier[T <: Data]{
  def fire : Bool
  def valid : Bool
  def data : T
  def freeRun : Unit
}