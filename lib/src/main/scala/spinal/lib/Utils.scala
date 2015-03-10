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

import spinal._

import scala.collection.mutable.ArrayBuffer


object OHToUInt {
  def apply(bitVector: BitVector): UInt = apply(bitVector.toBools)

  def apply(bools: collection.IndexedSeq[Bool]): UInt = {
    val boolsSize = bools.size
    if (boolsSize < 2) return UInt(0)

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

object Counter {
  def apply(stateCount: Int) = new Counter(stateCount)
  implicit def implicitValue(c: Counter) = c.value
}

class Counter(val stateCount: Int) extends Area {
  val inc = Bool(false)
  def ++() : Unit = inc := Bool(true)

  val valueNext = UInt(log2Up(stateCount) bit)
  val value = RegNext(valueNext, UInt(0))

  if (isPow2(stateCount))
    valueNext := value + inc.toUInt
  else {
    when(inc) {
      when(value === UInt(stateCount - 1)) {
        valueNext := UInt(0)
      } otherwise {
        valueNext := value + UInt(1)
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

class SpinalMap[Key <: Data, Value <: Data](pairs: Iterable[( () => Key,() => Value)]) {
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