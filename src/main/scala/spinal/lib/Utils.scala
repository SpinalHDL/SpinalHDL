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


//Give number of bit to encode a given number of states
object log2Up {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    (value - 1).bitLength
  }
}

object OHToUInt{
  def apply(bitVector: BitVector) : UInt = apply(bitVector.toBools)

  def apply(bools : collection.IndexedSeq[Bool]): UInt ={
    val boolsSize = bools.size
    if(boolsSize < 2) return UInt(0)

    val retBitCount = log2Up(bools.size)
    val ret = Vec(retBitCount,Bool())

    for(retBitId <- 0 until retBitCount){
      var bit : Bool = null
      for(boolsBitId <- 0 until boolsSize if ((boolsBitId >> retBitId) & 1) != 0){
        if(bit != null)
          bit = bit | bools(boolsBitId)
        else
          bit = bools(boolsBitId)
      }
      ret(retBitId) := bit.dontSimplifyIt
    }

    ret.toBits.toUInt
  }
}
