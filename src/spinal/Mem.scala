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

package spinal


import spinal.IntBuilder._
/**
 * Created by PIC18F on 02.02.2015.
 */



/*
val mem = Mem(Bits(2 bit),1024)
mem(28) := UInt(3) =>



 */

class RamDualPort(wordCount : Int,wordWidth : Int) extends BlackBox{
  val generic = new Bundle {
    val wordCount = Number(RamDualPort.this.wordCount)
    val wordWidth = Number(RamDualPort.this.wordWidth)
  }

  val io = new Bundle {
    val wr = new Bundle{
      val en = in.Bool()
      val addr = in Bits(log2Up(wordCount) bit)
      val data = in Bits(wordWidth bit)
    }
    val rd = new Bundle{
      val en = in.Bool()
      val addr = in Bits(log2Up(wordCount) bit)
      val data = out Bits(wordWidth bit)
    }

  }
}
