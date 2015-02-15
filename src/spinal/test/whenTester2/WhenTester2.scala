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

package spinal.test.whenTester

import spinal._
import spinal.importMe._
import spinal.test.SpinalJUnit
import org.junit._

class WhenTester extends Component {
  val io = new Bundle {
    val conds = in Vec(8, Bool())
    val data = in Vec(12, UInt(8 bit))
    val outDefault = out UInt (8 bit)
    val outComplex = out UInt (8 bit)
  }

  io.outDefault := io.data(0)
  when(io.conds(0)) {
    io.outDefault := io.data(1)
  }

  when(io.conds(0)) {
    io.outComplex := io.data(0)
  }.elsewhen(io.conds(1)) {
    io.outComplex := io.data(1)
    switch(io.data(3)) {
      is(io.data(4)) {
        io.outComplex := io.data(5)
      }
      is(io.data(6)) {
        io.outComplex := io.data(7)
      }
      is(UInt(0x55)) {
        when(io.conds(2)) {
          io.outComplex := UInt(0xAA)
        }.elsewhen(io.conds(3)) {
          io.outComplex := io.data(8)
        }
      }
    }
  }.otherwise {
    when(io.conds(4)) {
      io.outComplex := io.data(9)
    }otherwise{
      io.outComplex := io.data(10)
    }
  }
}


//class WhenTester2 extends WhenTester
//class WhenTester3 extends WhenTester
//class WhenTester4 extends WhenTester
//class WhenTester5 extends WhenTester
//class WhenTester6 extends WhenTester


class WhenTesterJUnit extends SpinalJUnit {
  override def getName: String = "WhenTester"
  override def createToplevel: Component = new WhenTester
}

//
//class WhenTester2JUnit extends SpinalJUnit {
//  override def getName: String = "WhenTester2"
//  override def createToplevel: Component = new WhenTester2
//}
//
//class WhenTester3JUnit extends SpinalJUnit {
//  override def getName: String = "WhenTester3"
//  override def createToplevel: Component = new WhenTester3
//}
//
//class WhenTester4JUnit extends SpinalJUnit {
//  override def getName: String = "WhenTester4"
//  override def createToplevel: Component = new WhenTester4
//}
//
//class WhenTester5JUnit extends SpinalJUnit {
//  override def getName: String = "WhenTester5"
//  override def createToplevel: Component = new WhenTester5
//}
//
//class WhenTester6JUnit extends SpinalJUnit {
//  override def getName: String = "WhenTester6"
//  override def createToplevel: Component = new WhenTester6
//}