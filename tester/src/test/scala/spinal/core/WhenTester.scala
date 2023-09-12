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

import spinal.tester.SpinalTesterCocotbBase

class WhenTester extends Component {
  val io = new Bundle {
    val conds = in Vec(Bool(),8)
    val data = in Vec(UInt(8 bit),12)
    val outDefault = out UInt (8 bit)
    val outComplex = out UInt (8 bit)
    val outRegComplex = out (Reg(UInt (8 bit)))
  }

  io.outDefault := io.data(0)
  when(io.conds(0)) {
    io.outDefault := io.data(1)
  }


  complexOn(io.outComplex)
  complexOn(io.outRegComplex)

  def complexOn(that : UInt): Unit = {
    when(io.conds(0)) {
      that := io.data(0)
    } elsewhen(io.conds(1)) {
      that := io.data(1)
      switch(io.data(3)) {
        is(io.data(4)) {
          that := io.data(5)
        }
        default{
          that := io.data(11)
        }
        is(io.data(6)) {
          that := io.data(7)
        }
        is(U(0x55)) {
          when(io.conds(2)) {
            that := U(0xAA)
          }elsewhen(io.conds(3)) {
            that := io.data(8)
          }
        }

      }
    } otherwise {
      when(io.conds(4)) {
        that := io.data(9)
      }otherwise{
        that := io.data(10)
      }
    }
  }

}


//class WhenTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "WhenTester"
//  override def createToplevel: Component = new WhenTester
//}

class WhenTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "WhenTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/WhenTester"
  override def createToplevel: Component = new WhenTester
}