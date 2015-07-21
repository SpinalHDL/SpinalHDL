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

package spinal.tester.pending

import spinal.core._


object MyEnum extends SpinalEnum{
  val a = fix(2)
  val b = fix(3)
  val c = fix(4)
  val e,f,g = ordered
}


object MyEnum2{
  def apply() = Bits(3 bit)
  def a = 1
  def b = 2
  def c = 3
}


object Debug {

  //Test new WhenNode system
  class TopLevel extends Component {
    val io = new Bundle {
      val conds = in Vec(Bool, 8)
      val outs = out Vec(Bool, 6)
    }
    val Hello = Vec(Seq('S','Y','S','I','D','0','1',' ').map(c => B(c.toInt,8 bit)))
    val b = UInt()
    b := U(3) + 12

    val size = 2
    val conds = Vec(UInt(10 bit), 10)

    val reg0 = Reg(Bool)

    when(io.conds(0)) {
      reg0 := !reg0
      when(io.conds(1)) {
        reg0 := reg0 ^ io.conds(2)
      }
      reg0 := !io.conds(3)
      reg0 := !io.conds(4)
      when(io.conds(5)) {
        reg0 := reg0 ^ io.conds(6)
      }
    }
    io.outs(0) := reg0


    io.outs(1) := io.conds(0)
    when(io.conds(1)) {
      io.outs(1) := False
    }
    io.outs(2) := io.conds(3)
    when(io.conds(4)) {
      io.outs(2) := False
    }
    io.outs(3) := io.conds(4)
    when(io.conds(5)) {
      io.outs(3) := False
    }

    var memo: Bool = null
    when(io.conds(6)) {
      memo = Bool
      memo := io.conds(6)
      when(io.conds(7)) {
        memo := False
        io.outs(1) := True
        io.outs(2) := True
      }
    }

    io.outs(5) := memo
    memo = null


    when(U(3, 4 bit) < U(5, 7 bit)) {
      io.outs(4) := False
    }.otherwise {
      io.outs(4) := True
    }


  }


  def main(args: Array[String]) {


    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}

