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

package spinal.scalaTest.pending

import spinal._
import spinal.importMe._

object Debug {

  //Test new WhenNode system
  class TopLevel extends Component {
    val io = new Bundle {
      val conds = in Vec(8, Bool())
      val outs = out Vec(6, Bool())
    }

    val reg0 = Reg(Bool())

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
      io.outs(1) := Bool(false)
    }
    io.outs(2) := io.conds(3)
    when(io.conds(4)) {
      io.outs(2) := Bool(false)
    }
    io.outs(3) := io.conds(4)
    when(io.conds(5)) {
      io.outs(3) := Bool(false)
    }

    var memo : Bool = null
    when(io.conds(6)) {
      memo = Bool()
      memo := io.conds(6)
      when(io.conds(7)){
        memo := Bool(false)
        io.outs(1) := Bool(true)
        io.outs(2) := Bool(true)
      }
    }

    io.outs(5) := memo
    memo = null


    when(UInt(3, 4 bit) < UInt(5, 7 bit)) {
      io.outs(4) := Bool(false)
    }.otherwise {
      io.outs(4) := Bool(true)
    }

  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}

