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

package spinal.code

import spinal._
import spinal.importMe._

object Debug {


  class TopLevel extends Component {
    val io = new Bundle {
      val conds = in Vec(4,Bool())
      val outs = out Vec(1,Bool())
    }

    val reg0 = Reg(Bool())

    when(io.conds(0)){
      //reg0 := ! reg0
      when(io.conds(1)){
        reg0 := reg0 ^ io.conds(2)
      }
    }

    io.outs(0) := reg0

  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}

