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

import spinal.core
import spinal.core._


object Debug {


  class TopLevel extends Component {
    val io = new Bundle {
      val conds = core.in Vec(8, Bool())
      val outs = core.out Vec(7, core.Bool())

      val outBits = core.out Bits(8 bit)

      val sel = core.in UInt(4 bit)
      val selIn = core.in SInt(16 bit)
      val selOut = core.out SInt(3 bit)

      val demux = core.out SInt(16 bit)
      val partialAssign = core.out Bits(16 bit)
    }

    io.partialAssign(0) := io.conds(0)
    io.partialAssign(1) := io.conds(1)
    when(io.conds(2)){
      io.partialAssign(2) := core.Bool(false)
    }.elsewhen(io.conds(3)) {
      io.partialAssign(2) := core.Bool(true)
    }.otherwise{
      io.partialAssign(3,2) := core.Bits(54)
    }
    io.partialAssign(3,13 bit) := core.Bits(0)



    io.selOut := io.selIn(io.sel,io.selOut.getWidth bit)
    io.demux := SInt(0)
    io.demux(io.sel,io.selOut.getWidth bit) := io.selOut




    {
      val initialValue = core.Bool()
      initialValue := io.conds(4)
      core.when(io.conds(5)) {
        initialValue := core.Bool(false)
      }
      io.outs(6) := RegNext(!io.outs(6),initialValue)
    }
    io.outBits := core.Bits(0xAA)
    io.outBits(3,2) := io.conds.toBits(1,0)


    val reg0 = Reg(core.Bool())

    core.when(io.conds(0)) {
      reg0 := !reg0
      core.when(io.conds(1)) {
        reg0 := reg0 ^ io.conds(2)
      }
      reg0 := !io.conds(3)
      reg0 := !io.conds(4)
      core.when(io.conds(5)) {
        reg0 := reg0 ^ io.conds(6)
      }
    }
    io.outs(0) := reg0


    io.outs(1) := io.conds(0)
    core.when(io.conds(1)) {
      io.outs(1) := core.Bool(false)
    }
    io.outs(2) := io.conds(3)
    core.when(io.conds(4)) {
      io.outs(2) := core.Bool(false)
    }
    io.outs(3) := io.conds(4)
    core.when(io.conds(5)) {
      io.outs(3) := core.Bool(false)
    }

    var memo : Bool = null
    core.when(io.conds(6)) {
      memo = core.Bool()
      memo := io.conds(6)
      core.when(io.conds(7)){
        memo := core.Bool(false)
        io.outs(1) := core.Bool(true)
        io.outs(2) := core.Bool(true)
      }
    }

    io.outs(5) := memo
    memo = null


    core.when(core.UInt(3, 4 bit) < core.UInt(5, 7 bit)) {
      io.outs(4) := core.Bool(false)
    }.otherwise {
      io.outs(4) := core.Bool(true)
    }

  }


  def main(args: Array[String]) {
    println("START")
    core.SpinalVhdl(new TopLevel)
    println("DONE")
  }

}

