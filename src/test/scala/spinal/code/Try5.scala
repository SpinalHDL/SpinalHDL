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
/**
 * Created by PIC18F on 22.08.2014.
 */
object Try5 {

  class SubComponent extends Component {
    val io = new Bundle {
      val in = (new Bool().asInput)
      val out = new Bool().asOutput
    }
    io.out := RegNext(io.in)
  }


  class TopLevel(clockC: ClockDomain) extends Component {
    val io = new Bundle {
      val clkA = in Bool()
      val resetA = in Bool()
      val clkEnA = in Bool()

      val clkB = in Bool()
      val resetB = in Bool()


      val in0 = in Bool()
      val in1 = in Bool()
      val in2 = in Bool()
      val outA = out Bool()
      val outB = out Bool()
      val outC = out Bool()
      val outX = out Bool()

      val wrAddr = in UInt(4 bit)
      val wrData = in Bool()
      val rdAddr = in UInt(4 bit)
      val rdData = out Bool()
    }
    val clockA = ClockDomain(io.clkA, io.resetA, clockEnable = io.clkEnA)
    val clockB = ClockDomain(io.clkB, io.resetB)
    val mem = Mem(Bool(),16)

    clockA.push
    io.outA := RegNext(io.in0, Bool(true))
    mem.write(io.wrAddr,io.wrData)

    clockA.pop

    val B = new ComponentPart {
      clockB.push
      val reg1 = RegNext(io.in1, Bool(true))
      io.outB := reg1
      io.outX := RegNext(io.outA && io.outB).addTag(crossClockDomain)

      io.rdData := RegNext(mem.readSync(io.rdAddr).addTag(crossClockDomain))

      clockB.pop
    }


    clockC.push
    val subComponent = Component(new SubComponent)
    subComponent.io.in := io.in2
    io.outC := subComponent.io.out
    clockC.pop


  }


  def main(args: Array[String]) {
    println("START")

    SpinalVhdl({
      val clockC = ClockDomain(Bool().setName("clkC"), Bool().setName("resetC"))
      new TopLevel(clockC)})
    println("DONE")
  }

}

