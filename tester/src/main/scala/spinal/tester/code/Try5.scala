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

package spinal.tester.code

import spinal.core
import spinal.core._

/**
 * Created by PIC18F on 22.08.2014.
 */
object Try5 {

  class SubComponent extends Component {
    val io = new Bundle {
      val in = (Bool.asInput())
      val out = Bool.asOutput()
    }
    io.out := RegNext(io.in)
  }

  class BundleA extends Bundle{
    val a = Bool

    val sub = new Bundle{
      val b = core.Bool
      val sub = new Bundle{
        val c = core.Bool
      }
    }
  }

  class TopLevel(clockC: ClockDomain) extends Component {
    val io = new Bundle {
      val bundleIn = core.in (new BundleA)


      val clkA = core.in Bool
      val resetA = core.in Bool
      val clkEnA = core.in Bool

      val clkB = core.in Bool
      val resetB = core.in Bool


      val in0 = core.in Bool
      val in1 = core.in Bool
      val in2 = core.in Bool
      val outA = core.out Bool
      val outB = core.out Bool
      val outC = core.out Bool
      val outX = core.out Bool

      val wrAddr = core.in UInt(4 bit)
      val wrData = core.in Bool
      val rdAddr = core.in UInt(4 bit)
      val rdData = core.out Bool
    }
    val clockA = ClockDomain(io.clkA, io.resetA, clockEnable = io.clkEnA)
    val clockB = core.ClockDomain(io.clkB, io.resetB)
    val mem = Mem(core.Bool,16)

    clockA.push
    io.outA := core.RegNext(io.in0, core.True)
    mem.write(io.wrAddr,io.wrData)

    clockA.pop

    val B = new Area {
      clockB.push
      val reg1 = core.RegNext(io.in1, core.True)
      io.outB := reg1
      io.outX := core.RegNext(io.outA && io.outB).addTag(core.crossClockDomain)

      io.rdData := core.RegNext(mem.readSync(io.rdAddr).addTag(core.crossClockDomain))

      clockB.pop
    }


    clockC.push
    val subComponent = new SubComponent
    subComponent.io.in := io.in2
    io.outC := subComponent.io.out
    clockC.pop


  }


  def main(args: Array[String]) {


    println("START")

    SpinalVhdl({

//
//      val gen = new Generic{
//        val a = 1
//        val b = "asd"
//        val sub = new Bundle{
//          val c = True
//          val d = UInt(3,8 bit)
//        }
//      }
//
//      gen.genNames
//      val genElements = gen.flatten

      val clockC = core.ClockDomain(core.Bool.setName("clkC"), core.Bool.setName("resetC"))
      new TopLevel(clockC)})
    println("DONE")
  }

}

