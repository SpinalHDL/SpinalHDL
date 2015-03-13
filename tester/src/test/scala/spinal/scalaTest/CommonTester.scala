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

package spinal.scalaTest

import spinal.core
import spinal.core._

object CommonTester {


  class BundleA extends Bundle {
    val bod = new Bundle {
      val gggg = Bool()
      val aosi = core.UInt(3 bit)
    }
    val ahe = core.Bool()
    val zwg = core.Bool()
  }

  class BundleAA extends BundleA {
    val vsw = core.Bool()
    val lwee = core.UInt(5 bit)
  }

  class CommonTester extends Component {
    val io = new Bundle {
      val conds = in Vec(8, core.Bool())

      val inUIntA = core.in UInt (8 bit)
      val inUIntB = core.in UInt (8 bit)
      val outUIntAdder = core.out UInt()

      val inAA = core.in(new BundleAA)
      val inAABits = core.in Bits (new BundleAA().getBitsWidth bit)
      val outAA = core.out(new BundleAA)
      val outAABits = core.out Bits (new BundleAA().getBitsWidth bit)


      val assign = new Bundle{
        val sel = core.in Vec(4,core.UInt(4 bit))
        val bitDemux = core.out Bits(16 bit)


        def doit: Unit ={
          bitDemux := Bits(0)
          bitDemux(sel(0)) := conds(0)
          when(conds(1)){
            bitDemux(sel(1)) := conds(2)
          }.elsewhen(conds(3)){
            bitDemux(sel(0)) := conds(4)
          }
          core.when(conds(5)){
            bitDemux(sel(1)) := conds(6)
          }
          bitDemux(5) := core.Bool(true)
        }
      }
      def doit: Unit ={
        assign.doit
      }
    }


    io.doit

    io.outAA.fromBits(io.inAABits)
    io.outAABits := io.inAA.toBits


    val combAdder = new Area {
      val size = io.inUIntA.getWidth
      var out = Vec(size, core.Bool())

      var c = core.Bool(false)
      for (i <- 0 until size) {
        val a = io.inUIntA(i)
        val b = io.inUIntB(i)
        out(i) := a ^ b ^ c
        c = (a & b) | (a & c) | (b & c);
      }
      io.outUIntAdder := out.toBits.toUInt
    }


  }

}

class CommonTesterBoot extends SpinalTesterBase {
  override def getName: String = "CommonTester"

  override def createToplevel: Component = new CommonTester.CommonTester
}
